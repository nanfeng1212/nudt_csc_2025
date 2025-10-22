use super::{
    ir::{
        basicblock::BasicBlock,
        context::{Context, FunctionDecl},
        function::Function,
        global::Global,
        instruction::{
            BinaryOp as IBinaryOp, BitBinaryOp, ConversionOp, FCompCond, ICompCond, Instruction,
            MemAccessOp,
        },
        typ::Typ,
        value::{ConstantValue, Value},
    },
    lalrpop::ast::{
        self, BlockItem, ComptimeValue, ConstDecl, ConstDef, ConstInitVal, Decl, Exp, FuncCall,
        FuncDef, FuncFParam, GlobalItem, InitVal, LVal, Stmt, Type, TypeKind, VarDecl, VarDef,
    },
    symboltable::{SymbolEntry, SymbolTable},
};
use crate::{
    frontend::lalrpop::ast::{BinaryOp as ABinaryOp, CompUnit, ExpKind, UnaryOp as AUnaryOp},
    utils::linked_list::LinkedListContainer,
};

#[derive(Debug, Clone, Copy)]
pub enum IrGenResult {
    Global(Global), //全局变量需要特殊处理和一般的值不同需要通过地址访问
    Value(Value),
}

impl IrGenResult {
    pub fn unwrap(self, ctx: &mut Context) -> Value {
        match self {
            IrGenResult::Value(val) => val,
            IrGenResult::Global(slot) => {
                let name = slot.name(ctx).to_string();
                let value_ty = slot.typ(ctx);
                Value::global_ptr(ctx, name, value_ty)
            }
        }
    }
}

#[derive(Default)]
pub struct IrGenContext {
    pub ctx: Context,
    /// 符号表
    pub symboltable: SymbolTable,

    //当前函数
    pub cur_func: Option<Function>,
    pub cur_func_id: Option<String>,
    pub cur_bbk: Option<BasicBlock>,

    // Stacks for loop control flow.
    pub loop_entry_stack: Vec<BasicBlock>,
    pub loop_exit_stack: Vec<BasicBlock>,

    // Return BasicBlock and slot
    pub cur_ret_slot: Option<Value>,
    pub cur_ret_bbk: Option<BasicBlock>,
}

impl IrGenContext {
    pub fn context(self) -> Context {
        self.ctx
    }

    //将AST中的数据类型转化为IR中的数据类型
    fn ast2ir_type(&mut self, typ: &Type) -> Typ {
        match typ.kind() {
            TypeKind::Void => Typ::void(&mut self.ctx),
            TypeKind::Bool => Typ::bool(&mut self.ctx),
            TypeKind::Int => Typ::int32(&mut self.ctx),
            TypeKind::Float => Typ::float32(&mut self.ctx),
            TypeKind::Ptr(ty) => {
                let ty = self.ast2ir_type(ty);
                Typ::ptr(&mut self.ctx, ty)
            }
            TypeKind::Array(typ, size) => {
                let ir_ty = self.ast2ir_type(typ);
                Typ::array(&mut self.ctx, ir_ty, *size)
            }
            TypeKind::Function(..) => unreachable!("function type should be handled separately"),
        }
    }

    /// AST常量值转化为IR中的常量值
    fn ast2ir_const(&mut self, comptime_const: &ComptimeValue) -> ConstantValue {
        match comptime_const {
            ComptimeValue::Bool(a) => ConstantValue::bool(&mut self.ctx, *a),
            ComptimeValue::Int(a) => ConstantValue::int32(&mut self.ctx, *a),
            ComptimeValue::Float(a) => ConstantValue::float32(&mut self.ctx, *a),
            ComptimeValue::List(a) => {
                let mut elements = Vec::new();
                for e in a {
                    elements.push(self.ast2ir_const(e));
                }

                let typ = comptime_const.get_type();
                let typ = self.ast2ir_type(&typ);

                ConstantValue::array(&mut self.ctx, typ, elements)
            }
            ComptimeValue::Zeros(typ) => {
                let ir_ty = self.ast2ir_type(typ);
                ConstantValue::zero(ir_ty)
            }
            ComptimeValue::Undef(typ) => {
                let ir_ty = self.ast2ir_type(typ);
                ConstantValue::undef(ir_ty)
            }
        }
    }

    /// AST二元表达式，生成表达式，转化为IR中的值
    fn ast2ir_binary(&mut self, exp: &Exp) -> Option<Value> {
        let cur_bbk = self.cur_bbk.unwrap();

        match &exp.kind {
            ExpKind::Binary(op, opd1, opd2) => {
                // 提前计算操作数和类型
                let typ = exp.get_type();
                let typ_of_ir = self.ast2ir_type(&typ);
                let opd1_val = self.ast2ir_exp(opd1)?;
                let opd2_val = self.ast2ir_exp(opd2)?;

                let instruction = match op {
                    // 算术运算统一处理
                    ABinaryOp::Add
                    | ABinaryOp::Sub
                    | ABinaryOp::Mul
                    | ABinaryOp::Div
                    | ABinaryOp::Mod => {
                        let op_kind = match (op, typ.is_float()) {
                            (ABinaryOp::Add, true) => IBinaryOp::Fadd,
                            (ABinaryOp::Add, false) => IBinaryOp::Add,
                            (ABinaryOp::Sub, true) => IBinaryOp::FSub,
                            (ABinaryOp::Sub, false) => IBinaryOp::Sub,
                            (ABinaryOp::Mul, true) => IBinaryOp::Fmul,
                            (ABinaryOp::Mul, false) => IBinaryOp::Mul,
                            (ABinaryOp::Div, true) => IBinaryOp::Fdiv,
                            (ABinaryOp::Div, false) => IBinaryOp::Sdiv,
                            (ABinaryOp::Mod, true) => IBinaryOp::Frem,
                            (ABinaryOp::Mod, false) => IBinaryOp::Srem,
                            _ => unreachable!(),
                        };
                        Instruction::binary(&mut self.ctx, op_kind, typ_of_ir, opd1_val, opd2_val)
                    }
                    // 比较运算统一处理
                    ABinaryOp::Lt
                    | ABinaryOp::Gt
                    | ABinaryOp::Le
                    | ABinaryOp::Ge
                    | ABinaryOp::Eq
                    | ABinaryOp::Ne => {
                        let typ_opd1 = opd1_val.kind(&self.ctx);
                        if typ_opd1.is_float(&self.ctx) {
                            let cond = match op {
                                ABinaryOp::Lt => FCompCond::Ult,
                                ABinaryOp::Gt => FCompCond::Ugt,
                                ABinaryOp::Le => FCompCond::Ule,
                                ABinaryOp::Ge => FCompCond::Uge,
                                ABinaryOp::Eq => FCompCond::Oeq,
                                ABinaryOp::Ne => FCompCond::One,
                                _ => unreachable!(),
                            };
                            Instruction::fcmp(&mut self.ctx, cond, typ_of_ir, opd1_val, opd2_val)
                        } else {
                            let cond = match op {
                                ABinaryOp::Lt => ICompCond::Slt,
                                ABinaryOp::Gt => ICompCond::Sgt,
                                ABinaryOp::Le => ICompCond::Sle,
                                ABinaryOp::Ge => ICompCond::Sge,
                                ABinaryOp::Eq => ICompCond::Eq,
                                ABinaryOp::Ne => ICompCond::Ne,
                                _ => unreachable!(),
                            };
                            Instruction::icmp(&mut self.ctx, cond, typ_of_ir, opd1_val, opd2_val)
                        }
                    }
                    // 逻辑运算
                    ABinaryOp::And | ABinaryOp::Or => {
                        if *typ != Type::bool() {
                            panic!("Invalid type for binary operation(And, Or) in AST");
                        }
                        let op_kind = match op {
                            ABinaryOp::And => BitBinaryOp::And,
                            ABinaryOp::Or => BitBinaryOp::Or,
                            _ => panic!("Invalid binary operation(And, Or) in AST"),
                        };
                        Instruction::bitbinary(
                            &mut self.ctx,
                            op_kind,
                            typ_of_ir,
                            opd1_val,
                            opd2_val,
                        )
                    }
                };

                cur_bbk.push_back(&mut self.ctx, instruction).unwrap();
                instruction.get_result(&self.ctx)
            }
            _ => None,
        }
    }

    /// AST中的二元表达式，生成表达式，转化为IR中的值
    fn ast2ir_unary(&mut self, exp: &Exp) -> Option<Value> {
        let cur_bbk = self.cur_bbk.unwrap();

        match &exp.kind {
            ExpKind::Unary(op, opd1) => {
                let opd1_val = self.ast2ir_exp(opd1)?;
                let typ = exp.get_type();
                let typ_of_ir = self.ast2ir_type(&typ);

                let instruction = match op {
                    AUnaryOp::Neg => {
                        if typ.is_float() {
                            Instruction::fneg(&mut self.ctx, typ_of_ir, opd1_val)
                        } else if typ.is_int() {
                            let izero_const = ConstantValue::int32(&mut self.ctx, 0);
                            let zero = Value::constant(&mut self.ctx, izero_const);
                            Instruction::binary(
                                &mut self.ctx,
                                IBinaryOp::Sub,
                                typ_of_ir,
                                zero,
                                opd1_val,
                            )
                        } else {
                            panic!("Invalid type for unary operation(Neg) in AST");
                        }
                    }
                    AUnaryOp::Not => {
                        if *typ != Type::bool() {
                            panic!("Invalid type for unary operation(Not) in AST");
                        }
                        Instruction::not(&mut self.ctx, typ_of_ir, opd1_val)
                    }
                };

                cur_bbk.push_back(&mut self.ctx, instruction).unwrap();
                instruction.get_result(&self.ctx)
            }
            _ => None,
        }
    }

    /// AST中的表达式转化为IR表达式，并计算结果为IR值返回
    fn ast2ir_exp(&mut self, exp: &Exp) -> Option<Value> {
        let cur_bbk = self.cur_bbk.unwrap();

        match &exp.kind {
            ExpKind::Const(v) => {
                let v = self.ast2ir_const(v);
                Some(Value::constant(&mut self.ctx, v))
            }
            ExpKind::Binary(..) => self.ast2ir_binary(exp),
            ExpKind::Unary(..) => self.ast2ir_unary(exp),
            ExpKind::LVal(lval) => {
                // 提前获取符号表条目和类型
                let entry = self.symboltable.lookup(&lval.id).unwrap();
                let ir_value = entry.ir_value.unwrap();
                let mut typ_of_ir = self.ast2ir_type(&entry.typ.clone());
                let mut slot = ir_value.unwrap(&mut self.ctx);
                // 处理数组访问
                if !lval.dimensions.is_empty() {
                    // 优化索引计算
                    let mut path = Vec::with_capacity(lval.dimensions.len() + 1);
                    if !typ_of_ir.is_ptr(&self.ctx) {
                        let izero_const = ConstantValue::int32(&mut self.ctx, 0);
                        path.push(Value::constant(&mut self.ctx, izero_const));
                    }

                    typ_of_ir = typ_of_ir.get_basetype(&self.ctx).unwrap();
                    for dim in &lval.dimensions {
                        path.push(self.ast2ir_exp(dim)?);
                    }

                    // 构建GEP指令
                    let mut operands = vec![slot];
                    operands.extend(path);
                    let gep = Instruction::memaccess(
                        &mut self.ctx,
                        MemAccessOp::GetElementPtr { typ: typ_of_ir },
                        typ_of_ir,
                        operands,
                    );

                    cur_bbk.push_back(&mut self.ctx, gep).unwrap();
                    slot = gep.get_result(&self.ctx).unwrap();
                }
                if slot.is_parameter(&self.ctx) {
                    Some(slot)
                } else {
                    let load = Instruction::memaccess(
                        &mut self.ctx,
                        MemAccessOp::Load,
                        typ_of_ir,
                        vec![slot],
                    );
                    cur_bbk.push_back(&mut self.ctx, load).unwrap();
                    load.get_result(&self.ctx)
                }
            }
            ExpKind::Coercion(exp_coer) => {
                let src_typ = exp_coer.typ();
                let dst_typ = exp.typ();
                let opd1_val = self.ast2ir_exp(exp_coer)?;

                let i32_type = Typ::int32(&mut self.ctx);
                let f32_type = Typ::float32(&mut self.ctx);
                let instruction = match (src_typ.kind(), dst_typ.kind()) {
                    (TypeKind::Bool, TypeKind::Int) => Instruction::conversion(
                        &mut self.ctx,
                        ConversionOp::ZExt,
                        i32_type,
                        opd1_val,
                    ),
                    (TypeKind::Int, TypeKind::Bool) => {
                        let izero_const = ConstantValue::int32(&mut self.ctx, 0);
                        let zero = Value::constant(&mut self.ctx, izero_const);
                        Instruction::icmp(&mut self.ctx, ICompCond::Ne, i32_type, opd1_val, zero)
                    }
                    (TypeKind::Int, TypeKind::Float) => Instruction::conversion(
                        &mut self.ctx,
                        ConversionOp::SiToFp,
                        f32_type,
                        opd1_val,
                    ),
                    (TypeKind::Float, TypeKind::Bool) => {
                        let fzero_const = ConstantValue::float32(&mut self.ctx, 0.0);
                        let zero = Value::constant(&mut self.ctx, fzero_const);
                        Instruction::fcmp(&mut self.ctx, FCompCond::One, f32_type, opd1_val, zero)
                    }
                    (TypeKind::Float, TypeKind::Int) => Instruction::conversion(
                        &mut self.ctx,
                        ConversionOp::FpToSi,
                        i32_type,
                        opd1_val,
                    ),
                    (TypeKind::Array(_, _), TypeKind::Ptr(dst_typ)) => {
                        let dst_typ = self.ast2ir_type(dst_typ);
                        Instruction::memaccess(
                            &mut self.ctx,
                            MemAccessOp::GetElementPtr { typ: dst_typ },
                            dst_typ,
                            vec![opd1_val],
                        )
                    }
                    _ => panic!("Unsupported coercion: {:?} to {:?}", src_typ, dst_typ),
                };

                cur_bbk.push_back(&mut self.ctx, instruction).unwrap();
                instruction.get_result(&self.ctx)
            }
            ExpKind::FuncCall(call) => {
                // 提前计算参数类型和值
                let params_typ: Vec<&Type> = call.args.iter().map(|arg| arg.get_type()).collect();
                let params_of_ir: Vec<Typ> =
                    params_typ.iter().map(|typ| self.ast2ir_type(typ)).collect();

                // 优化参数处理
                let mut args_of_ir = Vec::with_capacity(call.args.len());
                for (arg, param_type) in call.args.iter().zip(params_of_ir) {
                    args_of_ir.push(self.ast2ir_param(arg, param_type.clone())?);
                }

                let typ_of_ir = self.ast2ir_type(&exp.get_type());
                let instruction =
                    Instruction::call(&mut self.ctx, typ_of_ir, call.id.clone(), args_of_ir);
                cur_bbk.push_back(&mut self.ctx, instruction).unwrap();

                if typ_of_ir.is_void(&self.ctx) {
                    None
                } else {
                    instruction.get_result(&self.ctx)
                }
            }
            ExpKind::InitList(..) => {
                panic!("InitList not implemented here, please look at ast2ir_intlist")
            }
        }
    }

    /// 将AST中的函数参数转化为IR中的函数参数，过程和表达式计算不相同，尤其是在数组和指针的处理上
    fn ast2ir_param(&mut self, exp: &Exp, param_type: Typ) -> Option<Value> {
        let cur_bbk = self.cur_bbk.unwrap();
        match &exp.kind {
            ExpKind::Const(v) => {
                let value_of_ir = self.ast2ir_const(v);
                Some(Value::constant(&mut self.ctx, value_of_ir))
            }
            ExpKind::Binary(..) => self.ast2ir_binary(exp),
            ExpKind::Unary(..) => self.ast2ir_unary(exp),
            ExpKind::LVal(LVal { id, dimensions }) => {
                //在符号表中查找变量的IR值
                let entry = self.symboltable.lookup(id).unwrap();
                let ir_value = entry.ir_value.unwrap();
                //获取变量类型
                let depth_of_entry = &entry.typ.get_array_depth();
                let depth_of_exp = &exp.get_type().get_array_depth();
                let mut typ_of_ir = self.ast2ir_type(&entry.typ.clone());
                let exp_typ = self.ast2ir_type(&exp.get_type());

                if (exp_typ.is_array(&self.ctx) && depth_of_entry == depth_of_exp)
                    || exp_typ.is_ptr(&self.ctx)
                {
                    Some(ir_value.unwrap(&mut self.ctx))
                } else {
                    let mut slot = ir_value.unwrap(&mut self.ctx);
                    if !dimensions.is_empty() {
                        let mut path: Vec<Value> = vec![];
                        if typ_of_ir.is_array(&self.ctx) {
                            // 主要区分数组和指针
                            let zero = ConstantValue::int32(&mut self.ctx, 0);
                            let zero = Value::constant(&mut self.ctx, zero);
                            if !dimensions.is_empty() {
                                path.push(zero);
                            }
                        }
                        // 此处不能直接获取基类型要根据需要的dimensiaons深度获取
                        for dim in dimensions {
                            path.push(self.ast2ir_exp(dim).unwrap());
                            (typ_of_ir, _) = typ_of_ir.as_array(&self.ctx).unwrap();
                        }

                        let mut operands = vec![slot];
                        operands.extend(path);
                        let gep = Instruction::memaccess(
                            &mut self.ctx,
                            MemAccessOp::GetElementPtr { typ: typ_of_ir },
                            typ_of_ir,
                            operands,
                        );

                        cur_bbk.push_back(&mut self.ctx, gep).unwrap();
                        slot = gep
                            .get_result(&self.ctx)
                            .expect("Failed to unwrap the result of the lval in AST");
                    }
                    if slot.is_parameter(&self.ctx) || param_type.is_ptr(&self.ctx) {
                        //如果时函数参数直接返回slot，因为此时的值已经在栈帧中
                        Some(slot)
                    } else {
                        //不是函数参数则可能是临时变量或者全局变量，需要从内存中加载出来
                        let instruction = Instruction::memaccess(
                            &mut self.ctx,
                            MemAccessOp::Load,
                            typ_of_ir,
                            vec![slot],
                        ); //

                        cur_bbk.push_back(&mut self.ctx, instruction).unwrap();
                        Some(
                            instruction
                                .get_result(&self.ctx)
                                .expect("Failed to unwrap the result of the lval in AST"),
                        )
                    }
                }
            }
            ExpKind::Coercion(exp_of_coer) => {
                // println!(
                //     "IRGEN: Coercion from {:?} to {:?} ",
                //     exp_of_coer.typ(),
                //     exp.typ()
                // );
                let instruction = match (exp_of_coer.typ().kind(), exp.typ().kind()) {
                    (TypeKind::Bool, TypeKind::Int) => {
                        let opd1 = self.ast2ir_exp(exp_of_coer).unwrap();
                        let dst_type = Typ::int32(&mut self.ctx);
                        Instruction::conversion(&mut self.ctx, ConversionOp::ZExt, dst_type, opd1)
                    }
                    (TypeKind::Int, TypeKind::Bool) => {
                        let opd1 = self.ast2ir_exp(exp_of_coer).unwrap();
                        let src_type = Typ::int32(&mut self.ctx);
                        let zero = ConstantValue::int32(&mut self.ctx, 0);
                        let zero = Value::constant(&mut self.ctx, zero);
                        Instruction::icmp(&mut self.ctx, ICompCond::Ne, src_type, opd1, zero)
                    }
                    (TypeKind::Int, TypeKind::Float) => {
                        let opd1 = self.ast2ir_exp(exp_of_coer).unwrap();
                        let dst_type = Typ::float32(&mut self.ctx);
                        Instruction::conversion(&mut self.ctx, ConversionOp::SiToFp, dst_type, opd1)
                    }
                    (TypeKind::Float, TypeKind::Bool) => {
                        let opd1 = self.ast2ir_exp(exp_of_coer).unwrap();
                        let src_type = Typ::float32(&mut self.ctx);
                        let zero = ConstantValue::float32(&mut self.ctx, 0.0);
                        let zero = Value::constant(&mut self.ctx, zero);
                        Instruction::fcmp(&mut self.ctx, FCompCond::One, src_type, opd1, zero)
                    }
                    (TypeKind::Float, TypeKind::Int) => {
                        let opd1 = self.ast2ir_exp(exp_of_coer).unwrap();
                        let dst_type = Typ::int32(&mut self.ctx);
                        Instruction::conversion(&mut self.ctx, ConversionOp::FpToSi, dst_type, opd1)
                    }
                    (TypeKind::Array(_src_typ, _size), TypeKind::Ptr(dst_typ)) => {
                        let opd1 = self.ast2ir_param(exp_of_coer, param_type).expect(
                            "Failed to unwrap in ast2ir_exp coercion(OF array to ptr in AST)",
                        );
                        let dst_ir_typ = self.ast2ir_type(dst_typ);
                        let dst_ir_typ = Typ::ptr(&mut self.ctx, dst_ir_typ);
                        let mut operands = vec![opd1];
                        // println!(
                        //     "IRGEN: Coercion from array to ptr, dst_typ: {:?}, dst_ir_typ: {}",
                        //     dst_typ,
                        //     dst_ir_typ.display(&self.ctx)
                        // );
                        let depth = exp_of_coer.get_type().get_array_depth();
                        let depth = (depth - dst_typ.get_array_depth()) as usize;
                        let zero = ConstantValue::int32(&mut self.ctx, 0);
                        let zero = Value::constant(&mut self.ctx, zero);
                        operands.push(zero);
                        for _ in 0..depth {
                            operands.push(zero.clone());
                        }
                        Instruction::memaccess(
                            &mut self.ctx,
                            MemAccessOp::GetElementPtr { typ: dst_ir_typ },
                            dst_ir_typ,
                            operands,
                        )
                    }
                    _ => {
                        panic!(
                            "Failed to coerce the type of the expression[Unsupported coercion]{:?}",
                            exp
                        )
                    }
                };
                cur_bbk.push_back(&mut self.ctx, instruction).unwrap();
                Some(
                    instruction.get_result(&self.ctx).expect(
                        "Failed to unwrap the result of the coercion(OF bool to int in AST)",
                    ),
                )
            }
            ExpKind::FuncCall(FuncCall { id, args }) => {
                let params_typ = args
                    .iter()
                    .map(|arg| arg.get_type())
                    .cloned()
                    .collect::<Vec<ast::Type>>();
                let params_of_ir = params_typ
                    .iter()
                    .map(|typ| self.ast2ir_type(typ))
                    .collect::<Vec<Typ>>();
                let mut args_of_ir = Vec::new();
                for (arg, param_type) in args.iter().zip(params_of_ir.iter()) {
                    let arg_value = self
                        .ast2ir_param(arg, param_type.clone())
                        .expect("Failed to unwrap the argument value in AST");
                    args_of_ir.push(arg_value);
                }
                let typ_of_ir = self.ast2ir_type(&exp.get_type());
                let instructon =
                    Instruction::call(&mut self.ctx, typ_of_ir, id.clone(), args_of_ir);
                cur_bbk.push_back(&mut self.ctx, instructon).unwrap();
                if typ_of_ir.is_void(&self.ctx) {
                    None
                } else {
                    Some(
                        instructon
                            .get_result(&self.ctx)
                            .expect("Failed to unwrap the result of the function call in AST"),
                    )
                }
            }
            ExpKind::InitList(_exps) => {
                todo!("InitList not implemented yet")
            }
        }
    }

    fn ast2ir_initlist(&mut self, initlist: &Exp) -> Vec<(Value, Vec<Value>)> {
        let zero = ConstantValue::int32(&mut self.ctx, 0);
        let zero = Value::constant(&mut self.ctx, zero);
        self.initlist_helper(initlist, &vec![zero])
    }

    fn initlist_helper(
        &mut self,
        initlist: &Exp,
        current_path: &[Value],
    ) -> Vec<(Value, Vec<Value>)> {
        // println!("initlist_helper initlist: [{:?}]", initlist);
        let mut result = vec![];
        match &initlist.kind {
            ExpKind::InitList(exps) => {
                for (i, exp) in exps.iter().enumerate() {
                    let mut new_path = current_path.to_vec();
                    let index = ConstantValue::int32(&mut self.ctx, i as i32);
                    new_path.push(Value::constant(&mut self.ctx, index));
                    match &exp.kind {
                        ExpKind::InitList(_) => {
                            result.extend(self.initlist_helper(exp, &new_path));
                        }
                        ExpKind::Const(exps) => {
                            result.extend(self.const_initlist_helper(exps, &new_path));
                        }
                        _ => {
                            let value = self.ast2ir_exp(exp).unwrap();
                            result.push((value, new_path));
                        }
                    }
                }
            }
            ExpKind::Const(exps) => result.extend(self.const_initlist_helper(exps, current_path)),
            _ => {
                let value = self.ast2ir_exp(initlist).unwrap();
                result.push((value, current_path.to_vec()));
            }
        }
        result
    }

    fn const_initlist_helper(
        &mut self,
        initlist: &ComptimeValue,
        current_path: &[Value],
    ) -> Vec<(Value, Vec<Value>)> {
        let mut result = vec![];
        match initlist {
            ComptimeValue::List(list) => {
                for (i, exp) in list.iter().enumerate() {
                    let mut new_path = current_path.to_vec();
                    let index = ConstantValue::int32(&mut self.ctx, i as i32);
                    new_path.push(Value::constant(&mut self.ctx, index));
                    match exp {
                        ComptimeValue::List(_) => {
                            result.extend(self.const_initlist_helper(exp, &new_path));
                        }
                        _ => {
                            let v = self.ast2ir_const(exp);
                            let value = Value::constant(&mut self.ctx, v);
                            result.push((value, new_path));
                        }
                    }
                }
            }
            ComptimeValue::Zeros(typ) => {
                // println!("ComptimeValue::Zeros typ [{:?}]", typ);
                if typ.is_array() {
                    let dimensions = typ.array_dims();
                    let total_size = dimensions.iter().product::<usize>();
                    let mut size_list = vec![];
                    let mut cur = 1;
                    for i in 0..dimensions.len() {
                        size_list.push(cur);
                        cur *= dimensions[dimensions.len() - 1 - i];
                    }
                    size_list.reverse();
                    let base_typ = self.ast2ir_type(typ.array_base());
                    let zero = Value::constzero(&mut self.ctx, base_typ);
                    for i in 0..total_size {
                        let mut new_path = current_path.to_vec();
                        let mut cur = i;
                        for j in 0..dimensions.len() {
                            let index = cur / size_list[j];
                            cur %= size_list[j];
                            let index = ConstantValue::int32(&mut self.ctx, index as i32);
                            new_path.push(Value::constant(&mut self.ctx, index));
                        }
                        result.push((zero.clone(), new_path.clone()));
                    }
                }
            }
            _ => {
                let v = self.ast2ir_const(initlist);
                let value = Value::constant(&mut self.ctx, v);
                result.push((value, current_path.to_vec()));
            }
        }
        result
    }

    fn const_initlist2value(&mut self, initlist: ConstantValue) -> Vec<(Value, Vec<Value>)> {
        let zero = ConstantValue::int32(&mut self.ctx, 0);
        let zero = Value::constant(&mut self.ctx, zero);
        self.const_initlist2value_helper(initlist, &[zero])
    }

    fn const_initlist2value_helper(
        &mut self,
        initlist: ConstantValue,
        current_path: &[Value],
    ) -> Vec<(Value, Vec<Value>)> {
        let mut result = vec![];
        match initlist {
            ConstantValue::Array { elements, .. } => {
                for (i, elem) in elements.iter().enumerate() {
                    let mut new_path = current_path.to_vec();
                    // 生成数组索引常量值
                    let index = ConstantValue::int32(&mut self.ctx, i as i32);
                    new_path.push(Value::constant(&mut self.ctx, index));

                    // 递归处理嵌套数组
                    result.extend(self.const_initlist2value_helper(elem.clone(), &new_path));
                }
            }
            ConstantValue::Zero { typ } | ConstantValue::Undef { typ } => {
                if typ.is_array(&self.ctx) {
                    let dimensions = typ.get_array_indices(&self.ctx).unwrap();
                    let total_size = dimensions.iter().product::<usize>();
                    let mut size_list = vec![];
                    let mut cur = 1;
                    for i in 0..dimensions.len() {
                        size_list.push(cur);
                        cur *= dimensions[dimensions.len() - 1 - i];
                    }
                    size_list.reverse();
                    let base_typ = typ.get_array_basetype(&self.ctx);
                    let zero = Value::constzero(&mut self.ctx, base_typ.unwrap());
                    for i in 0..total_size {
                        let mut new_path = current_path.to_vec();
                        let mut cur = i;
                        for j in 0..dimensions.len() {
                            let index = cur / size_list[j];
                            cur %= size_list[j];
                            let index = ConstantValue::int32(&mut self.ctx, index as i32);
                            new_path.push(Value::constant(&mut self.ctx, index));
                        }
                        result.push((zero.clone(), new_path.clone()));
                    }
                }
            }
            // 处理叶子节点（非数组类型）
            _ => {
                // 假设存在将ConstantValue转换为IR Value的方法
                let value = Value::constant(&mut self.ctx, initlist);
                result.push((value, current_path.to_vec()));
            }
        }
        result
    }

    fn ast2ir_cond(
        &mut self,
        cond: &Exp,
        true_bbk: BasicBlock,
        false_bbk: BasicBlock,
    ) -> Result<(), String> {
        let terminated = self.cur_bbk.map_or(true, |bb| bb.is_terminated(&self.ctx));
        if terminated {
            return Ok(());
        }
        match &cond.kind {
            ExpKind::Binary(ABinaryOp::And, opd1, opd2) => {
                let and_true_bbk = BasicBlock::new(&mut self.ctx);
                self.cur_func
                    .unwrap()
                    .push_back(&mut self.ctx, and_true_bbk)?;
                self.ast2ir_cond(opd1, and_true_bbk, false_bbk)?;
                self.cur_bbk = Some(and_true_bbk);
                return self.ast2ir_cond(opd2, true_bbk, false_bbk);
            }
            ExpKind::Binary(ABinaryOp::Or, opd1, opd2) => {
                let or_false_bbk = BasicBlock::new(&mut self.ctx);
                self.cur_func
                    .unwrap()
                    .push_back(&mut self.ctx, or_false_bbk)?;
                self.ast2ir_cond(opd1, true_bbk, or_false_bbk)?;
                self.cur_bbk = Some(or_false_bbk);
                return self.ast2ir_cond(opd2, true_bbk, false_bbk);
            }
            ExpKind::Unary(AUnaryOp::Not, opd) => {
                return self.ast2ir_cond(opd, false_bbk, true_bbk);
            }
            _ => {
                let cond = self.ast2ir_exp(cond).unwrap();
                let icbr = Instruction::icbr(&mut self.ctx, cond, true_bbk, false_bbk);
                let res = self.cur_bbk.unwrap().push_back(&mut self.ctx, icbr);
                match res {
                    Ok(_) => Ok(()),
                    Err(_) => Err(
                        "Failed to push back the ICBR instruction in the current basic block"
                            .to_string(),
                    ),
                }
            }
        }
    }

    // Generate the system library function definitions.
    fn gen_sysylib(&mut self) {
        // Since the system library is linked in the linking phase, we just need
        // to generate declarations here.
        self.symboltable.register_sysylib();
        // Generate declarations for the system library functions
        let sysylib_names = vec![
            "getint".to_string(),
            "getch".to_string(),
            "getfloat".to_string(),
            "getarray".to_string(),
            "getfarray".to_string(),
            "putint".to_string(),
            "putch".to_string(),
            "putfloat".to_string(),
            "putarray".to_string(),
            "putfarray".to_string(),
            "_sysy_starttime".to_string(),
            "_sysy_stoptime".to_string(),
            // "nudt_memset_int".to_string(),
            // "nudt_memset_float".to_string(),
        ];
        for name in sysylib_names {
            let (params_typ, ret_typ) = self.symboltable.lookup(&name).unwrap().typ.unwrap_func();
            let ir_params_typ = params_typ
                .iter()
                .map(|typ| self.ast2ir_type(typ))
                .collect::<Vec<Typ>>();
            let ir_ret_typ = self.ast2ir_type(&ret_typ);
            let decl = FunctionDecl {
                name: name,
                parameters_typ: ir_params_typ,
                return_type: ir_ret_typ,
            };
            self.ctx.add_funcdecl(decl);
        }
    }

    fn bbk_end_with_terminator(&mut self) -> bool {
        let cur_bbk = self.cur_bbk.as_ref().unwrap();
        // Check if the current basic block ends with a terminator instruction
        cur_bbk.is_terminated(&self.ctx)
    }
}

pub trait IrGen {
    fn irgen(&self, irgen: &mut IrGenContext);
}

impl IrGen for CompUnit {
    ///为Sysy顶层的编译单元生成IR
    fn irgen(&self, irgen: &mut IrGenContext) {
        //全局作用域
        irgen.symboltable.enter_scope();

        // 链接库声明
        irgen.gen_sysylib();

        for item in &self.items {
            item.irgen(irgen);
        }
        irgen.symboltable.leave_scope();
    }
}

impl IrGen for GlobalItem {
    // Generate IR for an item.
    fn irgen(&self, irgen: &mut IrGenContext) {
        match self {
            GlobalItem::Decl(decl) => match decl {
                Decl::ConstDecl(ConstDecl { defs, .. }) => {
                    for ConstDef { id, init, .. } in defs {
                        // 常量定义一定采用常量表达式初始化，可以直接进行折叠
                        let comptime = match init {
                            ConstInitVal::Exp(exp) => exp
                                .try_fold(&irgen.symboltable)
                                .expect("global def expected to have constant initializer"),
                            ConstInitVal::List(list) => list
                                .try_fold(&irgen.symboltable)
                                .expect("global def expected to have constant initializer"),
                        };
                        let constant = irgen.ast2ir_const(&comptime);
                        let slot = Global::new(
                            &mut irgen.ctx,
                            format!("NUDT_GLOBAL_CONST_{}", id),
                            constant,
                        );

                        irgen.symboltable.insert(
                            id.clone(),
                            SymbolEntry {
                                typ: match init {
                                    ConstInitVal::Exp(exp) => exp.typ().clone(),
                                    ConstInitVal::List(list) => list.typ().clone(),
                                },
                                comptime: Some(comptime),
                                ir_value: Some(IrGenResult::Global(slot)),
                            },
                        );
                    }
                }
                Decl::VarDecl(VarDecl { defs, .. }) => {
                    for VarDef { id, init, .. } in defs {
                        // 未给出初始化值的变量声明，默认初始化为0
                        let init = init.as_ref().unwrap();
                        let comptime = match init {
                            InitVal::Exp(exp) => exp
                                .try_fold(&irgen.symboltable)
                                .expect("global def expected to have constant initializer"),
                            InitVal::List(list) => list
                                .try_fold(&irgen.symboltable)
                                .expect("global def expected to have constant initializer"),
                        };
                        let constant = irgen.ast2ir_const(&comptime);
                        let slot = Global::new(
                            &mut irgen.ctx,
                            format!("NUDT_GLOBAL_VAR_{}", id),
                            constant,
                        );

                        irgen.symboltable.insert(
                            id.clone(),
                            SymbolEntry {
                                typ: match init {
                                    InitVal::Exp(exp) => exp.typ().clone(),
                                    InitVal::List(list) => list.typ().clone(),
                                },
                                comptime: Some(comptime),
                                ir_value: Some(IrGenResult::Global(slot)),
                            },
                        );
                    }
                }
            },
            GlobalItem::FuncDef(func_def) => func_def.irgen(irgen),
        }
    }
}

impl IrGen for FuncDef {
    fn irgen(&self, irgen: &mut IrGenContext) {
        // 进入函数当前的作用域
        irgen.symboltable.enter_scope();

        let mut param_tys = Vec::new();
        for FuncFParam {
            typ, dimensions, ..
        } in self.params.iter()
        {
            let mut typ = typ.clone();
            // 处理数组参数类型，需要区分数组和标量，标量是传值，数组是在传递地址
            // 数组的最外围可以不用给出
            if !dimensions.is_none() {
                for dim in dimensions.clone().unwrap().iter_mut().rev() {
                    typ = Type::array(
                        typ.clone(),
                        dim.try_fold(&irgen.symboltable)
                            .expect("dimension expected to be constant")
                            .unwrap2int() as usize,
                    );
                }
                typ = Type::ptr(typ.clone());
            }
            param_tys.push(typ.clone());
        }
        let func_ty = Type::function(param_tys.clone(), self.typ.clone());
        irgen.symboltable.insert_upper(
            self.id.clone(),
            SymbolEntry {
                typ: func_ty,
                comptime: None,
                ir_value: None,
            },
            1,
        );

        let ir_ret_ty = irgen.ast2ir_type(&self.typ);
        let function = Function::new(&mut irgen.ctx, self.id.clone(), ir_ret_ty);
        let bbk = BasicBlock::new(&mut irgen.ctx);
        function.push_back(&mut irgen.ctx, bbk).unwrap();

        irgen.cur_func = Some(function);
        irgen.cur_func_id = Some(self.id.clone());
        irgen.cur_bbk = Some(bbk);

        for FuncFParam {
            typ,
            dimensions,
            id,
        } in self.params.iter()
        {
            let mut typ = typ.clone();
            if !dimensions.is_none() {
                // let mut typ = typ.clone();
                for dim in dimensions.clone().unwrap().iter_mut().rev() {
                    typ = Type::array(
                        typ.clone(),
                        dim.try_fold(&irgen.symboltable)
                            .expect("dimension expected to be constant")
                            .unwrap2int() as usize,
                    );
                }
                typ = Type::ptr(typ.clone());
            }
            let ir_ty = irgen.ast2ir_type(&typ);
            let param = function.add_parameter(&mut irgen.ctx, ir_ty);

            irgen.symboltable.insert(
                id.clone(),
                SymbolEntry {
                    typ: typ.clone(),
                    comptime: None,
                    ir_value: Some(IrGenResult::Value(param)),
                },
            );
        }

        // 为传值的的参数创建alloca空间
        for (FuncFParam { id, .. }, typ) in self.params.iter().zip(param_tys.iter()) {
            if !typ.is_ptr() {
                let ir_typ = irgen.ast2ir_type(typ);
                let slot = Instruction::memaccess(
                    &mut irgen.ctx,
                    MemAccessOp::Alloca { typ: ir_typ },
                    ir_typ,
                    vec![],
                );

                bbk.push_front(&mut irgen.ctx, slot).unwrap();
                let slot = slot.get_result(&irgen.ctx).unwrap();

                // get old entry
                let param = irgen
                    .symboltable
                    .lookup(id)
                    .unwrap()
                    .ir_value
                    .unwrap()
                    .unwrap(&mut irgen.ctx);

                // store
                let store = Instruction::memaccess(
                    &mut irgen.ctx,
                    MemAccessOp::Store,
                    ir_typ,
                    vec![param, slot],
                );

                bbk.push_back(&mut irgen.ctx, store).unwrap();

                // set new entry
                irgen.symboltable.insert(
                    id.clone(),
                    SymbolEntry {
                        typ: typ.clone(),
                        comptime: None,
                        ir_value: Some(IrGenResult::Value(slot)),
                    },
                );
            }
        }

        //创建返回基本块和返回值槽
        let ret_block = BasicBlock::new(&mut irgen.ctx);
        irgen.cur_ret_bbk = Some(ret_block);

        // 为返回值创建alloca空间
        if !self.typ.is_void() {
            let ir_ret_ty = irgen.ast2ir_type(&self.typ);
            let ret_slot = Instruction::memaccess(
                &mut irgen.ctx,
                MemAccessOp::Alloca { typ: ir_ret_ty },
                ir_ret_ty,
                vec![],
            );

            bbk.push_front(&mut irgen.ctx, ret_slot).unwrap();
            irgen.cur_ret_slot = Some(ret_slot.get_result(&irgen.ctx).unwrap());
        }

        // 函数体生成
        self.block.irgen(irgen);

        // 为函数添加返回基本块（保证每个函数的出口唯一）
        function.push_back(&mut irgen.ctx, ret_block).unwrap();

        if !irgen.bbk_end_with_terminator() {
            // 有可能函数在之前就已经有ret指令
            let br2ret = Instruction::br(&mut irgen.ctx, ret_block);
            irgen
                .cur_bbk
                .unwrap()
                .push_back(&mut irgen.ctx, br2ret)
                .unwrap();
        }
        if !self.typ.is_void() {
            //有返回值
            // load, ret
            let ret_slot = irgen.cur_ret_slot.unwrap();
            let typ = irgen.ast2ir_type(&self.typ);
            let load =
                Instruction::memaccess(&mut irgen.ctx, MemAccessOp::Load, typ, vec![ret_slot]);

            ret_block.push_back(&mut irgen.ctx, load).unwrap();
            let val = load.get_result(&irgen.ctx).unwrap();

            let ret = Instruction::ret(&mut irgen.ctx, Some(val));

            ret_block.push_back(&mut irgen.ctx, ret).unwrap();
        } else {
            //没有返回值
            // just return
            let ret = Instruction::ret(&mut irgen.ctx, None);
            ret_block.push_back(&mut irgen.ctx, ret).unwrap();
        }

        irgen.cur_func = None;
        irgen.cur_func_id = None;
        irgen.cur_bbk = None;
        irgen.cur_ret_slot = None;
        irgen.cur_ret_bbk = None;
        // 离开函数作用域
        irgen.symboltable.leave_scope();
    }
}

impl IrGen for Decl {
    fn irgen(&self, irgen: &mut IrGenContext) {
        let entry_block = irgen.cur_func.unwrap().head(&irgen.ctx).unwrap(); //aloca指令都放到入口块中
        match self {
            Decl::ConstDecl(ConstDecl { defs, .. }) => {
                for ConstDef { id, init, .. } in defs {
                    let comptime = match init {
                        ConstInitVal::Exp(exp) => exp
                            .try_fold(&irgen.symboltable)
                            .expect("constant initializer expected"),
                        ConstInitVal::List(list) => list
                            .try_fold(&irgen.symboltable)
                            .expect("constant initializer expected"),
                    };

                    // 统一处理初始化表达式类型
                    let (init_val, init_typ) = match init {
                        ConstInitVal::Exp(exp) => (exp, exp.typ()),
                        ConstInitVal::List(list) => (list, list.typ()),
                    };

                    let ir_ty = irgen.ast2ir_type(init_typ);

                    // 创建常量内存空间
                    let const_slot = Instruction::memaccess(
                        &mut irgen.ctx,
                        MemAccessOp::Alloca { typ: ir_ty },
                        ir_ty,
                        vec![],
                    );
                    entry_block.push_front(&mut irgen.ctx, const_slot).unwrap();

                    // 符号表插入
                    irgen.symboltable.insert(
                        id,
                        SymbolEntry {
                            typ: init_typ.clone(),
                            comptime: Some(comptime),
                            ir_value: Some(IrGenResult::Value(
                                const_slot.get_result(&irgen.ctx).unwrap(),
                            )),
                        },
                    );

                    // 处理数组初始化
                    let slot = const_slot.get_result(&irgen.ctx).unwrap();
                    Self::init_decl(irgen, init_val, ir_ty, slot);
                }
            }
            Decl::VarDecl(VarDecl { defs, .. }) => {
                for VarDef { id, init, .. } in defs {
                    let init = init.as_ref().unwrap();

                    let init_val = match init {
                        InitVal::Exp(exp) => exp,
                        InitVal::List(list) => list,
                    };
                    let ir_ty = irgen.ast2ir_type(init_val.typ());

                    let stack_slot = Instruction::memaccess(
                        &mut irgen.ctx,
                        MemAccessOp::Alloca { typ: ir_ty },
                        ir_ty,
                        vec![],
                    );
                    entry_block.push_front(&mut irgen.ctx, stack_slot).unwrap();

                    irgen.symboltable.insert(
                        id,
                        SymbolEntry {
                            typ: init_val.typ().clone(),
                            comptime: None,
                            ir_value: Some(IrGenResult::Value(
                                stack_slot.get_result(&irgen.ctx).unwrap(),
                            )),
                        },
                    );

                    let slot = stack_slot.get_result(&irgen.ctx).unwrap();
                    Self::init_decl(irgen, init_val, ir_ty, slot);
                }
            }
        }
    }
}

impl Decl {
    /// 常量和变量声明的初始化
    fn init_decl(irgen: &mut IrGenContext, init_val: &Exp, ir_ty: Typ, slot: Value) {
        let cur_block = irgen.cur_bbk.unwrap();
        if ir_ty.is_array(&irgen.ctx) {
            match &init_val.kind {
                ExpKind::InitList(_list) => {
                    let init_list = irgen.ast2ir_initlist(init_val);
                    Self::memset_initlist(ir_ty, init_list, slot, irgen);
                }
                ExpKind::Const(const_exp) => {
                    let init = irgen.ast2ir_const(const_exp);
                    match &init {
                        ConstantValue::Array { .. } => {
                            if init.is_all_zero() {
                                let size = ir_ty.bytewidth(&irgen.ctx) / 4;
                                let base_typ = ir_ty.get_array_basetype(&irgen.ctx).unwrap();
                                let size = ConstantValue::int32(&mut irgen.ctx, size as i32);
                                let size = Value::constant(&mut irgen.ctx, size);
                                Self::memset_zero(base_typ, slot, size, irgen);
                            } else {
                                let init_list = irgen.const_initlist2value(init);
                                Self::memset_initlist(ir_ty, init_list, slot, irgen);
                            }
                        }
                        ConstantValue::Undef { .. } => {}
                        ConstantValue::Zero { .. } => {
                            let size = ir_ty.bytewidth(&irgen.ctx) / 4;
                            let typ = ir_ty.get_array_basetype(&irgen.ctx).unwrap();
                            let size = ConstantValue::int32(&mut irgen.ctx, size as i32);
                            let size = Value::constant(&mut irgen.ctx, size);
                            Self::memset_zero(typ, slot, size, irgen);
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        } else {
            // 处理标量常量
            if matches!(init_val.kind, ExpKind::Const(ComptimeValue::Undef { .. })) {
                return;
            }
            let init_value = irgen.ast2ir_exp(init_val).unwrap();
            let store = Instruction::memaccess(
                &mut irgen.ctx,
                MemAccessOp::Store,
                ir_ty,
                vec![init_value, slot],
            );
            cur_block.push_back(&mut irgen.ctx, store).unwrap();
        }
    }

    /// 所有局部数组都会memeset 0只有非零值会store，这样从规模大的数组来看应该是好的？？？
    fn memset_initlist(
        ir_ty: Typ,
        init_list: Vec<(Value, Vec<Value>)>,
        slot: Value,
        irgen: &mut IrGenContext,
    ) {
        // 获取基础元素类型
        let elem_ty = ir_ty.get_array_basetype(&irgen.ctx).unwrap();
        let size = ir_ty.bytewidth(&irgen.ctx) / 4;
        let size = ConstantValue::int32(&mut irgen.ctx, size as i32);
        let size = Value::constant(&mut irgen.ctx, size);
        Self::memset_zero(elem_ty, slot, size, irgen);

        let cur_block = irgen.cur_bbk.unwrap();
        // 获取数组基址
        let mut operands = vec![slot];
        operands.extend(init_list.iter().next().unwrap().1.iter().cloned());
        let gep = Instruction::memaccess(
            &mut irgen.ctx,
            MemAccessOp::GetElementPtr { typ: elem_ty },
            elem_ty,
            operands,
        );
        cur_block.push_back(&mut irgen.ctx, gep).unwrap();
        let base_slot = gep.get_result(&irgen.ctx).unwrap();
        for i in 0..init_list.len() {
            if !init_list[i].0.is_zero(&irgen.ctx) {
                let offset = ConstantValue::int32(&mut irgen.ctx, i as i32);
                let offset = Value::constant(&mut irgen.ctx, offset);
                let gep = Instruction::memaccess(
                    &mut irgen.ctx,
                    MemAccessOp::GetElementPtr { typ: elem_ty },
                    elem_ty,
                    vec![base_slot, offset],
                );
                cur_block.push_back(&mut irgen.ctx, gep).unwrap();
                let elem_slot = gep.get_result(&irgen.ctx).unwrap();
                let store = Instruction::memaccess(
                    &mut irgen.ctx,
                    MemAccessOp::Store,
                    elem_ty,
                    vec![init_list[i].0, elem_slot],
                );
                cur_block.push_back(&mut irgen.ctx, store).unwrap();
            }
        }
    }

    /// memset 0，将指定槽的之后的指定大小初始化为0
    fn memset_zero(typ: Typ, slot: Value, size: Value, irgen: &mut IrGenContext) {
        let cur_block = irgen.cur_bbk.unwrap();
        let void_typ = Typ::void(&mut irgen.ctx);
        let zero = Value::constzero(&mut irgen.ctx, typ);
        let mut name = "nudt_memset".to_string();
        if typ.is_float(&irgen.ctx) {
            name += "_float"
        } else {
            name += "_int"
        }
        let typ_ptr = Typ::ptr(&mut irgen.ctx, typ.clone());
        let bitcast = Instruction::conversion(&mut irgen.ctx, ConversionOp::Bitcast, typ_ptr, slot);
        let slot = bitcast.get_result(&irgen.ctx).unwrap();
        cur_block.push_back(&mut irgen.ctx, bitcast).unwrap();
        let memset = Instruction::call(&mut irgen.ctx, void_typ, name, vec![slot, zero, size]);
        cur_block.push_back(&mut irgen.ctx, memset).unwrap();
    }
}

impl IrGen for Stmt {
    fn irgen(&self, irgen: &mut IrGenContext) {
        let cur_block = irgen.cur_bbk.unwrap();

        match self {
            Stmt::Assign(ast::Assign {
                lval: ast::LVal { id, dimensions },
                exp,
            }) => {
                let entry = irgen.symboltable.lookup(&id).unwrap();
                let ir_value = entry.ir_value.unwrap();
                let mut slot = ir_value.unwrap(&mut irgen.ctx);

                let mut cur_typ = slot.kind(&irgen.ctx);
                if cur_typ.is_ptr(&irgen.ctx) || cur_typ.is_array(&irgen.ctx) {
                    let mut operands = vec![slot];
                    if !cur_typ.is_ptr(&irgen.ctx) {
                        let zero = ConstantValue::int32(&mut irgen.ctx, 0);
                        let zero = Value::constant(&mut irgen.ctx, zero);
                        operands.push(zero);
                    }
                    for dim in dimensions {
                        let (elem_typ, _size) = cur_typ.as_array(&irgen.ctx).unwrap();
                        cur_typ = elem_typ;
                        let index = irgen.ast2ir_exp(dim).unwrap();
                        operands.push(index);
                    }
                    let gep = Instruction::memaccess(
                        &mut irgen.ctx,
                        MemAccessOp::GetElementPtr { typ: cur_typ },
                        cur_typ,
                        operands,
                    );

                    cur_block.push_back(&mut irgen.ctx, gep).unwrap();
                    slot = gep.get_result(&irgen.ctx).unwrap();
                }

                let val = irgen.ast2ir_exp(&exp).unwrap();
                let typ_of_ir = irgen.ast2ir_type(&exp.typ());
                let store = Instruction::memaccess(
                    &mut irgen.ctx,
                    MemAccessOp::Store,
                    typ_of_ir,
                    vec![val, slot],
                );

                cur_block.push_back(&mut irgen.ctx, store).unwrap();
            }
            Stmt::ExpStmt(ast::ExpStmt { exp }) => {
                if let Some(exp) = exp {
                    irgen.ast2ir_exp(exp);
                }
            }
            Stmt::Block(bbk) => bbk.irgen(irgen),
            Stmt::If(box_if) => {
                let cond = &box_if.cond;
                let then = &box_if.then;
                let else_then = &box_if.else_then;

                let br_entry = BasicBlock::new(&mut irgen.ctx);
                irgen
                    .cur_func
                    .unwrap()
                    .push_back(&mut irgen.ctx, br_entry)
                    .unwrap();
                if !irgen.cur_bbk.unwrap().is_terminated(&irgen.ctx) {
                    let br2entry = Instruction::br(&mut irgen.ctx, br_entry);
                    irgen
                        .cur_bbk
                        .unwrap()
                        .push_back(&mut irgen.ctx, br2entry)
                        .unwrap();
                }
                irgen.cur_bbk = Some(br_entry);

                let then_block = BasicBlock::new(&mut irgen.ctx);
                let exit_block = BasicBlock::new(&mut irgen.ctx);
                let mut else_block = None;
                // 生成跳转指令
                if else_then.is_none() {
                    irgen.ast2ir_cond(cond, then_block, exit_block).unwrap();
                } else {
                    else_block = Some(BasicBlock::new(&mut irgen.ctx));
                    irgen
                        .ast2ir_cond(cond, then_block, else_block.unwrap())
                        .unwrap();
                }

                // Then BBK 处理if分支下的Block
                irgen
                    .cur_func
                    .unwrap()
                    .push_back(&mut irgen.ctx, then_block)
                    .unwrap();
                irgen.cur_bbk = Some(then_block);
                then.irgen(irgen);

                if !irgen.cur_bbk.unwrap().is_terminated(&irgen.ctx) {
                    // 没有提前跳出就生成跳到退出块的指令
                    let jump = Instruction::br(&mut irgen.ctx, exit_block);
                    irgen
                        .cur_bbk
                        .unwrap()
                        .push_back(&mut irgen.ctx, jump)
                        .unwrap();
                }

                if else_block.is_some() {
                    //Else BBK 处理else分支下的Block
                    irgen
                        .cur_func
                        .unwrap()
                        .push_back(&mut irgen.ctx, else_block.unwrap())
                        .unwrap();
                    irgen.cur_bbk = Some(else_block.unwrap());
                    if let Some(else_stmt) = else_then {
                        else_stmt.irgen(irgen);
                    }

                    if !irgen.cur_bbk.unwrap().is_terminated(&irgen.ctx) {
                        let jump = Instruction::br(&mut irgen.ctx, exit_block);
                        irgen
                            .cur_bbk
                            .unwrap()
                            .push_back(&mut irgen.ctx, jump)
                            .unwrap();
                    }
                }

                // Exit BBK 处理退出块
                irgen
                    .cur_func
                    .unwrap()
                    .push_back(&mut irgen.ctx, exit_block)
                    .unwrap();
                irgen.cur_bbk = Some(exit_block);
            }
            Stmt::While(box_while) => {
                let cond = &box_while.cond;
                let body_block = &box_while.block;
                let loop_entry = BasicBlock::new(&mut irgen.ctx);
                let loop_body = BasicBlock::new(&mut irgen.ctx);
                let loop_exit = BasicBlock::new(&mut irgen.ctx);

                // 循环入口
                if !irgen.cur_bbk.unwrap().is_terminated(&irgen.ctx) {
                    let br2entry = Instruction::br(&mut irgen.ctx, loop_entry);
                    irgen
                        .cur_bbk
                        .unwrap()
                        .push_back(&mut irgen.ctx, br2entry)
                        .unwrap();
                }
                irgen
                    .cur_func
                    .unwrap()
                    .push_back(&mut irgen.ctx, loop_entry)
                    .unwrap();
                irgen
                    .cur_func
                    .unwrap()
                    .push_back(&mut irgen.ctx, loop_body)
                    .unwrap();

                // 循环体
                irgen.loop_entry_stack.push(loop_entry);
                irgen.loop_exit_stack.push(loop_exit);
                irgen.cur_bbk = Some(loop_entry);

                irgen.ast2ir_cond(cond, loop_body, loop_exit).unwrap();
                irgen.cur_bbk = Some(loop_body);

                body_block.irgen(irgen);

                if !irgen.cur_bbk.unwrap().is_terminated(&irgen.ctx) {
                    let jump_back = Instruction::br(&mut irgen.ctx, loop_entry);
                    irgen
                        .cur_bbk
                        .unwrap()
                        .push_back(&mut irgen.ctx, jump_back)
                        .unwrap();
                }

                irgen
                    .cur_func
                    .unwrap()
                    .push_back(&mut irgen.ctx, loop_exit)
                    .unwrap();
                irgen.cur_bbk = Some(loop_exit);

                irgen.loop_entry_stack.pop();
                irgen.loop_exit_stack.pop();
            }
            Stmt::Break(_tmp) => {
                if !irgen.cur_bbk.unwrap().is_terminated(&irgen.ctx) {
                    let dst = irgen.loop_exit_stack.last().unwrap();
                    let jump = Instruction::br(&mut irgen.ctx, *dst);
                    let res = irgen.cur_bbk.unwrap().push_back(&mut irgen.ctx, jump);
                    if res.is_err() {
                        panic!("Failed to push back the instruction");
                    }
                    let new_bbk = BasicBlock::new(&mut irgen.ctx);
                    let res = irgen.cur_func.unwrap().push_back(&mut irgen.ctx, new_bbk);
                    if res.is_err() {
                        panic!("Failed to push back the basic block");
                    }
                    irgen.cur_bbk = Some(new_bbk);
                }
            }
            Stmt::Continue(_tmp) => {
                if !irgen.cur_bbk.unwrap().is_terminated(&irgen.ctx) {
                    let dst = irgen.loop_entry_stack.last().unwrap();
                    let jump = Instruction::br(&mut irgen.ctx, *dst);
                    let res = irgen.cur_bbk.unwrap().push_back(&mut irgen.ctx, jump);
                    if res.is_err() {
                        panic!("Failed to push back the instruction");
                    }
                    let new_bbk = BasicBlock::new(&mut irgen.ctx);
                    let res = irgen.cur_func.unwrap().push_back(&mut irgen.ctx, new_bbk);
                    if res.is_err() {
                        panic!("Failed to push back the basic block");
                    }
                    irgen.cur_bbk = Some(new_bbk);
                }
            }
            Stmt::Return(ast::Return { exp }) => {
                if let Some(exp) = exp {
                    let val = irgen.ast2ir_exp(exp).unwrap();
                    let typ_of_ir = irgen.ast2ir_type(&exp.typ());
                    let store = Instruction::memaccess(
                        &mut irgen.ctx,
                        MemAccessOp::Store,
                        typ_of_ir,
                        vec![val, irgen.cur_ret_slot.unwrap()],
                    );
                    irgen
                        .cur_bbk
                        .unwrap()
                        .push_back(&mut irgen.ctx, store)
                        .unwrap();
                }

                if !irgen.cur_bbk.unwrap().is_terminated(&irgen.ctx) {
                    let br = Instruction::br(&mut irgen.ctx, irgen.cur_ret_bbk.unwrap());
                    irgen
                        .cur_bbk
                        .unwrap()
                        .push_back(&mut irgen.ctx, br)
                        .unwrap();
                }
            }
        }
    }
}

impl IrGen for ast::Block {
    fn irgen(&self, irgen: &mut IrGenContext) {
        irgen.symboltable.enter_scope();
        for item in self.items.iter() {
            if irgen.cur_bbk.unwrap().is_terminated(&irgen.ctx) {
                return;
            }
            match item {
                BlockItem::Decl(decl) => decl.irgen(irgen),
                BlockItem::Stmt(stmt) => stmt.irgen(irgen),
            }
        }
        irgen.symboltable.leave_scope();
    }
}

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::{
    frontend::{
        ir::{
            basicblock::BasicBlock,
            context::Context,
            defuse::{Useable, User},
            function,
            instruction::{
                BinaryOp, FCompCond, ICompCond, Instruction, InstructionKind, MemAccessOp, UnaryOp,
            },
            value::{ConstantValue, Value},
        },
        ir2string::Display,
    },
    passes::{
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::LinkedListContainer,
};

pub struct SCCP;

// lattice.rs
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum LatticeValue {
    Top,             // 未知值
    Constant,        // 已知常量
    ConstantAddress, // 已知常量地址
    Bottom,          // 非常量值
}

impl LatticeValue {
    pub fn is_top(&self) -> bool {
        matches!(self, Self::Top)
    }

    pub fn is_constant(&self) -> bool {
        matches!(self, Self::Constant)
    }

    pub fn is_constant_address(&self) -> bool {
        matches!(self, Self::ConstantAddress)
    }

    pub fn is_bottom(&self) -> bool {
        matches!(self, Self::Bottom)
    }

    pub fn calculate(op1: Self, op2: Self) -> Self {
        match (op1, op2) {
            (Self::Bottom, _x) | (_x, Self::Bottom) => Self::Bottom,
            (Self::Top, _x) | (_x, Self::Top) => Self::Top,
            (Self::Constant, Self::Constant) => Self::Constant,
            (Self::ConstantAddress, Self::Constant) | (Self::Constant, Self::ConstantAddress) => {
                Self::ConstantAddress
            }
            (Self::ConstantAddress, Self::ConstantAddress) => Self::ConstantAddress,
        }
    }

    pub fn calculate_gep(ops: &[Self]) -> Self {
        let mut res = Self::Top;
        for op in ops {
            res = LatticeValue::calculate(res, op.clone());
        }
        res
    }
}

impl Pass for SCCP {
    type Output = bool;
    type Error = PassError;

    fn pass_type(&self) -> PassType {
        PassType::GlobalBasedOptimization
    }

    fn execute(&mut self, ctx: &mut Context, pctx: &mut PassContext) -> Result<bool, PassError> {
        // 初始化工作列表和格值映射
        let mut worklist: Vec<Value> = Vec::new();
        let mut lattice_values = HashMap::default();
        let mut dead_bbks = HashSet::default();
        let mut _inst_removed = 0;

        // 初始化所有值为Top（未知）
        let functions: Vec<function::Function> = ctx.get_functions().collect();
        for func in functions {
            let cfg = &mut pctx.cfginfo.get_mut(&func.get_id(ctx)).unwrap().cfg;
            let mut stack = vec![cfg.entry_block.unwrap()];
            let mut visited = HashSet::default();
            while let Some(bbk) = stack.pop() {
                visited.insert(bbk);
                let insts: Vec<Instruction> = bbk.iter(ctx).collect();
                for inst in insts {
                    let res_value = inst.get_result(ctx);
                    if res_value.is_none() {
                        continue;
                    } else {
                        let mut res_value = res_value.unwrap();
                        if inst.is_call(ctx) {
                            lattice_values.insert(res_value, LatticeValue::Bottom);
                        }
                        // else if inst.is_gep(ctx){
                        //     lattice_values.insert(res_value, LatticeValue::Top);//????
                        // }
                        else if inst.is_alloca(ctx) {
                            lattice_values.insert(res_value, LatticeValue::ConstantAddress);
                        } else if inst.is_constant(ctx) {
                            lattice_values.insert(res_value, LatticeValue::Constant);
                            res_value = Self::apply_constant_propagation_single(
                                ctx,
                                &inst,
                                &mut lattice_values,
                            );
                            worklist.push(res_value);
                            _inst_removed += 1;
                        } else {
                            lattice_values.insert(res_value, LatticeValue::Top);
                        }
                    }
                }

                let successors = cfg.get_successors(&bbk).unwrap();
                for succ in successors {
                    if !visited.contains(&succ) {
                        stack.push(*succ);
                    }
                }
            }
        }

        // 工作列表算法
        while !worklist.is_empty() {
            let val = worklist.pop();
            if val.is_none() {
                panic!("Failed to pop value from worklist");
            }
            let val = val.unwrap();

            if val.is_removed(ctx) {
                continue;
            } // 可能在之前的传播中就已经被覆盖，并不是很好的写法

            let users = val.users(ctx).into_iter().collect::<Vec<User<Value>>>();

            for user in users {
                let inst = user.get_instruction();

                if inst.is_call(ctx) || inst.is_br(ctx) || inst.is_ret(ctx) {
                    // call指令的结果只能假设为Bottom，或者没有结果的call指令直接跳过
                    continue;
                } else if inst.is_store(ctx) || inst.is_alloca(ctx) || inst.is_bitcast(ctx) {
                    // stroe指令没有结果单独处理
                    continue; //目前先进行简化处理
                } else if inst.is_cbr(ctx) {
                    let op = inst.get_operand(ctx, 0).unwrap();
                    let op_lat = lattice_values.get(&op).unwrap_or(&LatticeValue::Top);
                    if op_lat.is_constant() {
                        if op.get_bool_const_value(ctx).unwrap() {
                            let (dead_bbk, const_phis) = Self::simplify_cbr_single(
                                ctx,
                                &inst.get_operand_bbk(ctx, 1).unwrap(),
                                inst,
                                pctx,
                                &mut lattice_values,
                            );
                            _inst_removed += const_phis.len();
                            worklist.extend(const_phis);
                            if dead_bbk.is_some() {
                                dead_bbks.insert(dead_bbk.unwrap());
                                let const_phis = Self::apply_cfg_propagation(
                                    ctx,
                                    pctx,
                                    &dead_bbk.unwrap(),
                                    &mut lattice_values,
                                );
                                _inst_removed += const_phis.len();
                                worklist.extend(const_phis);
                            }
                        } else {
                            let (dead_bbk, const_phis) = Self::simplify_cbr_single(
                                ctx,
                                &inst.get_operand_bbk(ctx, 0).unwrap(),
                                inst,
                                pctx,
                                &mut lattice_values,
                            );
                            _inst_removed += const_phis.len();
                            worklist.extend(const_phis);
                            if dead_bbk.is_some() {
                                dead_bbks.insert(dead_bbk.unwrap());
                                let const_phis = Self::apply_cfg_propagation(
                                    ctx,
                                    pctx,
                                    &dead_bbk.unwrap(),
                                    &mut lattice_values,
                                );
                                _inst_removed += const_phis.len();
                                worklist.extend(const_phis);
                            }
                        }
                    }
                    continue;
                }
                let res_value = inst.get_result(ctx).expect(
                    format!(
                        "Failed to get result in struct `Instruction [{}]`",
                        inst.display(ctx)
                    )
                    .as_str(),
                );
                let old_value = lattice_values
                    .get(&res_value)
                    .cloned()
                    .unwrap_or(LatticeValue::Top);
                let new_value =
                    SCCP::evaluate_instruction(inst, ctx, &lattice_values, &mut dead_bbks);

                if old_value != new_value {
                    // 如果值发生变化，加入工作列表
                    if new_value.is_constant() {
                        worklist.push(res_value);
                        let res_value = Self::apply_constant_propagation_single(
                            ctx,
                            &inst,
                            &mut lattice_values,
                        );
                        _inst_removed += 1;
                        worklist.push(res_value);
                    } else if new_value.is_bottom() {
                        lattice_values.insert(res_value, new_value.clone());
                        worklist.push(res_value);
                    } else {
                        lattice_values.insert(res_value, new_value.clone());
                    }
                }
            }
        }

        // 应用常量传播
        // Self::apply_constant_propagation(ctx, &constant_inst);

        // 简化控制流
        Self::simplify_control_flow(ctx, &dead_bbks, pctx);

        #[cfg(debug_assertions)]
        println!(
            "···[SCCP]Removed {} bbks, {} instructions",
            dead_bbks.len(),
            _inst_removed
        );
        Ok(true)
    }

    fn name(&self) -> &'static str {
        "SparseConditionalConstantPropagation"
    }

    fn dependencies(&self) -> &[&'static str] {
        &["CFAnalysis"]
    }

    fn effects(&self) -> &[&'static str] {
        &["DominatorRelAnalysis"]
    }
}

impl SCCP {
    /// 评估指令的格值
    fn evaluate_instruction(
        inst: Instruction,
        ctx: &Context,
        lattice: &HashMap<Value, LatticeValue>,
        dead_bbks: &mut HashSet<BasicBlock>,
    ) -> LatticeValue {
        match Instruction::get_kind(&inst, ctx) {
            InstructionKind::Terminator { op: _ } => LatticeValue::Bottom,
            InstructionKind::Unary { op: _ } => {
                let op = inst.get_operand(ctx, 0).unwrap();
                lattice.get(&op).cloned().unwrap_or(LatticeValue::Top)
            }
            InstructionKind::Binary { op: _ } | InstructionKind::BitBinary { op: _ } => {
                let op1 = lattice
                    .get(&inst.get_operand(ctx, 0).unwrap())
                    .cloned()
                    .unwrap_or(LatticeValue::Top);
                let op2 = lattice
                    .get(&inst.get_operand(ctx, 1).unwrap())
                    .cloned()
                    .unwrap_or(LatticeValue::Top);
                LatticeValue::calculate(op1, op2)
            }
            InstructionKind::MemAccess { op } => {
                match op {
                    MemAccessOp::Load => LatticeValue::Top,
                    MemAccessOp::Store => LatticeValue::Top, //self.get_operand(ctx, 0).unwrap().is_constant(ctx),
                    MemAccessOp::Alloca { .. } => LatticeValue::ConstantAddress,
                    MemAccessOp::GetElementPtr { .. } => {
                        let ops = inst
                            .get_operands(ctx)
                            .iter()
                            .map(|op| lattice.get(op).cloned().unwrap_or(LatticeValue::Top))
                            .collect::<Vec<LatticeValue>>();
                        LatticeValue::calculate_gep(&ops)
                    }
                }
            }
            InstructionKind::Conversion { op: _ } => {
                let op = inst.get_operand(ctx, 0).unwrap();
                lattice.get(&op).cloned().unwrap_or(LatticeValue::Top)
            }
            InstructionKind::IComp { cond: _ } => {
                let op1 = lattice
                    .get(&inst.get_operand(ctx, 0).unwrap())
                    .cloned()
                    .unwrap_or(LatticeValue::Top);
                let op2 = lattice
                    .get(&inst.get_operand(ctx, 1).unwrap())
                    .cloned()
                    .unwrap_or(LatticeValue::Top);
                LatticeValue::calculate(op1, op2)
            }
            InstructionKind::FComp { cond: _ } => {
                let op1 = lattice
                    .get(&inst.get_operand(ctx, 0).unwrap())
                    .cloned()
                    .unwrap_or(LatticeValue::Top);
                let op2 = lattice
                    .get(&inst.get_operand(ctx, 1).unwrap())
                    .cloned()
                    .unwrap_or(LatticeValue::Top);
                LatticeValue::calculate(op1, op2)
            }
            InstructionKind::Phi => {
                let phi_ops = inst.get_phi(ctx);
                let mut pre_val = None;
                let typ = inst.result_typ(ctx);
                for (bbk, val) in phi_ops {
                    if dead_bbks.contains(&bbk) {
                        continue;
                    } else {
                        if lattice.get(&val).unwrap_or(&LatticeValue::Top).is_bottom() {
                            return LatticeValue::Bottom;
                        } else if lattice.get(&val).unwrap_or(&LatticeValue::Top).is_top() {
                            return LatticeValue::Top;
                        } else {
                            if pre_val.is_none() {
                                pre_val = Some(val);
                            }
                            if typ.is_int(ctx) {
                                let cur = val.get_int_const_value(ctx).unwrap();
                                let pre = pre_val.unwrap().get_int_const_value(ctx).unwrap();
                                if cur != pre {
                                    return LatticeValue::Top;
                                }
                            } else if typ.is_float(ctx) {
                                let cur = val.get_float_const_value(ctx).unwrap();
                                let pre = pre_val.unwrap().get_float_const_value(ctx).unwrap();
                                if cur != pre {
                                    return LatticeValue::Top;
                                }
                            } else {
                                panic!(
                                    "[SCCP] Unsupported type in constant propagation of Phi instruction"
                                );
                            }
                            pre_val = Some(val);
                        }
                    }
                }
                LatticeValue::Constant
            }
            InstructionKind::Call { .. } => LatticeValue::Bottom,
        }
    }

    fn apply_cfg_propagation(
        ctx: &mut Context,
        pctx: &mut PassContext,
        dead_bbk: &BasicBlock,
        lattices: &mut HashMap<Value, LatticeValue>,
    ) -> Vec<Value> {
        let func = dead_bbk.get_function(ctx).unwrap().get_id(ctx);
        let cfg = &mut pctx.cfginfo.get_mut(&func).unwrap().cfg;
        let successors = cfg.get_successors(&dead_bbk).unwrap();
        if successors.is_empty() {
            vec![]
        } else {
            let mut res = vec![];
            for succ in successors {
                let insts = succ.iter(ctx).collect::<Vec<Instruction>>();
                for inst in insts {
                    if inst.is_phi(ctx) {
                        inst.remove_phi_operand(ctx, *dead_bbk);
                        if inst.is_constant(ctx) {
                            let res_value =
                                Self::apply_constant_propagation_single(ctx, &inst, lattices);
                            res.push(res_value);
                        }
                    } else {
                        break;
                    }
                }
            }
            res
        }
    }

    fn apply_constant_propagation_single(
        ctx: &mut Context,
        inst: &Instruction,
        lattice: &mut HashMap<Value, LatticeValue>,
    ) -> Value {
        let res = Self::calculate_res(ctx, inst);
        if res.is_none() {
            panic!("[SCCP] Instruction [{}] has no result", inst.display(ctx));
        }
        lattice.insert(res.unwrap(), LatticeValue::Constant);
        inst.replace_all_uses_with(ctx, res.unwrap());
        inst.remove(ctx);
        res.unwrap()
    }

    fn calculate_res(ctx: &mut Context, inst: &Instruction) -> Option<Value> {
        match Instruction::get_kind(inst, ctx) {
            InstructionKind::Terminator { op: _ } => None,
            InstructionKind::Unary { op } => {
                let cont_val = inst.get_operand(ctx, 0).unwrap();
                let value = Self::calculate_unary(op, cont_val, ctx);
                Some(Value::constant(ctx, value))
            }
            InstructionKind::Binary { op } => {
                let op1 = inst.get_operand(ctx, 0).unwrap();
                let op2 = inst.get_operand(ctx, 1).unwrap();
                let typ = op1.kind(ctx);
                let value;
                if typ.is_int(ctx) {
                    let op1 = op1.get_int_const_value(ctx).unwrap();
                    let op2 = op2.get_int_const_value(ctx).unwrap();
                    let val_rust = Self::calculate_binary(op, op1, op2);
                    value = ConstantValue::int32(ctx, val_rust);
                } else if typ.is_float(ctx) {
                    let op1 = op1.get_float_const_value(ctx).unwrap();
                    let op2 = op2.get_float_const_value(ctx).unwrap();
                    let val_rust = Self::calculate_binary(op, op1, op2);
                    value = ConstantValue::float32(ctx, val_rust);
                } else {
                    panic!("[SCCP] Unsupported type in constant propagation of Binary instruction");
                }
                Some(Value::constant(ctx, value))
            }
            InstructionKind::BitBinary { op: _ } => {
                todo!("[SCCP] Unsupported BitBinary instruction in constant propagation");
            }
            InstructionKind::MemAccess { op } => match op {
                MemAccessOp::Load => inst.get_operand(ctx, 0),
                MemAccessOp::Store => None,
                MemAccessOp::Alloca { .. } => None,
                MemAccessOp::GetElementPtr { .. } => None,
            },
            InstructionKind::Conversion { op: _ } => {
                let op = inst.get_operand(ctx, 0).unwrap();
                let typ = op.kind(ctx);
                let tar_typ = inst.result_typ(ctx);
                let mut value = ConstantValue::Undef { typ };
                if typ.is_int(ctx) && tar_typ.is_float(ctx) {
                    let op_val = op.get_int_const_value(ctx).unwrap();
                    value = ConstantValue::float32(ctx, op_val as f32);
                } else if typ.is_float(ctx) && tar_typ.is_int(ctx) {
                    let op_val = op.get_float_const_value(ctx).unwrap();
                    value = ConstantValue::int32(ctx, op_val as i32);
                } else if typ.is_bool(ctx) && tar_typ.is_int(ctx) {
                    let op_val = op.get_bool_const_value(ctx).unwrap();
                    value = ConstantValue::int32(ctx, op_val as i32);
                } else if typ.is_int(ctx) && tar_typ.is_bool(ctx) {
                    let op_val = op.get_int_const_value(ctx).unwrap();
                    value = ConstantValue::bool(ctx, op_val != 0);
                } else if typ.is_bool(ctx) && tar_typ.is_float(ctx) {
                    let op_val = op.get_bool_const_value(ctx).unwrap();
                    value = ConstantValue::float32(ctx, op_val as i32 as f32);
                } else if typ.is_float(ctx) && tar_typ.is_bool(ctx) {
                    let op_val = op.get_float_const_value(ctx).unwrap();
                    value = ConstantValue::bool(ctx, op_val != 0.0);
                }
                Some(Value::constant(ctx, value))
            }
            InstructionKind::IComp { cond } => {
                let op1 = inst.get_operand(ctx, 0).unwrap();
                let op2 = inst.get_operand(ctx, 1).unwrap();
                let typ = op1.kind(ctx);
                let value;
                if typ.is_int(ctx) {
                    let op1 = op1.get_int_const_value(ctx).unwrap();
                    let op2 = op2.get_int_const_value(ctx).unwrap();
                    let val_rust = Self::calculate_icomp(cond, op1, op2);
                    value = ConstantValue::bool(ctx, val_rust);
                } else {
                    panic!("[SCCP] Unsupported type in constant propagation of IComp instruction");
                }
                Some(Value::constant(ctx, value))
            }
            InstructionKind::FComp { cond } => {
                let op1 = inst.get_operand(ctx, 0).unwrap();
                let op2 = inst.get_operand(ctx, 1).unwrap();
                let typ = op1.kind(ctx);
                let value;
                if typ.is_float(ctx) {
                    let op1 = op1.get_float_const_value(ctx).unwrap();
                    let op2 = op2.get_float_const_value(ctx).unwrap();
                    let val_rust = Self::calculate_fcomp(cond, op1, op2);
                    value = ConstantValue::bool(ctx, val_rust);
                } else {
                    panic!("[SCCP] Unsupported type in constant propagation of FComp instruction");
                }
                Some(Value::constant(ctx, value))
            }
            InstructionKind::Phi => {
                let mut phi_ops = None;
                for (_bbk, val) in inst.get_phi(ctx) {
                    if val.is_constant(ctx) {
                        phi_ops = Some(val);
                        break;
                    }
                }
                if phi_ops.is_none() {
                    panic!("[SCCP] Missing operand in constant propagation of Phi instruction")
                } else {
                    let val = phi_ops.unwrap();
                    let typ = val.kind(ctx);
                    let value;
                    if typ.is_float(ctx) {
                        let val_rust = val.get_float_const_value(ctx).unwrap();
                        value = ConstantValue::float32(ctx, val_rust);
                    } else if typ.is_int(ctx) {
                        let val_rust = val.get_int_const_value(ctx).unwrap();
                        value = ConstantValue::int32(ctx, val_rust);
                    } else {
                        panic!(
                            "[SCCP] Unsupported type in constant propagation of Phi instruction"
                        );
                    }
                    Some(Value::constant(ctx, value))
                }
            }
            InstructionKind::Call { .. } => None,
        }
    }

    fn calculate_unary(op: UnaryOp, val: Value, ctx: &mut Context) -> ConstantValue {
        let typ = val.kind(ctx);
        let value;
        if typ.is_int(ctx) {
            let mut cont_val = val.get_int_const_value(ctx).unwrap();
            cont_val = match op {
                UnaryOp::Fneg => -cont_val,
                UnaryOp::Not => !cont_val,
            };
            value = ConstantValue::int32(ctx, cont_val);
        } else if typ.is_float(ctx) {
            let mut cont_val = val.get_float_const_value(ctx).unwrap();
            cont_val = match op {
                UnaryOp::Fneg => -cont_val,
                _ => panic!(
                    "[SCCP] Unsupported unary operation [Not] in constant propagation of Unary instruction"
                ),
            };
            value = ConstantValue::float32(ctx, cont_val);
        } else if typ.is_bool(ctx) {
            let mut cont_val = val.get_bool_const_value(ctx).unwrap();
            cont_val = match op {
                UnaryOp::Not => !cont_val,
                _ => panic!(
                    "[SCCP] Unsupported unary operation [Not] in constant propagation of Unary instruction"
                ),
            };
            value = ConstantValue::bool(ctx, cont_val);
        } else {
            panic!("[SCCP] Unsupported type in constant propagation of Unary instruction");
        }
        value
    }

    fn calculate_binary<T>(op: BinaryOp, val1: T, val2: T) -> T
    where
        T: std::ops::Add<Output = T>
            + std::ops::Sub<Output = T>
            + std::ops::Mul<Output = T>
            + std::ops::Div<Output = T>
            + std::ops::Rem<Output = T>
            + Copy,
    {
        match op {
            BinaryOp::Add => val1 + val2,
            BinaryOp::Fadd => val1 + val2,
            BinaryOp::Sub => val1 - val2,
            BinaryOp::FSub => val1 - val2,
            BinaryOp::Mul => val1 * val2,
            BinaryOp::Fmul => val1 * val2,
            BinaryOp::Udiv => val1 / val2,
            BinaryOp::Sdiv => val1 / val2,
            BinaryOp::Fdiv => val1 / val2,
            BinaryOp::Urem => val1 % val2,
            BinaryOp::Srem => val1 % val2,
            BinaryOp::Frem => val1 % val2,
        }
    }

    fn calculate_icomp<T>(op: ICompCond, val1: T, val2: T) -> bool
    where
        T: std::cmp::PartialOrd,
    {
        match op {
            ICompCond::Eq => val1 == val2,
            ICompCond::Ne => val1 != val2,
            ICompCond::Ugt => val1 > val2,
            ICompCond::Uge => val1 >= val2,
            ICompCond::Ult => val1 < val2,
            ICompCond::Ule => val1 <= val2,
            ICompCond::Sgt => val1 > val2,
            ICompCond::Sge => val1 >= val2,
            ICompCond::Slt => val1 < val2,
            ICompCond::Sle => val1 <= val2,
        }
    }

    fn calculate_fcomp<T>(op: FCompCond, val1: T, val2: T) -> bool
    where
        T: std::cmp::PartialOrd,
    {
        match op {
            FCompCond::False => false,
            FCompCond::True => true,
            FCompCond::Oeq => val1 == val2,
            FCompCond::Ogt => val1 > val2,
            FCompCond::Oge => val1 >= val2,
            FCompCond::Olt => val1 < val2,
            FCompCond::Ole => val1 <= val2,
            FCompCond::One => val1 != val2,
            FCompCond::Ord => panic!(
                "[SCCP] Unsupported FCompCond::Ord in constant propagation of FComp instruction"
            ),
            FCompCond::Uno => panic!(
                "[SCCP] Unsupported FCompCond::Uno in constant propagation of FComp instruction"
            ),
            FCompCond::Ueq => val1 == val2,
            FCompCond::Ugt => val1 > val2,
            FCompCond::Uge => val1 >= val2,
            FCompCond::Ule => val1 <= val2,
            FCompCond::Ult => val1 < val2,
            FCompCond::Une => val1 != val2,
        }
    }

    /// 简化控制流
    fn simplify_control_flow(
        ctx: &mut Context,
        dead_bbks: &HashSet<BasicBlock>,
        pctx: &mut PassContext,
    ) -> usize {
        let mut num_removed = 0;
        for bbk in dead_bbks {
            let func = bbk.get_function(ctx).unwrap().get_id(ctx);
            let cfg = &mut pctx.cfginfo.get_mut(&func).unwrap().cfg;
            cfg.remove_bbk(bbk);
            bbk.remove(ctx);
            num_removed += 1;
        }
        num_removed
    }

    fn simplify_cbr_single(
        ctx: &mut Context,
        bbk: &BasicBlock,
        cbr: Instruction,
        pctx: &mut PassContext,
        lattices: &mut HashMap<Value, LatticeValue>,
    ) -> (Option<BasicBlock>, Vec<Value>) {
        let mut res = vec![];
        let mut tar_bbk = cbr.get_operand_bbk(ctx, 0).unwrap();
        if &tar_bbk == bbk {
            tar_bbk = cbr.get_operand_bbk(ctx, 1).unwrap();
        }
        // inst从条件跳转变为无条件跳转，需要对控制流图进行修改，
        // 但是这个基本块删除与否取决于该基本快是否还有可达路径
        let func = bbk.get_function(ctx).unwrap().get_id(ctx);
        let cfg = &mut pctx.cfginfo.get_mut(&func).unwrap().cfg;
        let cur_bbk = cbr.get_basicblock(ctx).unwrap();

        let insts = bbk.iter(ctx).collect::<Vec<Instruction>>();
        for inst in insts {
            if inst.is_phi(ctx) {
                inst.remove_phi_operand(ctx, cur_bbk);
                if inst.is_constant(ctx) {
                    let res_value = Self::apply_constant_propagation_single(ctx, &inst, lattices);
                    res.push(res_value);
                }
            } else {
                break;
            }
        }
        cfg.succ_set.get_mut(&cur_bbk).unwrap().remove(bbk);
        cfg.pre_set.get_mut(bbk).unwrap().remove(&cur_bbk);
        let br = Instruction::br(ctx, tar_bbk);
        let _ = cur_bbk.push_back(ctx, br);
        cbr.remove(ctx);

        let pre_num = cfg.pre_set.get(bbk).unwrap().len();
        if pre_num == 0 {
            //前驱基本块为0，说明该基本块没有前驱，可以删除，会对后继的phi节点产生影响
            (Some(*bbk), res)
        } else {
            (None, res)
        }
    }
}

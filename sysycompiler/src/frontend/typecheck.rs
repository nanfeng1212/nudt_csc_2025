use crate::frontend;
use crate::frontend::lalrpop::ast::{self, *};
use crate::frontend::symboltable::*;

impl CompUnit {
    pub fn type_check(&mut self) {
        let mut symtable = SymbolTable::default();
        // 全局作用域
        symtable.enter_scope();
        // 注册系统库
        symtable.register_sysylib();

        for item in self.items.iter_mut() {
            item.type_check(&mut symtable);
        }

        // 退出全局作用域
        symtable.leave_scope();
    }
}

impl GlobalItem {
    pub fn type_check(&mut self, symtable: &mut SymbolTable) {
        match self {
            GlobalItem::Decl(decl) => match decl {
                Decl::ConstDecl(decl) => decl.type_check(symtable),
                Decl::VarDecl(decl) => decl.type_check(symtable),
            },
            GlobalItem::FuncDef(FuncDef {
                typ,
                id,
                params,
                block,
            }) => {
                symtable.enter_scope();
                // println!("func: {} {}", id, typ);
                // 插入函数参数到作用域
                let mut param_tys = Vec::new();
                for param in params.iter() {
                    let dimensions = param.dimensions.clone();
                    let mut typ = param.typ.clone();
                    if !dimensions.is_none() {
                        for dim in dimensions.unwrap().iter().rev() {
                            typ = Type::array(
                                typ,
                                dim.try_fold(symtable)
                                    .expect("non-constant dim")
                                    .unwrap2int() as usize,
                            );
                        }
                        typ = Type::ptr(typ);
                    }
                    // println!("param: {} {}", param.id, typ);
                    param_tys.push(typ.clone());
                    symtable.insert(param.id.clone(), SymbolEntry::from_ty(typ.clone()));
                }

                let func_ty = Type::function(param_tys, typ.clone());

                // 插入函数符号到作用域，位于当前作用域之上，因为我们处于参数作用域
                symtable.insert_upper(id.clone(), SymbolEntry::from_ty(func_ty), 1);
                symtable.curr_ret_ty = Some(typ.clone());

                block.type_check(symtable);

                symtable.curr_ret_ty = None;
                symtable.leave_scope();
            }
        }
    }
}

impl ConstDecl {
    pub fn type_check(&mut self, symtable: &mut SymbolTable) {
        let mut new_defs = Vec::new();

        for mut def in self.defs.drain(..) {
            let mut shape = def
                .dimensions
                .drain(..)
                .map(|expr| {
                    expr.try_fold(symtable)
                        .expect("non-constant dim")
                        .unwrap2int()
                })
                .collect::<Vec<i32>>();

            let mut typ = self.typ.clone();
            for dim in shape.iter().rev() {
                typ = Type::array(typ, *dim as usize);
            }

            def.init = frontend::lalrpop::ast::ConstInitVal::Exp(
                def.init.type_check(Some(&typ), symtable),
            );

            let folded = def
                .init
                .as_mut_exp()
                .try_fold(symtable)
                .expect("non-constant init");
            def.init = ConstInitVal::Exp(Exp::const_(folded.clone()));

            def.dimensions = shape
                .drain(..)
                .map(ComptimeValue::int)
                .map(Exp::const_)
                .map(|mut e| {
                    e.typ = Some(Type::int());
                    e
                })
                .collect::<Vec<ast::Exp>>();

            // 插入常量符号到作用域
            symtable.insert(
                def.id.clone(),
                SymbolEntry {
                    typ,
                    comptime: Some(folded),
                    ir_value: None,
                },
            );
            new_defs.push(def);
        }

        self.defs = new_defs;
    }
}

impl ConstInitVal {
    /// 返回一个指向表达式的引用，用于修改表达式的值。
    fn as_mut_exp(&mut self) -> &mut Exp {
        match self {
            ConstInitVal::Exp(num) => num,
            ConstInitVal::List(num) => num,
        }
    }

    fn type_check(self, expect_type: Option<&Type>, symboltable: &mut SymbolTable) -> Exp {
        match self {
            ConstInitVal::Exp(exp) => exp.type_check(expect_type, symboltable),
            ConstInitVal::List(list) => list.type_check(expect_type, symboltable),
        }
    }
}

impl VarDecl {
    pub fn type_check(&mut self, symtable: &mut SymbolTable) {
        let mut new_defs = Vec::new();

        for mut def in self.defs.drain(..) {
            let mut shape = def
                .dimensions
                .drain(..)
                .map(|expr| {
                    expr.try_fold(symtable)
                        .expect("non-constant dim")
                        .unwrap2int()
                })
                .collect::<Vec<i32>>();

            let mut typ = self.typ.clone();
            for dim in shape.iter().rev() {
                typ = Type::array(typ, *dim as usize);
            }

            let init = def
                .init
                .map(|init| {
                    // fold as much as possible
                    let typed_init = init.type_check(Some(&typ), symtable);
                    match typed_init.try_fold(symtable) {
                        Some(val) => Exp::const_(val),
                        None => typed_init,
                    }
                })
                .unwrap_or_else(|| {
                    let undef = ComptimeValue::undef(typ.clone());
                    Exp::const_(undef)
                });

            def.init = Some(frontend::lalrpop::ast::InitVal::Exp(init));

            def.dimensions = shape
                .drain(..)
                .map(ComptimeValue::int)
                .map(Exp::const_)
                .map(|mut e| {
                    e.typ = Some(Type::int());
                    e
                })
                .collect::<Vec<ast::Exp>>();

            symtable.insert(def.id.clone(), SymbolEntry::from_ty(typ));
            new_defs.push(def);
        }

        self.defs = new_defs;
    }
}

impl InitVal {
    pub fn type_check(self, expect_type: Option<&Type>, symboltable: &mut SymbolTable) -> Exp {
        match self {
            InitVal::Exp(exp) => exp.type_check(expect_type, symboltable),
            InitVal::List(list) => list.type_check(expect_type, symboltable),
        }
    }
}

impl Block {
    pub fn type_check(&mut self, symtable: &mut SymbolTable) {
        // 进入块作用域
        symtable.enter_scope();
        let mut new_items = Vec::new();

        for item in self.items.drain(..) {
            let item = match item {
                BlockItem::Decl(decl) => match decl {
                    Decl::ConstDecl(mut decl) => {
                        decl.type_check(symtable);
                        BlockItem::Decl(Decl::ConstDecl(decl))
                    }
                    Decl::VarDecl(mut decl) => {
                        decl.type_check(symtable);
                        BlockItem::Decl(Decl::VarDecl(decl))
                    }
                },
                BlockItem::Stmt(stmt) => {
                    let stmt = stmt.type_check(symtable);
                    BlockItem::Stmt(stmt)
                }
            };
            new_items.push(item);
        }

        self.items = new_items;
        symtable.leave_scope();
    }
}

impl Stmt {
    pub fn type_check(self, symtable: &mut SymbolTable) -> Self {
        match self {
            Stmt::Assign(Assign { lval, exp }) => {
                let id = lval.id; // 直接获取 id 的所有权
                let dimensions = lval.dimensions; // 获取 dimensions 的所有权

                let entry = symtable.lookup(&id).expect("variable not found");

                // 类型检查索引表达式
                let processed_dimensions: Vec<Exp> = dimensions
                    .into_iter()
                    .map(|index| index.type_check(Some(&Type::int()), symtable))
                    .collect();

                // 计算最终类型（例如处理数组维度）
                let mut typ = &entry.typ;
                for _ in 0..processed_dimensions.len() {
                    typ = typ.inner_ty().unwrap();
                }

                // 类型检查表达式
                let expr = exp.type_check(Some(typ), symtable);

                // 重构 Assign 结构体
                let assign = Stmt::Assign(Assign {
                    lval: LVal {
                        id,                               // 使用已解构的 id
                        dimensions: processed_dimensions, // 使用处理后的 dimensions
                    },
                    exp: expr,
                });
                assign
            }
            Stmt::ExpStmt(ExpStmt { exp }) => {
                let expr = exp.map(|expr| expr.type_check(None, symtable));
                Stmt::ExpStmt(ExpStmt { exp: expr })
            }
            Stmt::Block(mut block) => {
                block.type_check(symtable);
                Stmt::Block(block)
            }
            Stmt::Break(Break) => Stmt::Break(Break),
            Stmt::Continue(Continue) => Stmt::Continue(Continue),
            Stmt::Return(Return { exp }) => {
                let exp = exp.map(|expr| expr.type_check(symtable.curr_ret_ty.as_ref(), symtable));

                if exp.is_none() {
                    return Stmt::Return(Return { exp });
                }

                let mut exp = exp.unwrap();
                let ret_ty = symtable.curr_ret_ty.as_ref().unwrap();

                if ret_ty.is_float() {
                    exp = Exp::coercion(&exp, Type::float());
                } else if ret_ty.is_int() {
                    exp = Exp::coercion(&exp, Type::int());
                } else {
                    panic!("unsupported return type");
                }

                Stmt::Return(Return { exp: Some(exp) })
            }
            Stmt::If(block) => {
                let cond = block.cond.type_check(Some(&Type::bool()), symtable);
                let then_block = block.then.type_check(symtable);
                let else_block = block.else_then.map(|block| block.type_check(symtable));
                Stmt::If(Box::new(If {
                    cond,
                    then: Box::new(then_block),
                    else_then: else_block.map(Box::new),
                }))
            }
            Stmt::While(pre) => {
                let cond = pre.cond.type_check(Some(&Type::bool()), symtable);
                let block = pre.block.type_check(symtable);
                Stmt::While(Box::new(While {
                    cond,
                    block: Box::new(block),
                }))
            }
        }
    }
}

impl Exp {
    /// 创建完整的零初始化器（支持嵌套数组）
    fn create_zero_initializer(typ: &Type, symtable: &SymbolTable) -> Self {
        fn build_array_zero(elem_ty: &Type, len: usize) -> Exp {
            let elements = (0..len)
                .map(|_| {
                    if elem_ty.is_array() {
                        let (inner_elem_ty, inner_len) = elem_ty.unwrap_array();
                        build_array_zero(inner_elem_ty, inner_len)
                    } else {
                        Exp::create_zero_value(elem_ty)
                    }
                })
                .collect();

            let mut exp = Exp::init_list(elements);
            exp.typ = Some(Type::array(elem_ty.clone(), len));
            exp
        }

        match typ.kind() {
            TypeKind::Int | TypeKind::Float | TypeKind::Bool => Exp::create_zero_value(typ),

            TypeKind::Array(elem_ty, arr_len) => {
                let zero_array = build_array_zero(elem_ty, *arr_len);
                zero_array
                    .try_fold(symtable)
                    .map(Exp::const_)
                    .unwrap_or_else(|| {
                        panic!("Failed to fold array zero initializer for type {:?}", typ)
                    })
            }

            _ => panic!("Unsupported zero type: {:?}", typ),
        }
    }

    /// 检查是否为常量零值
    fn is_const_zero(&self) -> bool {
        match &self.kind {
            ExpKind::Const(val) => val.is_zero(),
            _ => false,
        }
    }

    /// 检查是否为初始化列表
    fn is_init_list(&self) -> bool {
        matches!(&self.kind, ExpKind::InitList(_))
    }

    /// 类型检查表达式，返回类型正确的表达式。
    pub fn get_type(&self) -> &Type {
        self.typ.as_ref().unwrap()
    }

    /// 标准化数组初始值列表
    pub fn canonicalize_init_list(&mut self, typ: &Type, symtable: &SymbolTable) {
        if let ExpKind::InitList(ref mut vals) = self.kind.clone() {
            let (sub_ty, len) = typ.unwrap_array();
            let base_ty = typ.array_base().clone();

            // 处理一维数组情况
            if &base_ty == sub_ty {
                self.handle_1d_array(typ, &base_ty, len, vals, symtable);
            }
            // 处理多维数组情况
            else {
                self.handle_nd_array(typ, sub_ty, &base_ty, len, vals, symtable);
            }
        } else {
            panic!("not an init list");
        }
    }

    // 处理一维数组的规范化
    fn handle_1d_array(
        &mut self,
        typ: &Type,
        base_ty: &Type,
        len: usize,
        vals: &mut Vec<Exp>,
        symtable: &SymbolTable,
    ) {
        let mut new_vals = Vec::new();

        // 处理原始值列表
        for val in vals.drain(..) {
            let mut val = val.type_check(Some(base_ty), symtable);
            if let Some(folded) = val.try_fold(symtable) {
                val = Exp::const_(folded);
            }
            new_vals.push(val);
        }

        // 填充缺失的值
        if new_vals.len() < len {
            new_vals.resize_with(len, || Exp::create_zero_value(base_ty));
        }

        // 检查是否全零
        if new_vals.iter().all(|val| val.is_const_zero()) {
            *self = Exp::create_zero_initializer(typ, symtable);
        } else {
            *self = Exp::init_list(new_vals);
            self.typ = Some(typ.clone());
        }
    }

    // 处理多维数组的规范化
    fn handle_nd_array(
        &mut self,
        typ: &Type,
        sub_ty: &Type,
        base_ty: &Type,
        len: usize,
        vals: &mut Vec<Exp>,
        symtable: &SymbolTable,
    ) {
        let elem_total_len = sub_ty.bytewidth() / base_ty.bytewidth();
        let mut elem_init_list = Vec::new();
        let mut new_init_list = Vec::new();

        for mut val in vals.drain(..) {
            if val.is_init_list() {
                if !elem_init_list.is_empty() {
                    elem_init_list.push(val);
                } else {
                    val.canonicalize_init_list(sub_ty, symtable);
                    new_init_list.push(val);
                }
            } else {
                elem_init_list.push(val);
                if elem_init_list.len() == elem_total_len {
                    let mut init_list = Exp::init_list(elem_init_list);
                    init_list.canonicalize_init_list(sub_ty, symtable);
                    new_init_list.push(init_list);
                    elem_init_list = Vec::new();
                }
            }
        }

        // 处理剩余元素
        if !elem_init_list.is_empty() {
            let mut init_list = Exp::init_list(elem_init_list);
            init_list.canonicalize_init_list(sub_ty, symtable);
            new_init_list.push(init_list);
        }

        // 填充缺失的子数组
        if new_init_list.len() < len {
            new_init_list.resize_with(len, || {
                let mut exp = Exp::init_list(Vec::new());
                exp.canonicalize_init_list(sub_ty, symtable);
                exp
            });
        }

        *self = Exp::init_list(new_init_list);
        self.typ = Some(typ.clone());
    }

    /// 创建对应类型的零值常量
    fn create_zero_value(ty: &Type) -> Self {
        match ty.kind() {
            TypeKind::Int => Exp::const_(ComptimeValue::int(0)),
            TypeKind::Float => Exp::const_(ComptimeValue::float(0.0)),
            TypeKind::Bool => Exp::const_(ComptimeValue::bool(false)),
            _ => panic!("Unsupported zero type: {:?}", ty),
        }
    }

    /// 常量折叠
    pub fn try_fold(&self, symtable: &SymbolTable) -> Option<ComptimeValue> {
        match &self.kind {
            ExpKind::Const(val) => Some(val.clone()),
            ExpKind::Binary(op, lhs, rhs) => {
                let lhs = lhs.try_fold(symtable)?;
                let rhs = rhs.try_fold(symtable)?;
                match op {
                    BinaryOp::Add => Some(lhs + rhs),
                    BinaryOp::Sub => Some(lhs - rhs),
                    BinaryOp::Mul => Some(lhs * rhs),
                    BinaryOp::Div => Some(lhs / rhs),
                    BinaryOp::Mod => Some(lhs % rhs),
                    BinaryOp::Lt => Some(ComptimeValue::Bool(lhs < rhs)),
                    BinaryOp::Gt => Some(ComptimeValue::Bool(lhs > rhs)),
                    BinaryOp::Le => Some(ComptimeValue::Bool(lhs <= rhs)),
                    BinaryOp::Ge => Some(ComptimeValue::Bool(lhs >= rhs)),
                    BinaryOp::Eq => Some(ComptimeValue::Bool(lhs == rhs)),
                    BinaryOp::Ne => Some(ComptimeValue::Bool(lhs != rhs)),
                    BinaryOp::And => Some(lhs.logical_and(&rhs)),
                    BinaryOp::Or => Some(lhs.logical_or(&rhs)),
                }
            }
            ExpKind::Unary(op, expr) => {
                let expr = expr.try_fold(symtable)?;
                match op {
                    UnaryOp::Neg => Some(-expr),
                    UnaryOp::Not => Some(!expr),
                }
            }
            ExpKind::FuncCall(_) => None,
            ExpKind::LVal(LVal { id, dimensions }) => {
                let entry = symtable.lookup(id).unwrap();
                let val = entry.comptime.as_ref()?;
                let mut folded_indices = Vec::new();

                for index in dimensions {
                    let index = match index.try_fold(symtable)? {
                        ComptimeValue::Int(i) => i,
                        ComptimeValue::Float(f) => f as i32,
                        ComptimeValue::Bool(b) => b as i32,
                        ComptimeValue::List(_)
                        | ComptimeValue::Zeros(_)
                        | ComptimeValue::Undef(_) => return None,
                    };
                    folded_indices.push(index);
                }

                let val = if let ComptimeValue::List(_) = val {
                    let mut val = val.clone();
                    for index in folded_indices {
                        if let ComptimeValue::List(list) = val {
                            val = list[index as usize].clone();
                        } else {
                            return None;
                        }
                    }
                    val
                } else {
                    val.clone()
                };
                Some(val)
            }
            ExpKind::InitList(vals) => {
                let vals = vals
                    .iter()
                    .map(|val| val.try_fold(symtable))
                    .collect::<Option<Vec<_>>>()?;

                Some(ComptimeValue::list(vals))
            }
            ExpKind::Coercion(expr) => {
                let expr = expr.try_fold(symtable)?;
                match self.typ.as_ref().unwrap().kind() {
                    TypeKind::Bool => {
                        let expr = match expr {
                            ComptimeValue::Bool(val) => val,
                            ComptimeValue::Int(val) => val != 0,
                            ComptimeValue::Float(val) => val != 0.0,
                            ComptimeValue::List(_)
                            | ComptimeValue::Zeros(_)
                            | ComptimeValue::Undef(_) => {
                                panic!("unsupported type coercion: {:?}", expr)
                            }
                        };
                        Some(ComptimeValue::bool(expr))
                    }
                    TypeKind::Int => {
                        let expr = match expr {
                            ComptimeValue::Bool(val) => val as i32,
                            ComptimeValue::Int(val) => val,
                            ComptimeValue::Float(val) => val as i32,
                            ComptimeValue::List(_)
                            | ComptimeValue::Zeros(_)
                            | ComptimeValue::Undef(_) => {
                                panic!("unsupported type coercion: {:?}", expr)
                            }
                        };
                        Some(ComptimeValue::int(expr))
                    }
                    TypeKind::Float => {
                        let expr = match expr {
                            ComptimeValue::Bool(val) => val as i32 as f32,
                            ComptimeValue::Int(val) => val as f32,
                            ComptimeValue::Float(val) => val,
                            ComptimeValue::List(_)
                            | ComptimeValue::Zeros(_)
                            | ComptimeValue::Undef(_) => {
                                panic!("unsupported type coercion: {:?}", expr)
                            }
                        };
                        Some(ComptimeValue::float(expr))
                    }
                    TypeKind::Void
                    | TypeKind::Ptr(_)
                    | TypeKind::Array(..)
                    | TypeKind::Function(..) => {
                        panic!("unsupported type coercion")
                    }
                }
            }
        }
    }

    pub fn type_check(mut self, expect: Option<&Type>, symtable: &SymbolTable) -> Self {
        if self.typ.is_some() && expect.is_none() {
            return self;
        }
        let mut expr = match self.kind {
            ExpKind::Const(_) => self,
            ExpKind::Binary(op, lhs, rhs) => {
                let mut lhs = lhs.type_check(None, symtable);
                let mut rhs = rhs.type_check(None, symtable);
                let lhs_ty = lhs.get_type();
                let rhs_ty = rhs.get_type();

                match (lhs_ty.kind(), rhs_ty.kind()) {
                    (TypeKind::Int, TypeKind::Int) => match op {
                        BinaryOp::And | BinaryOp::Or => {
                            lhs = Exp::coercion(&lhs, Type::bool());
                            rhs = Exp::coercion(&rhs, Type::bool());
                        }
                        _ => {}
                    },
                    (TypeKind::Float, TypeKind::Float) => match op {
                        BinaryOp::And | BinaryOp::Or => {
                            lhs = Exp::coercion(&lhs, Type::bool());
                            rhs = Exp::coercion(&rhs, Type::bool());
                        }
                        _ => {}
                    },
                    (TypeKind::Bool, TypeKind::Int) => match op {
                        BinaryOp::And | BinaryOp::Or => {
                            rhs = Exp::coercion(&rhs, Type::bool());
                        }
                        _ => {
                            lhs = Exp::coercion(&lhs, Type::int());
                        }
                    },
                    (TypeKind::Bool, TypeKind::Float) => match op {
                        BinaryOp::And | BinaryOp::Or => {
                            rhs = Exp::coercion(&rhs, Type::bool());
                        }
                        _ => {
                            let tmp = Exp::coercion(&lhs, Type::int());
                            lhs = Exp::coercion(&tmp, Type::float());
                        }
                    },
                    (TypeKind::Int, TypeKind::Bool) => match op {
                        BinaryOp::And | BinaryOp::Or => {
                            lhs = Exp::coercion(&lhs, Type::bool());
                        }
                        _ => {
                            rhs = Exp::coercion(&rhs, Type::int());
                        }
                    },
                    (TypeKind::Int, TypeKind::Float) => match op {
                        BinaryOp::And | BinaryOp::Or => {
                            rhs = Exp::coercion(&rhs, Type::bool());
                            lhs = Exp::coercion(&lhs, Type::bool());
                        }
                        _ => {
                            lhs = Exp::coercion(&lhs, Type::float());
                        }
                    },
                    (TypeKind::Float, TypeKind::Bool) => match op {
                        BinaryOp::And | BinaryOp::Or => {
                            lhs = Exp::coercion(&lhs, Type::bool());
                        }
                        _ => {
                            let tmp = Exp::coercion(&rhs, Type::int());
                            rhs = Exp::coercion(&tmp, Type::float());
                        }
                    },
                    (TypeKind::Float, TypeKind::Int) => match op {
                        BinaryOp::And | BinaryOp::Or => {
                            lhs = Exp::coercion(&lhs, Type::bool());
                            rhs = Exp::coercion(&rhs, Type::bool());
                        }
                        _ => {
                            rhs = Exp::coercion(&rhs, Type::float());
                        }
                    },
                    _ => {
                        if lhs_ty != rhs_ty {
                            panic!("unsupported type coercion: {:?} -> {:?}", lhs_ty, rhs_ty);
                        }
                    }
                }

                let lhs_ty = lhs.get_type().clone();
                let mut expr = Exp::binary(op, lhs, rhs);

                match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod => {
                        expr.typ = Some(lhs_ty.clone());
                    }
                    BinaryOp::Lt
                    | BinaryOp::Gt
                    | BinaryOp::Le
                    | BinaryOp::Ge
                    | BinaryOp::Eq
                    | BinaryOp::Ne => {
                        expr.typ = Some(Type::bool());
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        expr.typ = Some(Type::bool());
                    }
                }
                expr
            }
            ExpKind::Coercion(_) => unreachable!(),
            ExpKind::FuncCall(FuncCall { id, args }) => {
                let entry = symtable.lookup(&id).unwrap();

                let (param_tys, ret_ty) = entry.typ.unwrap_func();
                let args = args
                    .into_iter()
                    .zip(param_tys)
                    .map(|(arg, typ)| {
                        // println!(
                        //     "ExpKind:: Funcion {} FuncCall: arg: {:?}, typ: {:?}",
                        //     id, arg, typ
                        // );
                        arg.type_check(Some(&typ), symtable)
                    })
                    .collect();

                let mut expr = Exp::func_call(id, args);
                expr.typ = Some(ret_ty.clone());
                expr
            }
            ExpKind::InitList(ref list) => {
                if list.is_empty() {
                    let typ = expect.unwrap();
                    let val = ComptimeValue::zeros(typ.clone());
                    let expr = Exp::const_(val);
                    return expr;
                }

                self.canonicalize_init_list(expect.unwrap(), symtable);
                self
            }
            ExpKind::LVal(LVal { id, dimensions }) => {
                let entry = symtable.lookup(&id).unwrap();

                let indices: Vec<Exp> = dimensions
                    .into_iter()
                    .map(|index| index.type_check(Some(&Type::int()), symtable))
                    .collect();
                let mut typ = entry.typ.clone();
                for _ in 0..indices.len() {
                    typ = typ.inner_ty().unwrap().clone();
                }
                let mut expr = Exp::lval(LVal {
                    id,
                    dimensions: indices,
                });
                expr.typ = Some(typ);
                expr
            }
            ExpKind::Unary(op, expr) => {
                let mut expr = expr.type_check(None, symtable);
                let typ = match op {
                    UnaryOp::Neg => {
                        if expr.get_type().is_bool() {
                            // if this is bool, convert to int first
                            expr = Exp::coercion(&expr, Type::int());
                        }
                        let typ = expr.get_type();
                        if typ.is_int() || typ.is_float() {
                            typ.clone()
                        } else {
                            panic!("unsupported type for negation: {:?}", typ);
                        }
                    }
                    UnaryOp::Not => {
                        let typ = expr.get_type();
                        if typ.is_bool() {
                            // do nothing
                        } else if typ.is_int() {
                            let zero = Exp::const_(ComptimeValue::Int(0));
                            expr = Exp::binary(BinaryOp::Ne, expr, zero);
                            expr.typ = Some(Type::bool());
                        } else if typ.is_float() {
                            let zero = Exp::const_(ComptimeValue::Float(0.0));
                            expr = Exp::binary(BinaryOp::Ne, expr, zero);
                            expr.typ = Some(Type::bool());
                        } else {
                            panic!("unsupported type for logical not: {:?}", typ);
                        }
                        Type::bool()
                    }
                };

                let mut expr = Exp::unary(op, expr);
                expr.typ = Some(typ);
                expr
            }
        };

        if let Some(typ) = expect {
            if typ.is_float() || typ.is_int() || typ.is_bool() | typ.is_ptr() {
                match typ.kind() {
                    TypeKind::Bool => expr = Exp::coercion(&expr, Type::bool()),
                    TypeKind::Int => expr = Exp::coercion(&expr, Type::int()),
                    TypeKind::Float => expr = Exp::coercion(&expr, Type::float()),
                    TypeKind::Ptr(_) => expr = Exp::coercion(&expr, typ.clone()),
                    TypeKind::Array(..) | TypeKind::Function(..) | TypeKind::Void => {
                        unreachable!("");
                    }
                }
                expr.typ = Some(typ.clone());
            } else if typ.is_array() {
                let expr_dims = typ.array_dims();
                let exp_dims = expect.unwrap().array_dims();
                if expr_dims.len() != exp_dims.len() {
                    panic!(
                        "array dimension mismatch: {:?} vs. {:?}",
                        expr_dims, exp_dims
                    );
                }
            } else if typ != expr.get_type() {
                panic!("unsupported type coercion: {:?}", typ);
            }
        }
        match expr.kind {
            ExpKind::InitList(_) => {
                if let Some(comptime) = expr.try_fold(symtable) {
                    expr = Exp::const_(comptime);
                }
            }
            _ => {
                if let Some(comptime) = expr.try_fold(symtable) {
                    expr = Exp::const_(comptime);
                }
            }
        }
        expr
    }
}

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::vec;

use crate::{
    frontend::{
        ir::{
            defuse::{Useable, User},
            value::ValueKind,
        },
        ir2string::Display,
    },
    utils::{
        linked_list::{LinkedListContainer, LinkedListNode},
        storage::{Arena, ArenaPtr, GenericPtr},
    },
};

use super::{basicblock::BasicBlock, context::Context, typ::Typ, value::Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TerminatorOp {
    ///ret <type> <value>       ; Return a value from a non-void function
    ///ret void                 ; Return from void function
    Ret,
    /// br i1 <cond>, label <iftrue>, label <iffalse>		;条件跳转
    Icbr, //实际不存在
    Fcbr, //实际不存在
    /// br label <dest>          ;无条件跳转
    Br,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    // 符号位取反 <result> = fneg <Typ> <op1>   ; yields Typ:result
    Fneg,
    // 逻辑取反 <result> = not <Typ> <op1>   ; yields Typ:result
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    // <result> = add <Typ> <op1>, <op2>          ; yields Typ:result
    Add,
    // <result> = fadd <Typ> <op1>, <op2>   ; yields Typ:result
    Fadd,
    // 同Add
    Sub,
    // 同Fadd
    FSub,
    // 同Add
    Mul,
    // 同Fadd
    Fmul,
    // <result> = sdiv <Typ>           ; yields Typ:result
    Sdiv,
    // <result> = udiv <Typ> <op1>, <op2>         ; yields Typ:result
    Udiv,
    // 同Fadd
    Fdiv,
    // 同Fadd
    Urem,
    Srem,
    Frem,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BitBinaryOp {
    // <result> = and <Typ> <op1>, <op2>   ; yields Typ:result
    And,
    // <result> = or <Typ> <op1>, <op2>   ; yields Typ:result
    Or,
    // <result> = xor <Typ> <op1>, <op2>   ; yields Typ:result
    Xor,
    // <result> = shl <Typ> <op1>, <op2>           ; yields Typ:result
    Shl,
    // <result> = lshr <Typ> <op1>, <op2>         ; yields Typ:result
    Lshr,
    //<result> = ashr <Typ> <op1>, <op2>         ; yields Typ:result
    Ashr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MemAccessOp {
    // <result> = alloca <type>
    Alloca { typ: Typ },
    // store <Typ> <value>, <Typ>* <pointer>
    Store,
    // <<result> = load <Typ>, <Typ>* <pointer>
    Load,
    // <result> = getelementptr <Typ>, <Typ>* <ptrval>, [<typ> <idx>]
    // getelementptr指令来获得指向数组的元素和指向结构体成员的指针，所以这个指令产生的结果是一个指针。它只执行地址计算，不访问内存，该指令也可用于计算此类地址的向量
    GetElementPtr { typ: Typ },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConversionOp {
    // <result> = trunc <Typ> <value> to <ty2>             ; yields ty2
    // trunc...to...指令截取目标值的高位，使剩余的部分转换成目标类型
    //
    Trunc,
    // %result = zext <source type> <value> to <destination type>
    // zext指令将一个整数或布尔值的位数增加，新位数的高位都填充为零，即进行零扩展
    ZExt,
    // %result = sext <source type> <value> to <destination type>
    // sext指令将一个整数或布尔值的位数增加，新位数的高位都填充为符号位，即进行符号扩展
    SExt,
    //fptosi <source type> <value> to <destination type>
    // fptosi指令将浮点数转换为整数
    FpToSi,
    SiToFp,
    Bitcast,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ICompCond {
    // =
    Eq,
    // !=
    Ne,
    // 无符号数
    Ugt,
    Uge,
    Ult,
    Ule,
    // 有符号数
    Sgt,
    Sge,
    Slt,
    Sle,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FCompCond {
    False,
    True,
    // Both operands are not a QNAN
    Oeq,
    One,
    Ogt,
    Oge,
    Olt,
    Ole,
    Ord,
    // Either operand is a QNAN
    Ueq,
    Une,
    Ugt,
    Uge,
    Ult,
    Ule,
    Uno,
}

#[derive(Debug, Clone, Copy)]
pub enum InstructionKind {
    // Terminator instructions
    Terminator { op: TerminatorOp },
    // Unary instructions
    Unary { op: UnaryOp },
    // Binary instructions
    Binary { op: BinaryOp },
    // Bitwise binary instructions
    BitBinary { op: BitBinaryOp },
    // Memory access instructions
    MemAccess { op: MemAccessOp },
    // Conversion instructions
    Conversion { op: ConversionOp },
    // Integer comparison instructions
    // <result> = icmp <cond> <Typ> <op1>, <op2>   ; yields i1 or <N x i1>:result
    IComp { cond: ICompCond },
    // Floating-point comparison instructions
    // <result> = fcmp [fast-math flags]* <cond> <Typ> <op1>, <op2>     ; yields i1 or <N x i1>:result
    FComp { cond: FCompCond },
    // <result> = phi <Typ> [ <val0>, <label0>], ...
    // phi指令用于实现PHI节点。在运行时，phi指令根据在当前block之前执行的是哪一个predecessor(前导) block来得到相应的值。
    // phi 指令必须在basic block的最前面，也就是在一个basic block中，在phi 指令之前不允许有非phi指令，但是可以有多个phi指令
    Phi,
    // <result> =  call <Typ>|<fnty> <fnptrval>(<function args>)
    Call { tail_call: bool },
}

impl PartialEq for InstructionKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (InstructionKind::Terminator { op: op1 }, InstructionKind::Terminator { op: op2 }) => {
                op1 == op2
            }
            (InstructionKind::Unary { op: op1 }, InstructionKind::Unary { op: op2 }) => op1 == op2,
            (InstructionKind::Binary { op: op1 }, InstructionKind::Binary { op: op2 }) => {
                op1 == op2
            }
            (InstructionKind::BitBinary { op: op1 }, InstructionKind::BitBinary { op: op2 }) => {
                op1 == op2
            }
            (InstructionKind::MemAccess { op: op1 }, InstructionKind::MemAccess { op: op2 }) => {
                op1 == op2
            }
            (InstructionKind::Conversion { op: op1 }, InstructionKind::Conversion { op: op2 }) => {
                op1 == op2
            }
            (InstructionKind::IComp { cond: cond1 }, InstructionKind::IComp { cond: cond2 }) => {
                cond1 == cond2
            }
            (InstructionKind::FComp { cond: cond1 }, InstructionKind::FComp { cond: cond2 }) => {
                cond1 == cond2
            }
            (InstructionKind::Phi, InstructionKind::Phi) => true,
            (
                InstructionKind::Call {
                    tail_call: _tail_call1,
                },
                InstructionKind::Call {
                    tail_call: _tail_call2,
                },
            ) => true,
            _ => false,
        }
    }
}

pub struct InstructionData {
    self_ptr: Instruction,
    kind: InstructionKind,
    operands: Vec<(Value, bool)>,
    operands_bbk: Vec<(BasicBlock, bool)>,
    phi: HashMap<BasicBlock, usize>, // phi 指令前驱基本块到操作数的映射
    result: Option<Value>,
    succ: Option<Instruction>,
    pre: Option<Instruction>,
    basicblock: Option<BasicBlock>,
}

impl InstructionData {
    pub fn get_self_ptr(&self) -> Instruction {
        self.self_ptr
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub struct Instruction(pub GenericPtr<InstructionData>);

impl Instruction {
    /// 创建一个还未指定操作数的指令
    fn new_without_operands(ctx: &mut Context, kind: InstructionKind, typ: Typ) -> Self {
        let instruction = ctx.alloc_with(|self_ptr| InstructionData {
            self_ptr: self_ptr,
            kind,
            operands: vec![],
            operands_bbk: vec![],
            phi: HashMap::default(),
            result: None,
            succ: None,
            pre: None,
            basicblock: None,
        });

        if !typ.is_void(ctx) {
            // if typ.is_array(ctx) && kind == InstructionKind::MemAccess{ op: Getelementptr }{
            // };
            let result = Value::inst_result(ctx, instruction, typ);
            instruction
                .deref_mut(ctx)
                .expect("Failed to deref `instructions` in struct `Context`")
                .result = Some(result);
            result.insert(ctx, User::new(instruction, 0)); // 默认插入到第0个位置
            instruction
        } else {
            instruction
        }
    }

    /// 创建一个指定操作数的指令
    pub fn new_with_operands(
        ctx: &mut Context,
        kind: InstructionKind,
        typ: Typ,
        operands: Vec<Value>,
    ) -> Self {
        let instruction = Self::new_without_operands(ctx, kind, typ);
        let instruction_data = instruction
            .deref_mut(ctx)
            .expect("Failed to deref `instructions` in struct `Context`");
        instruction_data.operands = operands.into_iter().map(|op| (op, true)).collect();
        instruction
    }

    /// 为某个result创建load指令
    pub fn load_with_result(ctx: &mut Context, result: Value, pointer: Value) -> Self {
        if result.kind(ctx) != pointer.kind(ctx) {
            panic!("Type of result and pointer should be the same");
        }
        let instruction = ctx.alloc_with(|self_ptr| InstructionData {
            self_ptr: self_ptr,
            kind: InstructionKind::MemAccess {
                op: MemAccessOp::Load,
            },
            operands: vec![(pointer, true)],
            operands_bbk: vec![],
            phi: HashMap::default(),
            result: Some(result),
            succ: None,
            pre: None,
            basicblock: None,
        });
        result.insert(ctx, User::new(instruction, 0));
        pointer.insert(ctx, User::new(instruction, 1));
        result.deref_mut(ctx).unwrap().kind = ValueKind::InstResult {
            instruction: instruction,
            typ: result.kind(ctx),
        };
        instruction
    }

    /// 获取指定index位置的操作数
    pub fn get_operand(self, ctx: &Context, index: usize) -> Option<Value> {
        // if self.is_phi(ctx) {
        //     panic!("Phi instruction cannot get operand using this method");
        // }
        if index
            >= self
                .deref(ctx)
                .expect("Failed to deref `operands` in struct `Instruction`")
                .operands
                .len()
        {
            return None;
        }
        self.deref(ctx)
            .expect("Failed to deref `operands` in struct `Instruction`")
            .operands
            .get(index)
            .map(|(value, _)| *value)
    }

    /// 获取所有操作数
    pub fn get_operands(self, ctx: &Context) -> Vec<Value> {
        self.deref(ctx)
            .expect("Failed to deref `operands` in struct `Instruction`")
            .operands
            .iter()
            .map(|(value, _)| *value)
            .collect()
    }

    /// 获取指令操作数个数
    pub fn operand_count(self, ctx: &Context) -> usize {
        self.deref(ctx)
            .expect("Failed to deref `operands` in struct `Instruction`")
            .operands
            .len()
    }

    /// 获取跳转指令指定index的基本块
    pub fn get_operand_bbk(self, ctx: &Context, index: usize) -> Option<BasicBlock> {
        if index
            >= self
                .deref(ctx)
                .expect("Failed to deref `operands_bbk` in struct `Instruction`")
                .operands_bbk
                .len()
        {
            return None;
        }

        self.deref(ctx)
            .expect("Failed to deref `operands_bbk` in struct `Instruction`")
            .operands_bbk
            .get(index)
            .map(|(value, _)| *value)
    }

    /// 判断两个指令是否属于同一个basic block
    pub fn has_same_bbk(self, ctx: &Context, other: Self) -> bool {
        self.get_basicblock(ctx) == other.get_basicblock(ctx)
    }

    /// 判断self是不是在other之前
    pub fn is_before(self, ctx: &Context, other: Self) -> bool {
        let mut current = self.succ(ctx);
        while current.is_some() {
            if current.unwrap() == other {
                return true;
            }
            current = current.unwrap().succ(ctx);
        }
        false
    }

    /// 获取指令类型
    pub fn get_kind(&self, ctx: &Context) -> InstructionKind {
        self.deref(ctx)
            .expect("Failed to deref `kind` in struct `Instruction`")
            .kind
    }

    /// 获取指令的运算结果
    pub fn get_result(&self, ctx: &Context) -> Option<Value> {
        self.deref(ctx)
            .expect("Failed to deref `result` in struct `Instruction`")
            .result
    }

    /// 获取指令结果的类型
    pub fn result_typ(&self, ctx: &Context) -> Typ {
        self.get_result(ctx)
            .expect("Failed to get result from instruction")
            .kind(ctx)
    }

    /// 获取指令所在的基本块
    pub fn get_basicblock(&self, ctx: &Context) -> Option<BasicBlock> {
        self.deref(ctx)
            .expect("Failed to deref `basic_block` in struct `Instruction`")
            .basicblock
    }

    /// 判断指令是否是terminator指令
    pub fn is_terminater(&self, ctx: &Context) -> bool {
        matches!(self.get_kind(ctx), InstructionKind::Terminator { .. })
    }

    /// 为分支指令增加一个基本块
    fn add_operand_bbk(self, ctx: &mut Context, block: BasicBlock) {
        self.deref_mut(ctx)
            .expect("Failed to deref_mut `operand_bbk` in struct `Instruction`")
            .operands_bbk
            .push((block, true));
    }

    /// 获取分支指令的基本块操作数
    pub fn get_operand_bbks(self, ctx: &Context) -> Vec<BasicBlock> {
        self.deref(ctx)
            .expect("Failed to deref `operand_bbk` in struct `Instruction`")
            .operands_bbk
            .iter()
            .map(|(bbk, _)| *bbk)
            .collect()
    }

    /// 替换指令的所有使用者为指定value
    pub fn replace_all_uses_with(self, ctx: &mut Context, value: Value) -> i32 {
        let mut num_replaced = 0;
        let result = self.get_result(ctx);
        if let Some(result) = result {
            let users: Vec<User<Value>> = result.users(ctx).into_iter().collect();
            for user in users {
                let instruction = user.get_instruction();
                if instruction == self {
                    continue;
                }
                let index = user.get_index();
                if let Some(old_value) = instruction.get_operand(ctx, index - 1) {
                    <Value as Useable>::remove(old_value, ctx, user);
                    instruction.replace_operand(ctx, index - 1, value);
                    <Value as Useable>::insert(value, ctx, User::new(instruction, index));
                } else {
                    panic!("Error: Index out of bounds when replacing operand in instruction");
                }
                num_replaced += 1;
            }
        }
        num_replaced
    }

    /// 将指定位置的操作数替换为指定value
    pub fn replace_operand(self, ctx: &mut Context, index: usize, value: Value) {
        if index >= self.operand_count(ctx) {
            eprintln!("Error: Index out of bounds when replacing operand in instruction");
            println!(
                "Instruction: { }, Index: {}, Value: { }",
                self.display(ctx),
                index,
                value.display(ctx)
            );
            return;
        }
        value.insert(ctx, User::new(self, index + 1));
        self.deref_mut(ctx).unwrap().operands[index] = (value, true);
    }

    /// 替换指定位置操作数并移除
    pub fn replace_operand_with_remove(self, ctx: &mut Context, index: usize, value: Value) {
        if index >= self.operand_count(ctx) {
            eprintln!("Error: Index out of bounds when replacing operand in instruction");
            println!(
                "Instruction: { }, Index: {}, Value: { }",
                self.display(ctx),
                index,
                value.display(ctx)
            );
            return;
        }
        let old_value = self.get_operand(ctx, index).unwrap();
        old_value.remove(ctx, self);
        value.insert(ctx, User::new(self, index + 1));
        self.deref_mut(ctx).unwrap().operands[index] = (value, true);
    }

    /// 将指定位置的操作数替换为指定BasicBlock
    pub fn replace_operand_bbk(self, ctx: &mut Context, index: usize, block: BasicBlock) {
        if index
            >= self
                .deref(ctx)
                .expect("Failed to deref `operands_bbk` in struct `Instruction`")
                .operands_bbk
                .len()
        {
            eprintln!("Error: Index out of bounds when replacing operand_bbk in instruction");
            println!(
                "Instruction: { }, Index: {}, BasicBlock: { }",
                self.display(ctx),
                index,
                block.display(ctx)
            );
            return;
        }
        let old_bbk = self.get_operand_bbk(ctx, index).unwrap();
        let users = old_bbk
            .users(ctx)
            .into_iter()
            .collect::<Vec<User<BasicBlock>>>();
        for user in users {
            if user.get_instruction() == self {
                <BasicBlock as Useable>::remove(old_bbk, ctx, user);
                break;
            }
        }
        block.insert(ctx, User::new(self, index + 1));
        self.deref_mut(ctx)
            .expect("Failed to deref_mut `operands_bbk` in struct `Instruction`")
            .operands_bbk[index] = (block, true);
    }

    /// 创建ret指令
    pub fn ret(ctx: &mut Context, value: Option<Value>) -> Self {
        let typ = Typ::void(ctx);
        if value.is_none() {
            return Self::new_without_operands(
                ctx,
                InstructionKind::Terminator {
                    op: TerminatorOp::Ret,
                },
                typ,
            );
        }
        let instruction = Self::new_with_operands(
            ctx,
            InstructionKind::Terminator {
                op: TerminatorOp::Ret,
            },
            typ,
            vec![value.unwrap()],
        );
        value.unwrap().insert(ctx, User::new(instruction, 1));
        instruction
    }

    /// 判断是否是ret指令
    pub fn is_ret(&self, ctx: &Context) -> bool {
        matches!(
            self.get_kind(ctx),
            InstructionKind::Terminator {
                op: TerminatorOp::Ret
            }
        )
    }

    /// 创建条件分支指令
    pub fn icbr(ctx: &mut Context, cond: Value, iftrue: BasicBlock, iffalse: BasicBlock) -> Self {
        if cond.is_constant(ctx) {
            let operand = cond.get_bool_const_value(ctx).unwrap();
            if operand {
                let instruction = Self::br(ctx, iftrue);
                iftrue.insert(ctx, User::new(instruction, 1));
                return instruction;
            } else {
                let instruction = Self::br(ctx, iffalse);
                iffalse.insert(ctx, User::new(instruction, 1));
                return instruction;
            }
        }
        let typ = Typ::void(ctx);
        let instruction = Self::new_with_operands(
            ctx,
            InstructionKind::Terminator {
                op: TerminatorOp::Icbr,
            },
            typ,
            vec![cond],
        );
        instruction.add_operand_bbk(ctx, iftrue);
        instruction.add_operand_bbk(ctx, iffalse);
        iftrue.insert(ctx, User::new(instruction, 1));
        iffalse.insert(ctx, User::new(instruction, 2));
        cond.insert(ctx, User::new(instruction, 1));
        instruction
    }

    /// 判断是否是条件分支指令
    pub fn is_cbr(&self, ctx: &Context) -> bool {
        matches!(
            self.get_kind(ctx),
            InstructionKind::Terminator {
                op: TerminatorOp::Icbr { .. } | TerminatorOp::Fcbr { .. }
            }
        )
    }

    /// 创建无条件跳转指令
    pub fn br(ctx: &mut Context, dest: BasicBlock) -> Self {
        let typ = Typ::void(ctx);
        let instruction = Self::new_with_operands(
            ctx,
            InstructionKind::Terminator {
                op: TerminatorOp::Br,
            },
            typ,
            vec![],
        );
        instruction.add_operand_bbk(ctx, dest);
        dest.insert(ctx, User::new(instruction, 1));
        instruction
    }

    /// 判断是否是分支指令
    pub fn is_br(&self, ctx: &Context) -> bool {
        matches!(
            self.get_kind(ctx),
            InstructionKind::Terminator {
                op: TerminatorOp::Br
            }
        )
    }

    /// 创建取相反数反指令
    pub fn fneg(ctx: &mut Context, typ: Typ, op1: Value) -> Self {
        let instruction = Self::new_with_operands(
            ctx,
            InstructionKind::Unary { op: UnaryOp::Fneg },
            typ,
            vec![op1],
        );
        op1.insert(ctx, User::new(instruction, 1));
        instruction
    }

    /// 创建取反指令
    pub fn not(ctx: &mut Context, typ: Typ, op1: Value) -> Self {
        let instruction = Self::new_with_operands(
            ctx,
            InstructionKind::Unary { op: UnaryOp::Not },
            typ,
            vec![op1],
        );
        op1.insert(ctx, User::new(instruction, 1));
        instruction
    }

    /// 创建双目运算符
    pub fn binary(ctx: &mut Context, op: BinaryOp, typ: Typ, op1: Value, op2: Value) -> Self {
        let instruction =
            Self::new_with_operands(ctx, InstructionKind::Binary { op }, typ, vec![op1, op2]);
        op1.insert(ctx, User::new(instruction, 1));
        op2.insert(ctx, User::new(instruction, 2));
        instruction
    }

    /// 创建位运算符
    pub fn bitbinary(ctx: &mut Context, op: BitBinaryOp, typ: Typ, op1: Value, op2: Value) -> Self {
        let instruction =
            Self::new_with_operands(ctx, InstructionKind::BitBinary { op }, typ, vec![op1, op2]);
        op1.insert(ctx, User::new(instruction, 1));
        op2.insert(ctx, User::new(instruction, 2));
        instruction
    }

    /// 创建访存指令
    pub fn memaccess(ctx: &mut Context, op: MemAccessOp, typ: Typ, ops: Vec<Value>) -> Self {
        match op {
            MemAccessOp::Store => {
                let void = Typ::void(ctx);
                let instruction = Self::new_with_operands(
                    ctx,
                    InstructionKind::MemAccess { op },
                    void,
                    ops.clone(),
                );
                for (i, op) in ops.iter().enumerate() {
                    op.insert(ctx, User::new(instruction, i + 1));
                }
                instruction
            }
            _ => {
                let instruction = Self::new_with_operands(
                    ctx,
                    InstructionKind::MemAccess { op },
                    typ,
                    ops.clone(),
                );
                for (i, op) in ops.iter().enumerate() {
                    op.insert(ctx, User::new(instruction, i + 1));
                }
                instruction
            }
        }
    }

    /// 判断是否是store指令
    pub fn is_store(&self, ctx: &Context) -> bool {
        matches!(
            self.get_kind(ctx),
            InstructionKind::MemAccess {
                op: MemAccessOp::Store
            }
        )
    }

    /// 判断是否是load指令
    pub fn is_load(&self, ctx: &Context) -> bool {
        matches!(
            self.get_kind(ctx),
            InstructionKind::MemAccess {
                op: MemAccessOp::Load
            }
        )
    }

    /// 判断是否是alloca指令
    pub fn is_alloca(&self, ctx: &Context) -> bool {
        matches!(
            self.get_kind(ctx),
            InstructionKind::MemAccess {
                op: MemAccessOp::Alloca { .. }
            }
        )
    }

    /// 判断是否是getelementptr指令
    pub fn is_gep(&self, ctx: &Context) -> bool {
        matches!(
            self.get_kind(ctx),
            InstructionKind::MemAccess {
                op: MemAccessOp::GetElementPtr { .. }
            }
        )
    }

    /// 创建类型转化指令
    pub fn conversion(ctx: &mut Context, op: ConversionOp, dst_type: Typ, op1: Value) -> Self {
        let instruction =
            Self::new_with_operands(ctx, InstructionKind::Conversion { op }, dst_type, vec![op1]);
        op1.insert(ctx, User::new(instruction, 1));
        instruction
    }

    /// 判断是否是bitcast指令
    pub fn is_bitcast(&self, ctx: &Context) -> bool {
        matches!(
            self.get_kind(ctx),
            InstructionKind::Conversion {
                op: ConversionOp::Bitcast
            }
        )
    }

    /// 创建整数比较指令
    pub fn icmp(ctx: &mut Context, op: ICompCond, src_type: Typ, op1: Value, op2: Value) -> Self {
        let instruction = Self::new_with_operands(
            ctx,
            InstructionKind::IComp { cond: op },
            src_type,
            vec![op1, op2],
        );
        op1.insert(ctx, User::new(instruction, 1));
        op2.insert(ctx, User::new(instruction, 2));
        instruction
    }

    /// 创建浮点数比较指令
    pub fn fcmp(ctx: &mut Context, op: FCompCond, src_type: Typ, op1: Value, op2: Value) -> Self {
        let instruction = Self::new_with_operands(
            ctx,
            InstructionKind::FComp { cond: op },
            src_type,
            vec![op1, op2],
        );
        op1.insert(ctx, User::new(instruction, 1));
        op2.insert(ctx, User::new(instruction, 2));
        instruction
    }

    pub fn is_cmp(&self, ctx: &Context) -> bool {
        matches!(
            self.get_kind(ctx),
            InstructionKind::IComp { .. } | InstructionKind::FComp { .. }
        )
    }

    /// 创建一个phi指令
    pub fn phi(ctx: &mut Context, typ: Typ) -> Self {
        Self::new_without_operands(ctx, InstructionKind::Phi, typ)
    }

    /// 判断是否是phi指令
    pub fn is_phi(&self, ctx: &Context) -> bool {
        matches!(self.get_kind(ctx), InstructionKind::Phi)
    }

    /// 为phi指令增加一个（基本块，操作数）对
    pub fn insert_phi_operand(self, ctx: &mut Context, block: BasicBlock, value: Value) {
        assert!(
            matches!(
                self.deref(ctx)
                    .expect("Failed to deref `kind` in struct `Instruction`")
                    .kind,
                InstructionKind::Phi
            ),
            "not a phi node"
        );

        let next_index = self
            .deref_mut(ctx)
            .expect("Failed to deref `operands` in struct `Instruction`")
            .operands
            .len();
        self.deref_mut(ctx)
            .expect("Failed to deref `operands` in struct `Instruction`")
            .operands
            .push((value, true));
        value.insert(ctx, User::new(self, next_index + 1)); // +1 because the first operand is the result
        // Create the mapping from the predecessor block to the operand index.
        self.deref_mut(ctx)
            .expect("Failed to deref `phi` in struct `Instruction`")
            .phi
            .insert(block, next_index);
    }

    /// 删除phi指令的一个（基本块，操作数）对
    pub fn remove_phi_operand(self, ctx: &mut Context, block: BasicBlock) {
        assert!(
            matches!(
                self.deref(ctx)
                    .expect("Failed to deref `kind` in struct `Instruction`")
                    .kind,
                InstructionKind::Phi
            ),
            "not a phi node"
        );
        let index = self
            .deref_mut(ctx)
            .expect("Failed to deref `phi` in struct `Instruction`")
            .phi
            .remove(&block)
            .expect("Phi operand not found for block");
        self.deref_mut(ctx)
            .expect("Failed to deref `operands` in struct `Instruction`")
            .operands[index]
            .1 = false;
    }

    /// 替换phi指令的一个（基本块，操作数）对
    pub fn replace_phi_operand(
        self,
        ctx: &mut Context,
        from_block: BasicBlock,
        to_block: BasicBlock,
        to_value: Value,
    ) {
        assert!(
            matches!(
                self.deref(ctx)
                    .expect("Failed to deref `kind` in struct `Instruction`")
                    .kind,
                InstructionKind::Phi
            ),
            "not a phi node"
        );
        let index = self
            .deref_mut(ctx)
            .expect("Failed to deref `phi` in struct `Instruction`")
            .phi
            .get(&from_block)
            .expect("Phi operand not found for block")
            .clone();
        self.deref_mut(ctx)
            .expect("Failed to deref `operands` in struct `Instruction`")
            .phi
            .remove(&from_block);
        self.deref_mut(ctx)
            .expect("Failed to deref `operands` in struct `Instruction`")
            .phi
            .insert(to_block, index);
        let old_value = self.get_operand(ctx, index).unwrap();
        <Value as Useable>::remove(old_value, ctx, User::new(self, index + 1));
        self.deref_mut(ctx)
            .expect("Failed to deref `operands` in struct `Instruction`")
            .operands[index] = (to_value, true);
        to_value.insert(ctx, User::new(self, index + 1)); // +1 because the first operand is the result
    }

    /// 替换phi指令的一个基本块（用于连续基本块合并等）
    pub fn replace_phi_bbk(self, ctx: &mut Context, from_block: BasicBlock, to_block: BasicBlock) {
        assert!(
            matches!(
                self.deref(ctx)
                    .expect("Failed to deref `kind` in struct `Instruction`")
                    .kind,
                InstructionKind::Phi
            ),
            "not a phi node"
        );
        let index = self
            .deref_mut(ctx)
            .expect("Failed to deref `phi` in struct `Instruction`")
            .phi
            .get(&from_block)
            .expect("Phi operand not found for block")
            .clone();
        self.deref_mut(ctx)
            .expect("Failed to deref `operands` in struct `Instruction`")
            .phi
            .remove(&from_block);
        self.deref_mut(ctx)
            .expect("Failed to deref `operands` in struct `Instruction`")
            .phi
            .insert(to_block, index);
    }

    /// 判断phi指令是否有指定基本块的操作数
    pub fn phi_has_bbk(self, ctx: &Context, block: BasicBlock) -> bool {
        self.deref(ctx)
            .expect("Failed to deref `phi` in struct `Instruction`")
            .phi
            .contains_key(&block)
    }

    /// 获取phi指令的全部（基本块，操作数）对
    pub fn get_phi(self, ctx: &Context) -> HashMap<BasicBlock, Value> {
        self.deref(ctx)
            .expect("Failed to deref `phi` in struct `Instruction`")
            .phi
            .iter()
            .map(move |(&block, &idx)| (block, self.get_operand(ctx, idx).unwrap()))
            .collect()
    }

    /// 获取phi指令的指定基本块的操作数
    pub fn get_phi_operand(self, ctx: &Context, block: BasicBlock) -> Option<Value> {
        self.deref(ctx)
            .expect("Failed to deref `phi` in struct `Instruction`")
            .phi
            .get(&block)
            .map(|&idx| self.get_operand(ctx, idx).unwrap())
    }

    /// 创建call指令
    pub fn call(ctx: &mut Context, typ: Typ, func: String, args: Vec<Value>) -> Self {
        let func = Value::function(ctx, func, typ);
        let mut operands = vec![func];
        operands.extend(args);
        let instruction = Self::new_with_operands(
            ctx,
            InstructionKind::Call { tail_call: false },
            typ,
            operands.clone(),
        );
        for (i, op) in operands.iter().enumerate() {
            op.insert(ctx, User::new(instruction, i + 1));
        }
        instruction
    }

    /// 判断是否是一条call指令
    pub fn is_call(&self, ctx: &Context) -> bool {
        matches!(self.get_kind(ctx), InstructionKind::Call { .. })
    }

    /// 设置尾调用标志
    pub fn set_tail_call(&mut self, ctx: &mut Context, is_tail: bool) {
        if let InstructionKind::Call { ref mut tail_call } = self.deref_mut(ctx).unwrap().kind {
            *tail_call = is_tail;
        } else {
            panic!("not a call instruction");
        }
    }

    /// 判断一条指令是否有全局变量或参数指针作为操作数
    pub fn has_global_or_paramptr_operand(self, ctx: &Context) -> bool {
        self.get_operands(ctx)
            .iter()
            .any(|op| op.is_global_ptr(ctx) || (op.is_parameter(ctx) && op.is_ptr(ctx)))
    }

    /// 移除一条指令
    pub fn remove(self, ctx: &mut Context) {
        // println!("remove instruction: {}", self.display(ctx));
        let mut wait_to_remove = HashSet::default();
        for op in self.get_operands(ctx) {
            if wait_to_remove.contains(&op) {
                continue;
            }
            wait_to_remove.insert(op);
        }
        if let Some(res) = self.get_result(ctx) {
            if !wait_to_remove.contains(&res) {
                wait_to_remove.insert(res);
            }
        }
        // for op in &wait_to_remove {
        //     println!(
        //         "Try to remove {} of value: {} with {} users",
        //         self.display(ctx),
        //         op.display(ctx),
        //         op.users(ctx).into_iter().count()
        //     );
        // }
        for op in wait_to_remove {
            op.remove(ctx, self);
        }
        for bbk in self.get_operand_bbks(ctx) {
            if bbk.is_removed(ctx) {
                //不可达基本块删除时可能会出现基本块已经移除情况，比如循环依赖的基本块
                continue;
            }
            let bbk_users = bbk.users(ctx).into_iter().collect::<Vec<_>>();
            for bbk_user in bbk_users {
                if bbk_user.get_instruction() == self {
                    <BasicBlock as Useable>::remove(bbk, ctx, bbk_user);
                }
            }
        }
        self.unlink(ctx);
        ctx.dealloc(self);
    }

    /// 判断指令是否已经被移除
    pub fn is_removed(&self, ctx: &Context) -> bool {
        self.deref(ctx).is_none()
    }

    /// 只移除使用没有释放Value
    pub fn remove_only(self, ctx: &mut Context) {
        let mut wait_to_remove = HashSet::default();
        for op in self.get_operands(ctx) {
            if wait_to_remove.contains(&op) {
                continue;
            }
            wait_to_remove.insert(op);
        }
        if let Some(res) = self.get_result(ctx) {
            if !wait_to_remove.contains(&res) {
                wait_to_remove.insert(res);
            }
        }
        for op in wait_to_remove {
            let op_users = op.users(ctx).into_iter().collect::<Vec<_>>();
            for op_user in op_users {
                if op_user.get_instruction() == self {
                    <Value as Useable>::remove(op, ctx, op_user);
                }
            }
        }
        self.unlink(ctx);
        ctx.dealloc(self);
    }

    /// 判断指令是否是一个常量指令
    pub fn is_constant(&self, ctx: &Context) -> bool {
        match self.get_kind(ctx) {
            InstructionKind::Terminator { op: _ } => false,
            InstructionKind::Unary { op: _ } => self.get_operand(ctx, 0).unwrap().is_constant(ctx),
            InstructionKind::Binary { op: _ } => {
                self.get_operand(ctx, 0).unwrap().is_constant(ctx)
                    && self.get_operand(ctx, 1).unwrap().is_constant(ctx)
            }
            InstructionKind::BitBinary { op: _ } => {
                self.get_operand(ctx, 0).unwrap().is_constant(ctx)
                    && self.get_operand(ctx, 1).unwrap().is_constant(ctx)
            }
            InstructionKind::MemAccess { op } => {
                match op {
                    MemAccessOp::Load => false, //self.get_operand(ctx, 0).unwrap().is_constant(ctx),
                    MemAccessOp::Store => false, //self.get_operand(ctx, 0).unwrap().is_constant(ctx),
                    MemAccessOp::GetElementPtr { .. } => {
                        // for op in self.get_operands(ctx) {
                        //     if !op.is_constant(ctx) {
                        //         return false;
                        //     }
                        // }
                        // true
                        false
                    }
                    MemAccessOp::Alloca { .. } => false,
                }
            }
            InstructionKind::Conversion { op } => match op {
                ConversionOp::Bitcast => false,
                _ => self.get_operand(ctx, 0).unwrap().is_constant(ctx),
            },
            InstructionKind::IComp { cond: _ } => {
                self.get_operand(ctx, 0).unwrap().is_constant(ctx)
                    && self.get_operand(ctx, 1).unwrap().is_constant(ctx)
            }
            InstructionKind::FComp { cond: _ } => {
                self.get_operand(ctx, 0).unwrap().is_constant(ctx)
                    && self.get_operand(ctx, 1).unwrap().is_constant(ctx)
            }
            InstructionKind::Phi => {
                let typ = self.result_typ(ctx);
                let mut pre_value = None;
                for (_bbk, op) in self.get_phi(ctx).iter() {
                    if pre_value.is_none() {
                        pre_value = Some(op);
                    }
                    if op.is_constant(ctx) {
                        if typ.is_int(ctx) {
                            let cur = op.get_int_const_value(ctx).unwrap();
                            let pre = pre_value.unwrap().get_int_const_value(ctx).unwrap();
                            if cur != pre {
                                return false;
                            }
                        } else if typ.is_float(ctx) {
                            let cur = op.get_float_const_value(ctx).unwrap();
                            let pre = pre_value.unwrap().get_float_const_value(ctx).unwrap();
                            if cur != pre {
                                return false;
                            }
                        } else if typ.is_ptr(ctx) {
                            //TODO: 指针比较
                            return false;
                        } else {
                            panic!(
                                "In Instruction::is_constant, phi node's result type is not int or float"
                            );
                        }
                        pre_value = Some(op);
                    } else {
                        return false;
                    }
                }
                true
            }
            InstructionKind::Call { .. } => false,
        }
    }
}

impl ArenaPtr for Instruction {
    type Arena = Context;
    type Data = InstructionData;
}

impl LinkedListNode for Instruction {
    type Container = BasicBlock;
    type Ctx = Context;

    fn succ(self, ctx: &Self::Ctx) -> Option<Self> {
        self.deref(ctx)
            .expect("Failed to deref `succ` in struct `Instruction`")
            .succ
    }

    fn pre(self, ctx: &Self::Ctx) -> Option<Self> {
        self.deref(ctx)
            .expect("Failed to deref `pre` in struct `Instruction`")
            .pre
    }

    fn container(self, ctx: &Self::Ctx) -> Option<Self::Container> {
        self.deref(ctx)
            .expect("Failed to deref `container` in struct `Instruction")
            .basicblock
    }

    fn set_succ(self, ctx: &mut Self::Ctx, succ: Option<Self>) {
        self.deref_mut(ctx)
            .expect("Failed to deref `succ` in struct `Instruction`")
            .succ = succ;
    }

    fn set_pre(self, ctx: &mut Self::Ctx, pre: Option<Self>) {
        self.deref_mut(ctx)
            .expect("Failed to deref `pre` in struct `Instruction`")
            .pre = pre;
    }

    fn set_container(self, ctx: &mut Self::Ctx, container: Option<Self::Container>) {
        self.deref_mut(ctx)
            .expect("Failed to deref `container` in struct `Instruction`")
            .basicblock = container;
    }

    fn unlink(self, ctx: &mut Self::Ctx) {
        let pre = self.pre(ctx);
        let succ = self.succ(ctx);

        if let Some(pre) = pre {
            pre.set_succ(ctx, succ);
        }

        if let Some(succ) = succ {
            succ.set_pre(ctx, pre);
        }

        if let Some(container) = self.container(ctx) {
            if container.get_head(ctx) == Some(self) {
                container.set_head(ctx, succ);
            }

            if container.get_tail(ctx) == Some(self) {
                container.set_tail(ctx, pre);
            }
        }

        self.set_pre(ctx, None);

        self.set_succ(ctx, None);

        self.set_container(ctx, None);
    }
}

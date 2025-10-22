
use super::{
    mir_block::MirBlock,
    mir_context::MirContext,
    mir_label::MirLabel,
    mir_operand::MemLoc,
    regs::Reg,
};
use crate::{backend::{asm2string::Display}, utils::{
    linked_list::{LinkedListContainer, LinkedListError, LinkedListNode},
    storage::{Arena, ArenaPtr, GenericPtr},
}};

pub enum MirInstKind {
    // 二元指令
    Add { rd: Reg, rn: Reg, rm: Reg },  // 加法
    Addi { rd: Reg, rn: Reg, imm: i32 }, 
    Sub { rd: Reg, rn: Reg, rm: Reg },  // 减法
    Subi {rd: Reg, rn: Reg, imm: i32},
    Mul { rd: Reg, rn: Reg, rm: Reg },  // 乘法
    Div { rd: Reg, rn: Reg, rm: Reg },  // 除法
    Cmp { rn: Reg, rm: Reg, sign: Reg}, // 比较指令
    Cmpi { rn: Reg, imm: i32, sign: Reg},
    Madd { rd: Reg, rn: Reg, rm: Reg, ra: Reg },  // 乘加指令
    Msub { rd: Reg, rn: Reg, rm: Reg, ra: Reg },  // 乘减指令
    Mneg { rd: Reg, rn: Reg, rm: Reg },  // 乘取相反数
    Smull { rd: Reg, rn: Reg, rm: Reg },  // 乘长整型

    // 浮点指令
    Fadd { rd: Reg, rn: Reg, rm: Reg },  // 浮点加法
    Fsub { rd: Reg, rn: Reg, rm: Reg },  // 浮点减法
    Fmul { rd: Reg, rn: Reg, rm: Reg },  // 浮点乘法
    Fdiv { rd: Reg, rn: Reg, rm: Reg },  // 浮点除法
    Fcmp { rn: Reg, rm: Reg, sign: Reg},  // 浮点比较
    Fcmpi { rn: Reg, imm: f32, sign: Reg},
    Frecpe { rd: Reg, rn: Reg },  // 浮点倒数
    Frecps { rd: Reg, rn: Reg, rm: Reg }, 

    // 类型转换
    Scvtf { rd: Reg, rn: Reg }, // 整数转浮点
    Fcvtzs { rd: Reg, rn: Reg }, // 浮点转整数
    Sxtw { rd: Reg, rn: Reg }, // 扩展符号位

    // 逻辑运算
    And {rd: Reg, rn: Reg, rm: Reg}, // 与
    Andi {rd: Reg, rn: Reg, imm: i32}, 
    Eor {rd: Reg, rn: Reg, rm: Reg}, // 异或
    Eori {rd: Reg, rn: Reg, imm: i32},
    Neg {rd: Reg, rn: Reg}, // 取相反数
    Fneg {rd: Reg, rn: Reg}, // 浮点取相反数
    Fmadd {rd: Reg, rn: Reg, rm: Reg, ra: Reg}, // 浮点乘加
    Fmsub {rd: Reg, rn: Reg, rm: Reg, ra: Reg}, // 浮点乘减
    Fmneg {rd: Reg, rn: Reg, rm: Reg}, // 浮点取相反数
    Fnmsub {rd: Reg, rn: Reg, rm: Reg, ra: Reg}, // 浮点乘减取相反数

    // 内存操作指令
    Str { rm: Reg, mem: MemLoc },  // 存储到内存
    Ldr { rd: Reg, mem: MemLoc },  // 从内存加载
    Stp { rd1: Reg, rd2: Reg, mem: MemLoc }, // 存储两个寄存器到内存
    Ldp { rd1: Reg, rd2: Reg, mem: MemLoc }, // 从内存加载两个寄存器

    // 移动指令
    Movz { rd: Reg, imm: i32},
    Movk { rd: Reg, imm: i32},
    MovReg {rd: Reg, rn: Reg},  // 寄存器移动
    Fmov {rd: Reg, rn: Reg}, // 浮点数移动
    Fmovimm {rd: Reg, imm: f32},
    
    // 移位操作
    Lsl { rd: Reg, rn: Reg, imm: u32 },  // 逻辑左移
    Lsr { rd: Reg, rn: Reg, imm: u32 },  // 逻辑右移
    Asr { rd: Reg, rn: Reg, imm: u32 },  // 算术右移

    // 分支指令
    B { target: MirBlock },  // 无条件分支
    Beq { target: MirBlock, sign: Reg },  // 相等分支
    Bne { target: MirBlock, sign: Reg},  // 不相等分支
    Blt { target: MirBlock, sign: Reg},  // 小于分支
    Ble { target: MirBlock, sign: Reg},  // 小于等于分支
    Bgt { target: MirBlock, sign: Reg},  // 大于分支
    Bge { target: MirBlock, sign: Reg},  // 大于等于分支
    Ret,  // 函数返回
    Call {target: MirLabel, tail_call: bool, is_void: bool, rds: Vec<Reg>, args: Vec<Reg>},  // 调用函数
    
    // 其他指令
    Adrp {rd: Reg, label: MirLabel}, // 加载全局变量标签
    AddLabel {rd: Reg, rn: Reg, label: MirLabel},
    Subs {rd: Reg, rn: Reg, rm: Reg, sign: Reg}, // 减法并设置条件码
    Subsi {rd: Reg, rn: Reg, imm: i32, sign: Reg}, // 减法并设置条件码
    Cset {rd: Reg, cond: Condition, sign: Reg}, // 条件设置
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Condition {
    condkind: ConditionKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConditionKind {
    Eq, // 相等
    Ne, // 不相等
    Lt, // 小于
    Le, // 小于等于
    Gt, // 大于
    Ge, // 大于等于
}

impl Condition {
    pub fn new(condkind: ConditionKind) -> Self {
        Self { condkind }
    }
    pub fn kind(&self) -> &ConditionKind {
        &self.condkind
    }
    pub fn eq() -> Self {
        Self::new(ConditionKind::Eq)
    }
    pub fn ne() -> Self {
        Self::new(ConditionKind::Ne)
    }
    pub fn lt() -> Self {
        Self::new(ConditionKind::Lt)
    }
    pub fn le() -> Self {
        Self::new(ConditionKind::Le)   
    }
    pub fn gt() -> Self {
        Self::new(ConditionKind::Gt)
    }
    pub fn ge() -> Self {
        Self::new(ConditionKind::Ge)
    }
}

pub struct MirInstData {
    pub kind: MirInstKind,
    pub succ: Option<MirInst>,
    pub pre: Option<MirInst>,
    pub parent: Option<MirBlock>,
}



#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MirInst(pub GenericPtr<MirInstData>);

impl MirInst {
    pub fn add(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg) -> Self {
        let kind = MirInstKind::Add { rd, rn, rm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn addi(ctx: &mut MirContext, rd: Reg, rn: Reg, imm: i32) -> Self {
        let kind = MirInstKind::Addi { rd, rn, imm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn sub(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg) -> Self {
        let kind = MirInstKind::Sub { rd, rn, rm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn subi(ctx: &mut MirContext, rd: Reg, rn: Reg, imm: i32) -> Self {
        let kind = MirInstKind::Subi { rd, rn, imm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn mul(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg) -> Self {
        let kind = MirInstKind::Mul { rd, rn, rm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn div(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg) -> Self {
        let kind = MirInstKind::Div { rd, rn, rm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn madd(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg, ra: Reg) -> Self {
        let kind = MirInstKind::Madd { rd, rn, rm, ra };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn smull(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg) -> Self {
        let kind = MirInstKind::Smull { rd, rn, rm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn msub(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg, ra: Reg) -> Self {
        let kind = MirInstKind::Msub { rd, rn, rm, ra };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn mneg(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg) -> Self {
        let kind = MirInstKind::Mneg { rd, rn, rm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn fmadd(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg, ra: Reg) -> Self {
        let kind = MirInstKind::Fmadd { rd, rn, rm, ra };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn fmsub(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg, ra: Reg) -> Self {
        let kind = MirInstKind::Fmsub { rd, rn, rm, ra };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn fmneg(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg) -> Self {
        let kind = MirInstKind::Fmneg { rd, rn, rm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn fnmsub(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg, ra: Reg) -> Self {
        let kind = MirInstKind::Fnmsub { rd, rn, rm, ra };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn fadd(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg) -> Self {
        let kind = MirInstKind::Fadd { rd, rn, rm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn fsub(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg) -> Self {
        let kind = MirInstKind::Fsub { rd, rn, rm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn fmul(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg) -> Self {
        let kind = MirInstKind::Fmul { rd, rn, rm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn fdiv(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg) -> Self {
        let kind = MirInstKind::Fdiv { rd, rn, rm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn neg(ctx: &mut MirContext, rd: Reg, rn: Reg) -> Self {
        let kind = MirInstKind::Neg { rd, rn };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn fneg(ctx: &mut MirContext, rd: Reg, rn: Reg) -> Self {
        let kind = MirInstKind::Fneg { rd, rn };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn and(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg) -> Self {
        let kind = MirInstKind::And { rd, rn, rm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn andi(ctx: &mut MirContext, rd: Reg, rn: Reg, imm: i32) -> Self {
        let kind = MirInstKind::Andi { rd, rn, imm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn eor(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg) -> Self {
        let kind = MirInstKind::Eor { rd, rn, rm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn eori(ctx: &mut MirContext, rd: Reg, rn: Reg, imm: i32) -> Self {
        let kind = MirInstKind::Eori { rd, rn, imm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn scvtf(ctx: &mut MirContext, rd: Reg, rn: Reg) -> Self {
        let kind = MirInstKind::Scvtf { rd, rn };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn fcvtzs(ctx: &mut MirContext, rd: Reg, rn: Reg) -> Self {
        let kind = MirInstKind::Fcvtzs { rd, rn };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn sxtw(ctx: &mut MirContext, rd: Reg, rn: Reg) -> Self {
        let kind = MirInstKind::Sxtw { rd, rn };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn str(ctx: &mut MirContext, rm: Reg, mem: MemLoc) -> Self {
        let kind = MirInstKind::Str { rm, mem};
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn ldr(ctx: &mut MirContext, rd: Reg, mem: MemLoc) -> Self {
        let kind = MirInstKind::Ldr { rd, mem };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn stp(ctx: &mut MirContext, rd1: Reg, rd2: Reg, mem: MemLoc) -> Self {
        let kind = MirInstKind::Stp { rd1, rd2, mem};
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn ldp(ctx: &mut MirContext, rd1: Reg, rd2: Reg, mem: MemLoc) -> Self {
        let kind = MirInstKind::Ldp { rd1, rd2, mem };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn movz(ctx: &mut MirContext, rd: Reg, imm: i32) -> Self {
        let kind = MirInstKind::Movz { rd, imm};
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn movk(ctx: &mut MirContext, rd: Reg, imm: i32) -> Self {
        let kind = MirInstKind::Movk { rd, imm};
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    
    pub fn lsl(ctx: &mut MirContext, rd: Reg, rn: Reg, imm: u32) -> Self {
        let kind = MirInstKind::Lsl { rd, rn, imm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn lsr(ctx: &mut MirContext, rd: Reg, rn: Reg, imm: u32) -> Self {
        let kind = MirInstKind::Lsr { rd, rn, imm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn asr(ctx: &mut MirContext, rd: Reg, rn: Reg, imm: u32) -> Self {
        let kind = MirInstKind::Asr { rd, rn, imm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn b(ctx: &mut MirContext, target: MirBlock) -> Self {
        let kind = MirInstKind::B { target };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn beq(ctx: &mut MirContext, target: MirBlock, sign: Reg) -> Self {
        let kind = MirInstKind::Beq { target, sign };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn bne(ctx: &mut MirContext, target: MirBlock, sign: Reg) -> Self {
        let kind = MirInstKind::Bne { target, sign };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn bgt(ctx: &mut MirContext, target: MirBlock, sign: Reg) -> Self {
        let kind = MirInstKind::Bgt { target, sign };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn bge(ctx: &mut MirContext, target: MirBlock, sign: Reg) -> Self {
        let kind = MirInstKind::Bge { target, sign };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    pub fn blt(ctx: &mut MirContext, target: MirBlock, sign: Reg) -> Self {
        let kind = MirInstKind::Blt { target, sign };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }  
    pub fn ble(ctx: &mut MirContext, target: MirBlock, sign: Reg) -> Self {
        let kind = MirInstKind::Ble { target, sign };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }


    pub fn ret(ctx: &mut MirContext) -> Self {
        let kind = MirInstKind::Ret;
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    
    pub fn mov_reg(ctx: &mut MirContext, rd: Reg, rn: Reg) -> Self {
        let kind = MirInstKind::MovReg { rd, rn };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn fmov(ctx: &mut MirContext, rd: Reg, rn: Reg) -> Self {
        let kind = MirInstKind::Fmov { rd, rn };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn fmovimm(ctx: &mut MirContext, rd: Reg, imm: f32) -> Self {
        let kind = MirInstKind::Fmovimm { rd, imm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn adrp(ctx: &mut MirContext, rd: Reg, label: MirLabel) -> Self {
        let kind = MirInstKind::Adrp { rd, label };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn add_label(ctx: &mut MirContext, rd: Reg, rn: Reg, label: MirLabel) -> Self {
        let kind = MirInstKind::AddLabel { rd, rn, label };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn call(ctx: &mut MirContext, target: MirLabel, tail_call: bool, is_void: bool, rds: Vec<Reg>, args: Vec<Reg>) -> Self {
        let kind = MirInstKind::Call { target, tail_call, is_void, rds, args };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn cmp(ctx: &mut MirContext, rn: Reg, rm: Reg, sign: Reg) -> Self {
        let kind = MirInstKind::Cmp { rn, rm, sign };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn cmpi(ctx: &mut MirContext, rn: Reg, imm: i32, sign: Reg) -> Self {
        let kind = MirInstKind::Cmpi { rn, imm, sign };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn fcmp(ctx: &mut MirContext, rn: Reg, rm: Reg, sign: Reg) -> Self {
        let kind = MirInstKind::Fcmp { rn, rm, sign };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    
    pub fn fcmpi(ctx: &mut MirContext, rn: Reg, imm: f32, sign: Reg) -> Self {
        let kind = MirInstKind::Fcmpi { rn, imm, sign };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    
    pub fn frecpe(ctx: &mut MirContext, rd: Reg, rn: Reg) -> Self {
        let kind = MirInstKind::Frecpe { rd, rn };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn frecps(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg) -> Self {
        let kind = MirInstKind::Frecps { rd, rn, rm };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn cset(ctx: &mut MirContext, rd: Reg, cond: Condition, sign: Reg) -> Self {
        let kind = MirInstKind::Cset { rd, cond, sign };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    
    pub fn subs(ctx: &mut MirContext, rd: Reg, rn: Reg, rm: Reg, sign: Reg) -> Self {
        let kind = MirInstKind::Subs { rd, rn, rm, sign };
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }
    
    pub fn subsi(ctx: &mut MirContext, rd: Reg, rn: Reg, imm: i32, sign: Reg) -> Self {
        let kind = MirInstKind::Subsi { rd, rn, imm, sign};
        let data = MirInstData {
            kind,
            succ: None,
            pre: None,
            parent: None,
        };
        ctx.alloc(data)
    }

    pub fn uses(self, mctx: &MirContext) -> Vec<Reg> {
        match &self.deref(mctx).unwrap().kind {
            MirInstKind::Add { rd:_, rn, rm } | 
                                MirInstKind::Sub { rd:_, rn, rm } |
                                MirInstKind::Mul { rd:_, rn, rm } |
                                MirInstKind::Div { rd:_, rn, rm } |
                                MirInstKind::And { rd:_, rn, rm } |
                                MirInstKind::Smull { rd:_, rn, rm } => vec![*rn, *rm],
            MirInstKind::Madd { rd:_, rn, rm, ra } => vec![*rn, *rm, *ra],
            MirInstKind::Msub { rd:_, rn, rm, ra } => vec![*rn, *rm, *ra],
            MirInstKind::Mneg { rd:_, rn, rm} => vec![*rn, *rm],
            MirInstKind::Fmadd { rd:_, rn, rm, ra } => vec![*rn, *rm, *ra],
            MirInstKind::Fmsub { rd:_, rn, rm, ra } => vec![*rn, *rm, *ra],
            MirInstKind::Fmneg { rd:_, rn, rm} => vec![*rn, *rm],
            MirInstKind::Fnmsub { rn, rm, ra, .. } => vec![*rn, *rm, *ra],
            MirInstKind::Fadd { rd:_, rn, rm } | 
                                MirInstKind::Fsub { rd:_, rn, rm } |
                                MirInstKind::Fmul { rd:_, rn, rm } |
                                MirInstKind::Fdiv { rd:_, rn, rm } => vec![*rn, *rm],
            MirInstKind::Addi {rd:_, rn, imm:_} |
                                MirInstKind::Subi {rd:_, rn, imm:_} |
                                MirInstKind::Andi {rd:_, rn, imm:_} => vec![*rn],
            MirInstKind::Scvtf { rd:_, rn } |
                                MirInstKind::Fcvtzs { rd:_, rn } |
                                MirInstKind::Sxtw { rd:_, rn }=> vec![*rn],
            MirInstKind::Eor { rd:_, rn, rm } => vec![*rn, *rm],
            MirInstKind::Eori { rd:_, rn, imm:_ } => vec![*rn],
            MirInstKind::Neg { rd:_, rn } => vec![*rn],
            MirInstKind::Fneg { rd:_, rn } => vec![*rn],
            MirInstKind::Ldr { mem, .. } => match mem {
                                MemLoc::Reg { address} => vec![*address],
                                MemLoc::RegOffset { base, .. } => vec![*base],
                                MemLoc::Reg2Offset { base, offset } => vec![*base, *offset],
                            },
            MirInstKind::Ldp { mem, .. } => match mem {
                                MemLoc::Reg { address} => vec![*address],
                                MemLoc::RegOffset { base, .. } => vec![*base],
                                MemLoc::Reg2Offset { base, offset } => vec![*base, *offset],
                            }
            MirInstKind::Str { rm, mem, .. } => {
                                let mut uses = vec![*rm];
                                match mem {
                                    MemLoc::Reg { address} => uses.push(*address),
                                    MemLoc::RegOffset { base, .. } => uses.push(*base),
                                    MemLoc::Reg2Offset { base, offset } => {
                                        uses.push(*base);
                                        uses.push(*offset);
                                    },
                                }
                                uses
                            }
            MirInstKind::Stp {rd1, rd2, mem, ..} => {
                                let mut uses = vec![*rd1, *rd2];
                                match mem {
                                    MemLoc::Reg { address} => uses.push(*address),
                                    MemLoc::RegOffset { base, .. } => uses.push(*base),
                                    MemLoc::Reg2Offset { base, offset } => {
                                        uses.push(*base);
                                        uses.push(*offset);
                                    }
                                }
                                uses
                            }
            MirInstKind::Movk { rd, imm:_ } => vec![*rd],
            MirInstKind::MovReg { rd:_, rn } => vec![*rn],
            MirInstKind::Lsl { rd:_, rn, imm:_ } => vec![*rn],
            MirInstKind::Lsr { rd:_, rn, imm:_ } => vec![*rn],
            MirInstKind::Asr { rd:_, rn, imm:_ } => vec![*rn],
            MirInstKind::Fmov { rd:_, rn} => vec![*rn],
            MirInstKind::AddLabel { rn, ..} => vec![*rn],
            MirInstKind::Cmp { rn, rm, .. } => vec![*rn, *rm],
            MirInstKind::Cmpi { rn, ..} => vec![*rn],
            MirInstKind::Fcmp { rn, rm , .. } => vec![*rn, *rm],
            MirInstKind::Fcmpi { rn, .. } => vec![*rn],
            MirInstKind::Frecpe { rn, .. } => vec![*rn],
            MirInstKind::Frecps { rn, rm, .. } => vec![*rn, *rm],
            MirInstKind::Subs { rd:_, rn, rm, ..} => vec![*rn, *rm],
            MirInstKind::Subsi { rd:_, rn, ..} => vec![*rn],
            MirInstKind::Call { is_void, args, .. } => {
                        let mut uses = vec![];
                        if *is_void {
                            for i in args {
                                uses.push(*i);
                            }
                        } else {
                            for i in args.iter().skip(1) {
                                uses.push(*i);
                            }
                        }
                        uses
                    }
            MirInstKind::Movz { .. } => vec![],
            MirInstKind::Fmovimm { .. } => vec![],
            MirInstKind::B { .. } => vec![],
            MirInstKind::Beq { sign, ..  } => vec![*sign],
            MirInstKind::Bne { sign, ..  } => vec![*sign],
            MirInstKind::Blt { sign, ..  } => vec![*sign],
            MirInstKind::Ble { sign, ..  } => vec![*sign],
            MirInstKind::Bgt { sign, ..  } => vec![*sign],
            MirInstKind::Bge { sign, ..  } => vec![*sign],
            MirInstKind::Ret => vec![],
            MirInstKind::Adrp { .. } => vec![],
            MirInstKind::Cset { sign, ..} => vec![*sign],
        }
    }
    
    pub fn def(self, mctx: &MirContext) -> Vec<Reg> {
        match &self.deref(mctx).unwrap().kind {
            MirInstKind::Add { rd, rn:_, rm:_ } | 
                MirInstKind::Sub { rd, rn:_, rm:_ } |
                MirInstKind::Mul { rd, rn:_, rm:_ } |
                MirInstKind::Div { rd, rn:_, rm:_ } |
                MirInstKind::And { rd, rn:_, rm:_ } |
                MirInstKind::Smull { rd, rn:_, rm:_ }=> vec![*rd],
            MirInstKind::Madd { rd, .. } => vec![*rd],
            MirInstKind::Msub { rd, .. } => vec![*rd],
            MirInstKind::Mneg { rd, .. } => vec![*rd],
            MirInstKind::Fmadd { rd, .. } => vec![*rd],
            MirInstKind::Fmsub { rd, .. } => vec![*rd],
            MirInstKind::Fmneg { rd, .. } => vec![*rd],
            MirInstKind::Fnmsub { rd, .. } => vec![*rd],
            MirInstKind::Fadd { rd, rn:_, rm:_ } | 
                                MirInstKind::Fsub { rd, rn:_, rm:_ } |
                                MirInstKind::Fmul { rd, rn:_, rm:_ } |
                                MirInstKind::Fdiv { rd, rn:_, rm:_ } => vec![*rd],
            MirInstKind::Addi {rd, rn:_, imm:_} |
                                MirInstKind::Subi {rd, rn:_, imm:_} |
                                MirInstKind::Andi {rd, rn:_, imm:_}=> vec![*rd],
            MirInstKind::Eor {rd, rn:_, rm:_} => vec![*rd],
            MirInstKind::Eori {rd, rn:_, imm:_} => vec![*rd],
            MirInstKind::Scvtf { rd, rn:_ } |
                                MirInstKind::Fcvtzs { rd, rn:_ } |
                                MirInstKind::Sxtw { rd, rn:_ } => vec![*rd],
            MirInstKind::Neg { rd, rn:_ } => vec![*rd],
            MirInstKind::Fneg { rd, rn:_} => vec![*rd],
            MirInstKind::Ldr { rd, .. } => vec![*rd],
            MirInstKind::Ldp {rd1, rd2, ..} => vec![*rd1, *rd2],
            MirInstKind::Str { .. } => vec![],
            MirInstKind::Stp { .. } => vec![],
            MirInstKind::Movz { rd, imm:_ } => vec![*rd],
            MirInstKind::Movk { rd, imm:_ } => vec![*rd],
            MirInstKind::Fmovimm { rd, .. } => vec![*rd],
            MirInstKind::Lsl { rd, rn:_, imm:_ } => vec![*rd],
            MirInstKind::Lsr { rd, rn:_, imm:_ } => vec![*rd],
            MirInstKind::Asr { rd, rn:_, imm:_ } => vec![*rd],
            MirInstKind::MovReg { rd, rn:_ } => vec![*rd],
            MirInstKind::Fmov { rd, rn:_} => vec![*rd],
            MirInstKind::Adrp { rd, .. } => vec![*rd],
            MirInstKind::AddLabel{ rd, .. } => vec![*rd],
            MirInstKind::Cset { rd, .. } => vec![*rd],
            MirInstKind::Subs {rd,sign, .. } => vec![*rd, *sign],
            MirInstKind::Subsi {rd, sign, .. } => vec![*rd, *sign],
            MirInstKind::Call { is_void, rds, args, .. } => {
                        let mut def = vec![];
                        if !*is_void {
                            def.push(*args.first().unwrap());
                        }
                        for reg in rds {
                            def.push(*reg);
                        }
                        def
                    }
            MirInstKind::Cmp { sign, .. } => vec![*sign],
            MirInstKind::Cmpi { sign , .. } => vec![*sign],
            MirInstKind::Fcmp { sign , .. } => vec![*sign],
            MirInstKind::Fcmpi { sign, .. } => vec![*sign],
            MirInstKind::Frecpe { rd, .. } => vec![*rd],
            MirInstKind::Frecps { rd, .. } => vec![*rd],
            MirInstKind::B { ..} => vec![],
            MirInstKind::Beq { .. } => vec![],
            MirInstKind::Bne { .. } => vec![],
            MirInstKind::Blt { .. } => vec![],
            MirInstKind::Ble { .. } => vec![],
            MirInstKind::Bgt { .. } => vec![],
            MirInstKind::Bge { .. } => vec![],
            MirInstKind::Ret => vec![],
        }
    }
    
    pub fn all_regs(self, mctx: &MirContext) -> Vec<Reg> {
        match &self.deref(mctx).unwrap().kind {
            MirInstKind::Add { rd, rn, rm } | 
                MirInstKind::Sub { rd, rn, rm } |
                MirInstKind::Mul { rd, rn, rm } |
                MirInstKind::Div { rd, rn, rm } |
                MirInstKind::And { rd, rn, rm } |
                MirInstKind::Smull { rd, rn, rm } =>  vec![*rd, *rn, *rm],
            MirInstKind::Madd { rd, rn, rm, ra } => vec![*rd, *rn, *rm, *ra],
            MirInstKind::Msub { rd, rn, rm, ra } => vec![*rd, *rn, *rm, *ra],
            MirInstKind::Mneg { rd, rn, rm} => vec![*rd, *rn, *rm],
            MirInstKind::Fmadd { rd, rn, rm, ra } => vec![*rd, *rn, *rm, *ra],
            MirInstKind::Fmsub { rd, rn, rm, ra } => vec![*rd, *rn, *rm, *ra],
            MirInstKind::Fnmsub { rd, rn, rm, ra } => vec![*rd, *rn, *rm, *ra],
            MirInstKind::Fmneg { rd, rn, rm} => vec![*rd, *rn, *rm],
            MirInstKind::Fadd { rd, rn, rm } | 
                        MirInstKind::Fsub { rd, rn, rm } |
                        MirInstKind::Fmul { rd, rn, rm } |
                        MirInstKind::Fdiv { rd, rn, rm } => vec![*rd, *rn, *rm],
            MirInstKind::Addi {rd, rn, imm:_} |
                        MirInstKind::Subi {rd, rn, imm:_} |
                        MirInstKind::Andi {rd, rn, imm:_} => vec![*rd,*rn],
            MirInstKind::Eor { rd, rn, rm } => vec![*rd, *rn, *rm],
            MirInstKind::Eori { rd, rn, imm:_ } => vec![*rd, *rn],
            MirInstKind::Neg { rd, rn } => vec![*rd, *rn],
            MirInstKind::Fneg { rd, rn} => vec![*rd, *rn],
            MirInstKind::Scvtf { rd, rn } |
                        MirInstKind::Fcvtzs { rd, rn } |
                        MirInstKind::Sxtw { rd, rn } => vec![*rd, *rn],
            MirInstKind::Ldr { rd, mem } => match mem {
                        MemLoc::Reg { address } => vec![*rd, *address],
                        MemLoc::RegOffset { base, .. } => vec![*rd, *base],
                        MemLoc::Reg2Offset { base, offset } => vec![*rd, *base, *offset],
                    }
            MirInstKind::Ldp {rd1, rd2, mem} => match mem {
                        MemLoc::Reg { address } => vec![*rd1, *rd2, *address],
                        MemLoc::RegOffset { base, .. } => vec![*rd1, *rd2, *base],
                        MemLoc::Reg2Offset { base, offset } => vec![*rd1, *rd2, *base, *offset],
                    }
            MirInstKind::Str { rm, mem } => {
                        let mut regs = vec![*rm];
                        match mem {
                            MemLoc::Reg { address } => regs.push(*address),
                            MemLoc::RegOffset { base, .. } => regs.push(*base),
                            MemLoc::Reg2Offset { base, offset } => {
                                regs.push(*base);
                                regs.push(*offset);
                            },
                        }
                        regs
                    }
            MirInstKind::Stp { rd1, rd2, mem } => {
                        let mut regs = vec![*rd1, *rd2];
                        match mem {
                            MemLoc::Reg { address } => regs.push(*address),
                            MemLoc::RegOffset { base, .. } => regs.push(*base),
                            MemLoc::Reg2Offset { base, offset } => {
                                regs.push(*base);
                                regs.push(*offset);
                            },
                        }
                        regs
                    }
            MirInstKind::Movz { rd, .. } => vec![*rd],
            MirInstKind::Movk { rd, .. } => vec![*rd],
            MirInstKind::Fmovimm { rd, .. } => vec![*rd],
            MirInstKind::MovReg { rd, rn } => vec![*rd, *rn],
            MirInstKind::Lsl { rd, rn, imm:_ } => vec![*rd, *rn],
            MirInstKind::Lsr { rd, rn, imm:_ } => vec![*rd, *rn],
            MirInstKind::Asr { rd, rn, imm:_ } => vec![*rd, *rn],
            MirInstKind::Fmov { rd, rn} => vec![*rd, *rn],
            MirInstKind::Adrp { rd, .. } => vec![*rd],
            MirInstKind::AddLabel { rd, rn, .. } => vec![*rd, *rn],
            MirInstKind::Cmp { rn, rm, sign } => vec![*rn, *rm, *sign],
            MirInstKind::Cmpi { rn, imm:_ , sign } => vec![*rn, *sign],
            MirInstKind::Fcmp { rn, rm, sign} => vec![*rn, *rm, *sign],
            MirInstKind::Fcmpi { rn, sign, .. } => vec![*rn, *sign],
            MirInstKind::Frecpe { rd, rn } => vec![*rd, *rn],
            MirInstKind::Frecps { rd, rn, rm } => vec![*rd, *rn, *rm],
            MirInstKind::Cset { rd, sign, .. } => vec![*rd, *sign],
            MirInstKind::Subs {rd,rn, rm, sign} => vec![*rd, *rn, *rm, *sign],
            MirInstKind::Subsi {rd,rn, imm:_, sign} => vec![*rd, *rn, *sign],
            MirInstKind::Call {rds, args, .. } => {
                        let mut alls = vec![];
                        for reg in rds {
                            alls.push(*reg);
                        }
                        for reg in args {
                            alls.push(*reg);
                        }
                        alls
                    }
            MirInstKind::B { .. } => vec![],
            MirInstKind::Beq { sign, .. } => vec![*sign],
            MirInstKind::Bne { sign, .. } => vec![*sign],
            MirInstKind::Blt { sign, .. } => vec![*sign],
            MirInstKind::Ble { sign, .. } => vec![*sign],
            MirInstKind::Bgt { sign, .. } => vec![*sign],
            MirInstKind::Bge { sign, .. } => vec![*sign],
            MirInstKind::Ret => vec![],
        }
    }
    
    pub fn replace_regs(self, mctx: &mut MirContext, from: Reg, to: Reg) {
        match &mut self.deref_mut(mctx).unwrap().kind {
            MirInstKind::Ldr { rd, mem } => {
                        if *rd == from {
                            *rd = to;
                        }
                        match mem {
                            MemLoc::Reg { address } => {
                                if *address == from {
                                    *address = to;
                                }
                            }
                            MemLoc::RegOffset { base, .. } => {
                                if *base == from {
                                    *base = to;
                                }
                            }
                            MemLoc::Reg2Offset { base, offset } => {
                                if *base == from {
                                    *base = to;
                                }
                                if *offset == from {
                                    *offset = to;
                                }
                            }
                        }
                    }
            MirInstKind::Ldp { rd1, rd2, mem } => {
                        if *rd1 == from {
                            *rd1 = to;
                        }
                        if *rd2 == from {
                            *rd2 = to;
                        }
                        match mem {
                            MemLoc::Reg { address } => {
                                if *address == from {
                                    *address = to;
                                }
                            }
                            MemLoc::RegOffset { base, .. } => {
                                if *base == from {
                                    *base = to;
                                }
                            }
                            MemLoc::Reg2Offset { base, offset } => {
                                if *base == from {
                                    *base = to;
                                }
                                if *offset == from {
                                    *offset = to;
                                }
                            }
                        }
                    }
            MirInstKind::Str { rm, mem } => {
                        if *rm == from {
                            *rm = to;
                        }
                        match mem {
                            MemLoc::Reg { address } => {
                                if *address == from {
                                    *address = to;
                                }
                            }
                            MemLoc::RegOffset { base, .. } => {
                                if *base == from {
                                    *base = to;
                                }
                            }
                            MemLoc::Reg2Offset { base, offset } => {
                                if *base == from {
                                    *base = to;
                                }
                                if *offset == from {
                                    *offset = to;
                                }
                            }
                        }
                    }
            MirInstKind::Stp { rd1, rd2, mem } => {
                        if *rd1 == from {
                            *rd1 = to;
                        }
                        if *rd2 == from {
                            *rd2 = to;
                        }
                        match mem {
                            MemLoc::Reg { address } => {
                                if *address == from {
                                    *address = to;
                                }
                            }
                            MemLoc::RegOffset { base, .. } => {
                                if *base == from {
                                    *base = to;
                                }
                            }
                            MemLoc::Reg2Offset { base, offset } => {
                                if *base == from {
                                    *base = to;
                                }
                                if *offset == from {
                                    *offset = to;
                                }
                            }
                        }
                    }
            MirInstKind::Movz { rd, .. } |
                MirInstKind::Fmovimm { rd, .. } => {
                if *rd == from {
                    *rd = to;
                }
            }
            MirInstKind::Movk { rd, .. } => {
                if *rd == from {
                    *rd = to;
                }
            }
            MirInstKind::MovReg { rd, rn } |
                        MirInstKind::Fmov { rd, rn } |
                        MirInstKind::Lsl { rd, rn, .. } |
                        MirInstKind::Lsr { rd, rn, .. } |
                        MirInstKind::Asr { rd, rn, .. } => {
                if *rd == from {
                    *rd = to;
                }
                if *rn == from {
                    *rn = to;
                }
            }
            MirInstKind::Add { rd, rn, rm } | 
                        MirInstKind::Sub { rd, rn, rm } |
                        MirInstKind::Mul { rd, rn, rm } |
                        MirInstKind::Div { rd, rn, rm } |
                        MirInstKind::And { rd, rn, rm } |
                        MirInstKind::Smull { rd, rn, rm } => {
                if *rd == from {
                    *rd = to;
                }
                if *rn == from {
                    *rn = to;
                }
                if *rm == from {
                    *rm = to;
                }
            }
            MirInstKind::Madd { rd, rn, rm, ra } |
                    MirInstKind::Msub { rd, rn, rm, ra } |
                    MirInstKind::Fmadd { rd, rn, rm, ra } |
                    MirInstKind::Fmsub { rd, rn, rm, ra} |
                    MirInstKind::Fnmsub { rd, rn, rm, ra } => {
                if *rd == from {
                    *rd = to;
                }
                if *rn == from {
                    *rn = to;
                }
                if *rm == from {
                    *rm = to;
                }
                if *ra == from {
                    *ra = to;
                }
            }
            MirInstKind::Mneg { rd, rn, rm} |
            MirInstKind::Fmneg { rd, rn, rm} => {
                if *rd == from {
                    *rd = to;
                }
                if *rn == from {
                    *rn = to;
                }
                if *rm == from {
                    *rm = to;
                }
            }
            MirInstKind::Fadd { rd, rn, rm } | 
                        MirInstKind::Fsub { rd, rn, rm } |
                        MirInstKind::Fmul { rd, rn, rm } |
                        MirInstKind::Fdiv { rd, rn, rm } => {
                if *rd == from {
                    *rd = to;
                }
                if *rn == from {
                    *rn = to;
                }
                if *rm == from {
                    *rm = to;
                }
            }
            MirInstKind::Addi {rd, rn, imm:_} |
                        MirInstKind::Subi {rd, rn, imm:_} |
                        MirInstKind::Andi {rd, rn, imm:_} |
                        MirInstKind::Scvtf { rd, rn } |
                        MirInstKind::Fcvtzs { rd, rn }| 
                        MirInstKind::Sxtw { rd, rn} => {
                if *rd == from {
                    *rd = to;
                }
                if *rn == from {
                    *rn = to;
                }
            },
            MirInstKind::Eor { rd, rn, rm } => {
                if *rd == from {
                    *rd = to;
                }
                if *rn == from {
                    *rn = to;
                }
                if *rm == from {
                    *rm = to;
                }
            }
            MirInstKind::Eori { rd, rn, imm:_ } => {
                if *rd == from {
                    *rd = to;
                }
                if *rn == from {
                    *rn = to;
                }
            }
            MirInstKind::Neg { rd, rn } => {
                if *rd == from {
                    *rd = to;
                }
                if *rn == from {
                    *rn = to;
                }
            }
            MirInstKind::Fneg { rd, rn}=> {
                if *rd == from {
                    *rd = to;
                }
                if *rn == from {
                    *rn = to;
                }
            }
            MirInstKind::Adrp { rd, .. } => {
                if *rd == from {
                    *rd = to;
                }
            },
            MirInstKind::AddLabel { rd, rn, ..} => {
                if *rd == from {
                    *rd = to;
                }
                if *rn == from {
                    *rn = to;
                }
            }
            MirInstKind::Cmp { rn, rm, .. } => {
                if *rn == from {
                    *rn = to;
                }
                if *rm == from {
                    *rm = to;
                }
            }
            MirInstKind::Cmpi { rn, ..} => {
                if *rn == from {
                    *rn = to;
                }
            }
            MirInstKind::Fcmp { rn, rm , ..} => {
                if *rn == from {
                    *rn = to;
                }
                if *rm == from {
                    *rm = to;
                }
            }
            MirInstKind::Fcmpi { rn, .. } => {
                if *rn == from {
                    *rn = to;
                }
            }
            MirInstKind::Frecpe { rd, rn } => {
                if *rd == from {
                    *rd = to;
                }
                if *rn == from {
                    *rn = to;
                }
            }
            MirInstKind::Frecps { rd, rn, rm } => {
                if *rd == from {
                    *rd = to;
                }
                if *rn == from {
                    *rn = to;
                }
                if *rm == from {
                    *rm = to;
                }
            }
            MirInstKind::Cset { rd, .. } => {
                        if *rd == from {
                            *rd = to;
                        }
                    }
            MirInstKind::Subs {rd,rn, rm, ..} => {
                        if *rd == from {
                            *rd = to;
                        }
                        if *rn == from {
                            *rn = to;
                        }
                        if *rm == from {
                            *rm = to;
                        }
                    },
            MirInstKind::Subsi {rd,rn, ..} => {
                        if *rd == from {
                            *rd = to;
                        }
                        if *rn == from {
                            *rn = to;
                        }
                    },
            MirInstKind::Call { args, .. } => {
                        for i in args.iter_mut() {
                            if *i == from {
                                *i = to;
                            }
                        }
                    },
            MirInstKind::B { .. } => {},
            MirInstKind::Beq { .. } => {},
            MirInstKind::Bne { .. } => {},
            MirInstKind::Blt { .. } => {},
            MirInstKind::Ble { .. } => {},
            MirInstKind::Bgt { .. } => {},
            MirInstKind::Bge { .. } => {},
            MirInstKind::Ret => {},
        }
    }
    
    pub fn is_call(&self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Call { .. } => true,
            _ => false,
        }
    }
    
    pub fn is_tail_call(&self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Call { tail_call, .. } => *tail_call,
            _ => false,
        }
    }
    
    pub fn is_madd(&self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Madd { .. } => true,
            _ => false,
        }
    }
    
    pub fn is_void(&self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Call { is_void, .. } => *is_void,
            _ => false,
        }
    }

    pub fn get_args(&self, ctx: &MirContext) -> Vec<Reg> {
        match self.kind(ctx) {
            MirInstKind::Call { args, .. } => args.clone(),
            _ => panic!("Invalid Inst {}", self.display(ctx))
        }
    }

    pub fn get_ret_reg(&self, ctx: &MirContext) -> Reg {
        match self.kind(ctx) {
            MirInstKind::Call { args, .. } => args[0],
            _ => panic!("Invalid Inst {}", self.display(ctx))
        }
    }

    pub fn get_rds(&self, ctx: &MirContext) -> Vec<Reg> {
        match self.kind(ctx) {
            MirInstKind::Call { rds, .. } => rds.clone(),
            _ => panic!("Invalid Inst {}", self.display(ctx))
        }
    }
}

impl MirInst {
    pub fn kind(self, ctx: &MirContext) -> &MirInstKind { 
        &self.deref(ctx).expect("Invalid Inst").kind 
    }

    pub fn kind_mut(self, ctx: &mut MirContext) -> &mut MirInstKind { 
        &mut self.deref_mut(ctx).expect("Invalid Inst").kind 
    }

    pub fn is_movz(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Movz { .. } => true,
            _ => false,
        }
    }

    pub fn is_movk(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Movk { .. } => true,
            _ => false,
        }
    }

    pub fn get_operand_bbk(self, ctx: &MirContext) -> MirBlock {
        match self.kind(ctx) {
            MirInstKind::B { target } |  // 无条件分支
            MirInstKind::Beq { target,.. } |   // 相等分支
            MirInstKind::Bne { target ,..} |   // 不相等分支
            MirInstKind::Blt { target ,..} |  // 小于分支
            MirInstKind::Ble { target ,..} |  // 小于等于分支
            MirInstKind::Bgt { target ,..} |  // 大于分支
            MirInstKind::Bge { target ,..} => *target,
            _ => panic!("Invalid Inst {}", self.display(ctx))
        }
    }

    pub fn get_pre(self, ctx: &MirContext) -> Option<MirInst> {
        self.deref(ctx).expect("Invalid Inst").pre
    }

    pub fn get_mem(self, ctx: &MirContext) -> MemLoc {
        match self.kind(ctx) {
            MirInstKind::Ldr { mem, .. } |
                MirInstKind::Ldp {  mem, .. } |
                MirInstKind::Str { mem, .. } |
                MirInstKind::Stp { mem, .. } => *mem,
            _ => panic!("Invalid Inst {}", self.display(ctx))
        }
    }

    pub fn is_cbr(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Beq { .. } |   // 相等分支
            MirInstKind::Bne { .. } |   // 不相等分支
            MirInstKind::Blt { .. } |  // 小于分支
            MirInstKind::Ble { .. } |  // 小于等于分支
            MirInstKind::Bgt { .. } |  // 大于分支
            MirInstKind::Bge { .. } => true,
            _ => false,
        }
    }

    pub fn is_br(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::B { .. } => true,
            _ => false,
        }
    }

    pub fn is_ret(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Ret { .. } => true,
            _ => false,
        }
    }

    pub fn is_fpu(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Fadd { .. } | MirInstKind::Fsub { .. } | MirInstKind::Fmul { .. } | MirInstKind::Fdiv { .. } | 
                MirInstKind::Fcmp{ .. } | MirInstKind::Fcvtzs { .. } | MirInstKind::Scvtf{..} | MirInstKind::Fneg { .. } | 
                MirInstKind::Fmov { .. } | MirInstKind::Fmadd { .. } | MirInstKind::Fmsub { .. } | MirInstKind::Fmneg { .. } |
                MirInstKind::Fmovimm { .. } | MirInstKind::Fcmpi { .. } | MirInstKind::Fnmsub { .. } |MirInstKind::Frecpe { .. } |
                MirInstKind::Frecps { .. } => true,
            _ => false,
        }
    }
    
    pub fn is_multiply(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Mul { .. } | MirInstKind::Div { .. } |
                MirInstKind::Smull { .. } | MirInstKind::Madd { .. }| MirInstKind::Msub { .. } | MirInstKind::Mneg { .. } => true,
            _ => false,
        }
    }

    pub fn is_sdiv(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Div { .. } => true,
            MirInstKind::Fdiv { .. } => true, 
            _ => false,
        }
    }

    pub fn is_fcvt(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Fcvtzs { .. } | MirInstKind::Scvtf{..} |
            MirInstKind::Fmov { .. } | MirInstKind::Fmovimm { .. } => true,
            _ => false,
        }
    }

    pub fn is_fmov(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Fmov { .. } | MirInstKind::Fmovimm { .. } => true,
            _ => false,
        }
    }
    pub fn is_fmultiply(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Fmadd { .. } | MirInstKind::Fmneg { .. } |
                MirInstKind::Fmsub { .. } | MirInstKind::Fnmsub { .. } => true,
            _ => false,
        }
    }

    pub fn is_imul(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Mul { .. } | MirInstKind::Smull { .. } | MirInstKind::Madd { .. }| MirInstKind::Msub { .. } | MirInstKind::Mneg { .. } => true,
            _ => false,
        }
    }
    
    pub fn is_idiv(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Div { .. } => true,
            _ => false,
        }
    }

    pub fn is_fmul(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Fmul { .. } | MirInstKind::Fmadd { .. } | MirInstKind::Fmneg { .. } |
                MirInstKind::Fmsub { .. } | MirInstKind::Fnmsub { .. } => true,
            _ => false,
        }
    }

    pub fn is_fdiv(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Fdiv { .. } => true,
            _ => false,
        }
    }

    pub fn is_branch(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::B { .. } |   // 无条件分支
            MirInstKind::Beq { .. } |   // 相等分支
            MirInstKind::Bne { .. } |   // 不相等分支
            MirInstKind::Blt { .. } |  // 小于分支
            MirInstKind::Ble { .. } |  // 小于等于分支
            MirInstKind::Bgt { .. } |  // 大于分支
            MirInstKind::Bge { .. } |
            MirInstKind::Call { .. } | 
            MirInstKind::Ret { .. } => true,
            _ => false,
        }
    }

    pub fn is_cset(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Cset { .. } => true,
            _ => false,
        }
    }
    
    pub fn is_load(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Ldr { .. } | MirInstKind::Ldp { .. } => true,
            _ => false,
        }
    }

    pub fn is_store(self, ctx: &MirContext) -> bool {
        match self.kind(ctx) {
            MirInstKind::Str { .. } | MirInstKind::Stp { .. } => true,
            _ => false,
        }
    }

    pub fn is_memory(self, ctx: &MirContext) -> bool {
        return self.is_load(ctx) || self.is_store(ctx);
    }

    pub fn replace_with(self, ctx: &mut MirContext, new_inst: MirInst) {
        let pre = self.pre(ctx);
        let succ = self.succ(ctx);

        if let Some(pre) = pre {
            pre.set_succ(ctx, Some(new_inst));
            new_inst.set_pre(ctx, Some(pre));
        }

        if let Some(succ) = succ {
            succ.set_pre(ctx, Some(new_inst));
            new_inst.set_succ(ctx, Some(succ));
        }

        if let Some(container) = self.container(ctx) {
            if container.head(ctx) == Some(self) {
                container.set_head(ctx, Some(new_inst));
            }

            if container.tail(ctx) == Some(self) {
                container.set_tail(ctx, Some(new_inst));
            }
        }
        new_inst.set_container(ctx, self.container(ctx));
        self.set_pre(ctx, None);
        self.set_succ(ctx, None);
        self.set_container(ctx, None);
    }

}


impl ArenaPtr for MirInst {
    type Arena = MirContext;
    type Data = MirInstData;
}

impl LinkedListNode for MirInst {
    type Container = MirBlock;
    type Ctx = MirContext;
    fn succ(self, ctx: &Self::Ctx) -> Option<Self> { self.deref(ctx).expect("Invalid Inst").succ }

    fn pre(self, ctx: &Self::Ctx) -> Option<Self> { self.deref(ctx).expect("Invalid Inst").pre }

    fn set_succ(self, arena: &mut Self::Ctx, succ: Option<Self>) {
        self.deref_mut(arena).expect("Invalid Inst").succ = succ;
    }

    fn set_pre(self, arena: &mut Self::Ctx, pre: Option<Self>) {
        self.deref_mut(arena).expect("Invalid Inst").pre = pre;
    }
    
    fn container(self, ctx: &Self::Ctx) -> Option<Self::Container> { self.deref(ctx).expect("Invalid Inst").parent }

    fn set_container(self, arena: &mut Self::Ctx, parent: Option<Self::Container>) {
        self.deref_mut(arena).expect("Invalid Inst").parent = parent;
    }

    fn insert_after(self, ctx: &mut Self::Ctx, node: Self) -> Result<(), LinkedListError<Self>> {
        if self.container(ctx).is_none() {
            return Err(LinkedListError::CurrentNodeNotLinked(self));
        }

        if node.container(ctx).is_some() {
            return Err(LinkedListError::NodeAlreadyInContainer(node));
        }

        if let Some(succ) = self.succ(ctx) {
            succ.set_pre(ctx, Some(node));
            node.set_succ(ctx, Some(succ));
        }

        node.set_pre(ctx, Some(self));
        self.set_succ(ctx, Some(node));

        match self.container(ctx) {
            Some(container) => {
                if container.tail(ctx) == Some(self) {
                    container.set_tail(ctx, Some(node));
                }
            }
            None => unreachable!(),
        }
        node.set_container(ctx, self.container(ctx));

        Ok(())
    }

    fn insert_before(self, ctx: &mut Self::Ctx, node: Self) -> Result<(), LinkedListError<Self>> {
        if self.container(ctx).is_none() { 
            return Err(LinkedListError::CurrentNodeNotLinked(self));
        }

        if node.container(ctx).is_some() {
            return Err(LinkedListError::NodeAlreadyInContainer(node));
        }

        if let Some(pre) = self.pre(ctx) {
            pre.set_succ(ctx, Some(node));
            node.set_pre(ctx, Some(pre));
        }

        node.set_succ(ctx, Some(self));
        self.set_pre(ctx, Some(node));

        match self.container(ctx) {
            Some(container) => {
                if container.head(ctx) == Some(self) {
                    container.set_head(ctx, Some(node));
                }
            }
            None => unreachable!(),
        }

        node.set_container(ctx, self.container(ctx));

        Ok(())
    }
    
}
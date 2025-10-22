use super::regs::Reg;
use crate::frontend::ir;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MirOperandKind {
    Mem(MemLoc),
    Reg(Reg),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MirOperand {
    pub(crate) typ: ir::typ::Typ,
    pub(crate) kind: MirOperandKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemLoc {
    /// 地址在寄存器
    Reg { address: Reg },
    /// 基地址在寄存器，偏移位立即数
    RegOffset { base: Reg, offset: i32 },
    /// 基地址和偏移位都在寄存器(全局数组)
    Reg2Offset { base: Reg, offset: Reg },
}




#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum RegKind {
    Integer,
    Address,
    Float,
}


#[derive(Debug, Clone, Copy, )]
pub enum Reg {
    P(PReg),
    V(VReg),
}

impl Reg {
    pub fn kind(&self) -> RegKind {
        match self {
            Reg::P(preg) => preg.kind(),
            Reg::V(vreg) => vreg.kind(),
        }
    }

    pub fn is_preg(&self) -> bool { matches!(self, Reg::P(_)) }
    
    pub fn preg_num(&self) -> u8 {
        match self {
            Reg::P(preg) => preg.0,
            Reg::V(vreg) => panic!("virtual reg {} has no preg_num", vreg.0),
        }
    }
    
    pub fn is_vreg(&self) -> bool { matches!(self, Reg::V(_)) }
    
    pub fn is_address(&self) -> bool { 
        match self {
            Reg::P(preg) => preg.kind() == RegKind::Address,
            Reg::V(vreg) => vreg.kind() == RegKind::Address,
        }
    }
    
    pub fn is_float(&self) -> bool { 
        match self {
            Reg::P(preg) => preg.kind() == RegKind::Float,
            Reg::V(vreg) => vreg.kind() == RegKind::Float,
        }
    }

    pub fn is_caller_saved(&self) -> bool {
        match self {
            Reg::P(preg) => {
                preg.is_callee_saved()
            }
            Reg::V(_) => false,
        }
    }

}

// 手动实现PartialEq和Eq
impl PartialEq for Reg {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // 两个物理寄存器比较
            (Reg::P(a), Reg::P(b)) => {
                // 判断是否属于同一寄存器文件
                match (a.kind(), b.kind()) {
                    // 通用寄存器文件（Integer和Address共享）
                    (RegKind::Integer, RegKind::Address)
                    | (RegKind::Address, RegKind::Integer)
                    | (RegKind::Integer, RegKind::Integer)
                    | (RegKind::Address, RegKind::Address) => a.0 == b.0,
                    
                    // 浮点寄存器文件
                    (RegKind::Float, RegKind::Float) => a.0 == b.0,
                    
                    // 不同寄存器文件（通用 vs 浮点）
                    _ => false,
                }
            }
            
            // 两个虚拟寄存器比较：仅需编号相同
            (Reg::V(a), Reg::V(b)) => a.0 == b.0,
            
            // 其他情况（物理vs虚拟）不相等
            _ => false,
        }
    }
}

// 手动实现Hash以保持与PartialEq一致
impl std::hash::Hash for Reg {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Reg::P(p) => {
                // 通用寄存器（Integer/Address）和浮点寄存器使用不同的哈希域
                let domain = match p.kind() {
                    RegKind::Integer | RegKind::Address => 0u8,
                    RegKind::Float => 1u8,
                };
                domain.hash(state);
                p.0.hash(state);
            }
            Reg::V(v) => {
                // 虚拟寄存器单独哈希域
                2u8.hash(state);
                v.0.hash(state);
            }
        }
    }
}

impl Eq for Reg {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PReg(pub u8, pub RegKind);

impl PReg {
    pub const fn new(num: u8, kind: RegKind) -> Self { Self(num, kind) }
    pub const fn num(&self) -> u8 { self.0 }
    pub const fn kind(&self) -> RegKind { self.1 }
    pub const fn is_integer(&self) -> bool { 
        match self.1 {
            RegKind::Integer => true,
            _ => false,
        }
     }
    pub const fn is_address(&self) -> bool { 
        match self.1 {
            RegKind::Address => true,
            _ => false,
        }
    }
    pub const fn is_float(&self) -> bool { 
        match self.1 {
            RegKind::Float => true,
            _ => false,
        }
    }
    pub fn hw_index(&self) -> u8 { self.0}

    pub fn is_callee_saved(&self) -> bool { CALLEE_SAVED_REGS.contains(self) }
    pub fn is_caller_saved(&self) -> bool { CALLER_SAVED_REGS.contains(self) }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VReg(pub u32, pub RegKind);

impl VReg {
    pub fn new(num: u32, kind: RegKind) -> Self { Self(num, kind) }

    pub fn num(&self) -> u32 { self.0 }

    pub fn kind(&self) -> RegKind { self.1 }
}

impl From<VReg> for Reg {
    fn from(vreg: VReg) -> Self { Self::V(vreg) }
}

impl From<PReg> for Reg {
    fn from(preg: PReg) -> Self { Self::P(preg) }
}

#[rustfmt::skip]
// ARMV8-A64地址寄存器
pub const fn x_reg(index: u8) -> PReg { PReg::new(index, RegKind::Address) }
pub const fn x0() -> PReg { PReg::new(0, RegKind::Address) }
pub const fn x1() -> PReg { PReg::new(1, RegKind::Address) }
pub const fn x2() -> PReg { PReg::new(2, RegKind::Address) }
pub const fn x3() -> PReg { PReg::new(3, RegKind::Address) }
pub const fn x4() -> PReg { PReg::new(4, RegKind::Address) }
pub const fn x5() -> PReg { PReg::new(5, RegKind::Address) }
pub const fn x6() -> PReg { PReg::new(6, RegKind::Address) }
pub const fn x7() -> PReg { PReg::new(7, RegKind::Address) }
pub const fn x8() -> PReg { PReg::new(8, RegKind::Address) }
pub const fn x9() -> PReg { PReg::new(9, RegKind::Address) }
pub const fn x10() -> PReg { PReg::new(10, RegKind::Address) }
pub const fn x11() -> PReg { PReg::new(11, RegKind::Address) }  
pub const fn x12() -> PReg { PReg::new(12, RegKind::Address) }
pub const fn x13() -> PReg { PReg::new(13, RegKind::Address) }
pub const fn x14() -> PReg { PReg::new(14, RegKind::Address) }
pub const fn x15() -> PReg { PReg::new(15, RegKind::Address) }
pub const fn x16() -> PReg { PReg::new(16, RegKind::Address) }
pub const fn x17() -> PReg { PReg::new(17, RegKind::Address) }
pub const fn x18() -> PReg { PReg::new(18, RegKind::Address) }
pub const fn x19() -> PReg { PReg::new(19, RegKind::Address) }
pub const fn x20() -> PReg { PReg::new(20, RegKind::Address) }
pub const fn x21() -> PReg { PReg::new(21, RegKind::Address) }
pub const fn x22() -> PReg { PReg::new(22, RegKind::Address) }
pub const fn x23() -> PReg { PReg::new(23, RegKind::Address) }
pub const fn x24() -> PReg { PReg::new(24, RegKind::Address) }
pub const fn x25() -> PReg { PReg::new(25, RegKind::Address) }
pub const fn x26() -> PReg { PReg::new(26, RegKind::Address) }
pub const fn x27() -> PReg { PReg::new(27, RegKind::Address) }
pub const fn x28() -> PReg { PReg::new(28, RegKind::Address) }
pub const fn fp() -> PReg { PReg::new(29, RegKind::Address) }
pub const fn x30() -> PReg { PReg::new(30, RegKind::Address) }
pub const fn sp() -> PReg { PReg::new(31, RegKind::Address) }
pub const fn xzr() -> PReg { PReg::new(32, RegKind::Address) }
pub const fn bzw() -> PReg { PReg::new(33, RegKind::Integer)}
// ARMV8-A64整数寄存器
pub const fn w_reg(index: u8) -> PReg { PReg::new(index, RegKind::Integer) }
pub const fn w0() -> PReg { PReg::new(0, RegKind::Integer) }
pub const fn w1() -> PReg { PReg::new(1, RegKind::Integer) }
pub const fn w2() -> PReg { PReg::new(2, RegKind::Integer) }
pub const fn w3() -> PReg { PReg::new(3, RegKind::Integer) }
pub const fn w4() -> PReg { PReg::new(4, RegKind::Integer) }
pub const fn w5() -> PReg { PReg::new(5, RegKind::Integer) }
pub const fn w6() -> PReg { PReg::new(6, RegKind::Integer) }
pub const fn w7() -> PReg { PReg::new(7, RegKind::Integer) }
pub const fn w8() -> PReg { PReg::new(8, RegKind::Integer) }
pub const fn w9() -> PReg { PReg::new(9, RegKind::Integer) }
pub const fn w10() -> PReg { PReg::new(10, RegKind::Integer) }
pub const fn w11() -> PReg { PReg::new(11, RegKind::Integer) }
pub const fn w12() -> PReg { PReg::new(12, RegKind::Integer) }
pub const fn w13() -> PReg { PReg::new(13, RegKind::Integer) }
pub const fn w14() -> PReg { PReg::new(14, RegKind::Integer) }
pub const fn w15() -> PReg { PReg::new(15, RegKind::Integer) }
pub const fn w16() -> PReg { PReg::new(16, RegKind::Integer) }
pub const fn w17() -> PReg { PReg::new(17, RegKind::Integer) }
pub const fn w18() -> PReg { PReg::new(18, RegKind::Integer) }
pub const fn w19() -> PReg { PReg::new(19, RegKind::Integer) }
pub const fn w20() -> PReg { PReg::new(20, RegKind::Integer) }
pub const fn w21() -> PReg { PReg::new(21, RegKind::Integer) }
pub const fn w22() -> PReg { PReg::new(22, RegKind::Integer) }
pub const fn w23() -> PReg { PReg::new(23, RegKind::Integer) }
pub const fn w24() -> PReg { PReg::new(24, RegKind::Integer) }
pub const fn w25() -> PReg { PReg::new(25, RegKind::Integer) }
pub const fn w26() -> PReg { PReg::new(26, RegKind::Integer) }
pub const fn w27() -> PReg { PReg::new(27, RegKind::Integer) }
pub const fn w28() -> PReg { PReg::new(28, RegKind::Integer) }
pub const fn w29() -> PReg { PReg::new(29, RegKind::Integer) }
pub const fn w30() -> PReg { PReg::new(30, RegKind::Integer) }
pub const fn w31() -> PReg { PReg::new(31, RegKind::Integer) }
pub const fn wzr() -> PReg { PReg::new(32, RegKind::Integer) }
// ARMV8-A64浮点寄存器
pub const fn s_reg(index: u8) -> PReg { PReg::new(index, RegKind::Float) }
pub const fn s0() -> PReg { PReg::new(0, RegKind::Float) }
pub const fn s1() -> PReg { PReg::new(1, RegKind::Float) }
pub const fn s2() -> PReg { PReg::new(2, RegKind::Float) }
pub const fn s3() -> PReg { PReg::new(3, RegKind::Float) }
pub const fn s4() -> PReg { PReg::new(4, RegKind::Float) }
pub const fn s5() -> PReg { PReg::new(5, RegKind::Float) }
pub const fn s6() -> PReg { PReg::new(6, RegKind::Float) }
pub const fn s7() -> PReg { PReg::new(7, RegKind::Float) }
pub const fn s8() -> PReg { PReg::new(8, RegKind::Float) }
pub const fn s9() -> PReg { PReg::new(9, RegKind::Float) }
pub const fn s10() -> PReg { PReg::new(10, RegKind::Float) }
pub const fn s11() -> PReg { PReg::new(11, RegKind::Float) }
pub const fn s12() -> PReg { PReg::new(12, RegKind::Float) }
pub const fn s13() -> PReg { PReg::new(13, RegKind::Float) }
pub const fn s14() -> PReg { PReg::new(14, RegKind::Float) }
pub const fn s15() -> PReg { PReg::new(15, RegKind::Float) }
pub const fn s16() -> PReg { PReg::new(16, RegKind::Float) }
pub const fn s17() -> PReg { PReg::new(17, RegKind::Float) }
pub const fn s18() -> PReg { PReg::new(18, RegKind::Float) }
pub const fn s19() -> PReg { PReg::new(19, RegKind::Float) }
pub const fn s20() -> PReg { PReg::new(20, RegKind::Float) }
pub const fn s21() -> PReg { PReg::new(21, RegKind::Float) }
pub const fn s22() -> PReg { PReg::new(22, RegKind::Float) }
pub const fn s23() -> PReg { PReg::new(23, RegKind::Float) }
pub const fn s24() -> PReg { PReg::new(24, RegKind::Float) }
pub const fn s25() -> PReg { PReg::new(25, RegKind::Float) }
pub const fn s26() -> PReg { PReg::new(26, RegKind::Float) }
pub const fn s27() -> PReg { PReg::new(27, RegKind::Float) }
pub const fn s28() -> PReg { PReg::new(28, RegKind::Float) }
pub const fn s29() -> PReg { PReg::new(29, RegKind::Float) }
pub const fn s30() -> PReg { PReg::new(30, RegKind::Float) }
pub const fn s31() -> PReg { PReg::new(31, RegKind::Float) }

pub const CALLER_SAVED_REGS: [PReg; 34] = [
    w9(), w10(), w11(), w12(), w13(), w14(), w15(), w16(), w17(), 
    x9(), x10(), x11(), x12(), x13(), x14(), x15(), x16(), x17(),
    s16(), s17(), s18(), s19(), s20(), s21(), s22(), s23(), s24(), s25(), s26(), s27(), s28(), s29(), s30(), s31(),
    ];
pub const CALLEE_SAVED_REGS: [PReg; 28] = [
    w19(), w20(), w21(), w22(), w23(), w24(), w25(), w26(), w27(), w28(), 
    x19(), x20(), x21(), x22(), x23(), x24(), x25(), x26(), x27(), x28(),
    s8(), s9(), s10(), s11(), s12(), s13(), s14(), s15(),
    ];
pub const INT_ARG_REGS: [PReg; 16] = [
    w0(), w1(), w2(), w3(), w4(), w5(), w6(), w7(), 
    x0(), x1(), x2(), x3(), x4(), x5(), x6(), x7()
    ];
pub const FLOAT_ARG_REGS: [PReg; 8] = [s0(), s1(), s2(), s3(), s4(), s5(), s6(), s7()];

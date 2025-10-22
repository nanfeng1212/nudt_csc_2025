use super::mir::{
    regs::{self,PReg, Reg, RegKind, VReg},
    mir_operand::MemLoc,
    mir_block::MirBlock,
    mir_function::MirFunction,
    mir_label::MirLabel,
    mir_inst::{MirInst, MirInstKind, Condition, ConditionKind},
    mir_context::{MirContext, RawData},
};
use crate::utils::{linked_list::LinkedListContainer,
                   storage::ArenaPtr};

                   
pub trait Display {
    fn display(self, mctx: &MirContext) -> String;
}

impl Display for MirLabel {
    fn display(self, _mctx: &MirContext) -> String { self.0 }
}

impl Display for MirBlock {
    fn display(self, mctx: &MirContext) -> String {
        self.label(mctx).clone().display(mctx)
    }
}

impl Display for MirFunction {
    fn display(self, mctx: &MirContext) -> String {
        self.label(mctx).clone().display(mctx)
    }
}

impl Display for PReg {
    fn display(self, _mctx: &MirContext) -> String {
        let mut asm = String::new();
        let str = match self.kind() {
            RegKind::Address => match self.num() {
                0 => "x0",
                1 => "x1",
                2 => "x2",
                3 => "x3",
                4 => "x4",
                5 => "x5",
                6 => "x6",
                7 => "x7",
                8 => "x8",
                9 => "x9",
                10 => "x10",
                11 => "x11",
                12 => "x12",
                13 => "x13",
                14 => "x14",
                15 => "x15",
                16 => "x16",
                17 => "x17",
                18 => "x18",
                19 => "x19",
                20 => "x20",
                21 => "x21",
                22 => "x22",
                23 => "x23",
                24 => "x24",
                25 => "x25",
                26 => "x26",
                27 => "x27",
                28 => "x28",
                29 => "fp",
                30 => "x30",
                31 => "sp",
                32 => "xzr",
                _ => "<invalid>",
            },
            RegKind::Integer => match self.num() {
                0 => "w0",
                1 => "w1",
                2 => "w2",
                3 => "w3",
                4 => "w4",
                5 => "w5",
                6 => "w6",
                7 => "w7",
                8 => "w8",
                9 => "w9",
                10 => "w10",
                11 => "w11",
                12 => "w12",
                13 => "w13",
                14 => "w14",
                15 => "w15",
                16 => "w16",
                17 => "w17",
                18 => "w18",
                19 => "w19",
                20 => "w20",
                21 => "w21",
                22 => "w22",
                23 => "w23",
                24 => "w24",
                25 => "w25",
                26 => "w26",
                27 => "w27",
                28 => "w28",
                29 => "fp",
                30 => "w30",
                31 => "sp",
                32 => "wzr",
                _ => "invalid",
            }
            RegKind::Float => match self.num() {
                0 => "s0",
                1 => "s1",
                2 => "s2",
                3 => "s3",
                4 => "s4",
                5 => "s5",
                6 => "s6",
                7 => "s7",
                8 => "s8",
                9 => "s9",
                10 => "s10",
                11 => "s11",
                12 => "s12",
                13 => "s13",
                14 => "s14",
                15 => "s15",
                16 => "s16",
                17 => "s17",
                18 => "s18",
                19 => "s19",
                20 => "s20",
                21 => "s21",
                22 => "s22",
                23 => "s23",
                24 => "s24",
                25 => "s25",
                26 => "s26",
                27 => "s27",
                28 => "s28",
                29 => "s29",
                30 => "s30",
                31 => "s31",
                _ => "invalid",
            }
        };
        asm += str.to_string().as_str();
        asm
    }
}

impl Display for VReg {
    fn display(self, _mctx: &MirContext) -> String  {
        let mut asm = String::new();
        let str = match self.1 {
            RegKind::Address => "vx",
            RegKind::Integer => "vw",
            RegKind::Float => "s",
            //RegKind::Vector => "$v",
        };
        asm += str.to_string().as_str();
        asm += &self.0.to_string();
        asm
    }
}

impl Display for Reg {
    fn display(self, mctx: &MirContext) -> String {
        let mut asm = String::new();
        match self {
            Reg::P(preg) => asm += format!("{}", preg.display(mctx)).as_str(),
            Reg::V(vreg) => asm += format!("{}", vreg.display(mctx)).as_str(),
        }
        asm
    }
}

impl Display for MirInst {
    fn display(self, mctx: &MirContext) -> String {
        let mut asm = String::new();
        match &self.deref(mctx).unwrap().kind {
            MirInstKind::Str { rm, mem } => {
                asm += format!("str {}, {}", rm.display(mctx), mem.display(mctx)).as_str()
            }
            MirInstKind::Stp { rd1, rd2, mem } => {
                asm += format!("stp {}, {}, {}", rd1.display(mctx), rd2.display(mctx), mem.display(mctx)).as_str()
            }
            MirInstKind::Ldr { rd, mem } => {
                asm += format!("ldr {}, {}", rd.display(mctx), mem.display(mctx)).as_str()
            }
            MirInstKind::Ldp { rd1, rd2, mem } => {
                asm += format!("ldp {}, {}, {}", rd1.display(mctx), rd2.display(mctx), mem.display(mctx)).as_str()
            }
            MirInstKind::Movz{ rd, imm } => {
                asm += format!("movz {}, #{}",  rd.display(mctx), imm).as_str();
            }
            MirInstKind::Movk{ rd, imm } => {
                asm += format!("movk {}, #{}, lsl #16",  rd.display(mctx),imm).as_str();
            }
            MirInstKind::MovReg { rd, rn } => {
                if !rd.is_address() && rn.is_address() && rn.is_preg() {
                    let tmp_reg: Reg = regs::w_reg(rn.preg_num()).into();
                    asm += format!("mov {}, {}", rd.display(mctx), tmp_reg.display(mctx)).as_str();
                } else {
                    asm += format!("mov {}, {}", rd.display(mctx), rn.display(mctx)).as_str()
                }
            }
            MirInstKind::Fmov { rd, rn } => {
                asm += format!("fmov {}, {}", rd.display(mctx), rn.display(mctx)).as_str()
            }
            MirInstKind::Fmovimm { rd, imm } => {
                asm += format!("fmov {}, #{}", rd.display(mctx), imm).as_str()
            }
            MirInstKind::Neg { rd, rn } => {
                asm += format!("neg {}, {}", rd.display(mctx), rn.display(mctx)).as_str()
            }
            MirInstKind::Fneg {rd, rn} => {
                asm += format!("fneg {}, {}", rd.display(mctx), rn.display(mctx)).as_str()
            }
            MirInstKind::B { target } => {
                asm += format!("b {}", target.clone().display(mctx)).as_str()
            }
            MirInstKind::Ret => {
                asm += format!("ret").as_str()
            }
            MirInstKind::Adrp { rd, label } => {
                asm += format!("adrp {}, {}", rd.display(mctx), label.clone().display(mctx)).as_str();
            }
            MirInstKind::AddLabel { rd, rn, label } => {
                asm += format!("add {}, {}, :lo12:{}",rd.display(mctx), rn.display(mctx), label.clone().display(mctx)).as_str()
            }
            MirInstKind::Add { rd, rn, rm } => {
                // 如果rn是Address但rm是Integer,需要统一用address
                if rn.is_address() && !rm.is_address() {
                    asm += format!("add {}, {}, {}, SXTW", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str();
                }
                else {
                    asm += format!("add {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str();
                }
            }
            MirInstKind::Addi {rd,rn,imm} => {
                asm += format!("add {}, {}, #{}", rd.display(mctx), rn.display(mctx), imm).as_str();
            }
            MirInstKind::Sub { rd, rn, rm } => {
                if rn.is_address() && !rm.is_address() {
                    asm += format!("sub {}, {}, {}, SXTW", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str();
                }
                else {
                    asm += format!("sub {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str();
                }
            }
            MirInstKind::Subi {rd,rn,imm} => {
                asm += format!("sub {}, {}, #{}", rd.display(mctx), rn.display(mctx), imm).as_str();
            }
            MirInstKind::Mul { rd, rn, rm } => {
                asm += format!("mul {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str();
            }
            MirInstKind::Div { rd, rn, rm } => {
                   asm += format!("sdiv {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str();
            }   
            MirInstKind::And {rd, rn, rm} => {
                asm += format!("and {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str();
            }
            MirInstKind::Andi {rd, rn, imm} => {
                asm += format!("and {}, {}, #{}", rd.display(mctx), rn.display(mctx), imm).as_str();
            }
            MirInstKind::Madd { rd, rn, rm, ra } => {
                asm += format!("madd {}, {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx), ra.display(mctx)).as_str();
            }

            MirInstKind::Smull { rd, rn, rm } => {
                asm += format!("smull {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str();
            }

            MirInstKind::Msub { rd, rn, rm, ra } => {
                asm += format!("msub {}, {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx), ra.display(mctx)).as_str();
            }
            MirInstKind::Mneg { rd, rn, rm } => {
                asm += format!("mneg {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str();
            }
            MirInstKind::Fmadd { rd, rn, rm, ra } => {
                asm += format!("fmadd {}, {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx), ra.display(mctx)).as_str();
            }
            MirInstKind::Fmsub { rd, rn, rm, ra } => {
                asm += format!("fmsub {}, {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx), ra.display(mctx)).as_str();
            }
            MirInstKind::Fmneg { rd, rn, rm } => {
                asm += format!("fnmul {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str();
            }
            MirInstKind::Fnmsub { rd, rn, rm, ra } => {
                asm += format!("fnmsub {}, {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx), ra.display(mctx)).as_str();
            } 
            MirInstKind::Fadd {rd, rn, rm} => {
                asm += format!("fadd {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str();
            }
            MirInstKind::Fsub {rd, rn, rm} => {
                asm += format!("fsub {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str();
            }
            MirInstKind::Fmul {rd, rn, rm} => {
                asm += format!("fmul {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str();
            }
            MirInstKind::Fdiv {rd, rn, rm} => {
                asm += format!("fdiv {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str();
            }
            MirInstKind::Frecpe { rd, rn } => {
                asm += format!("frecpe {}, {}", rd.display(mctx), rn.display(mctx)).as_str()
            }
            MirInstKind::Frecps { rd, rn, rm } => {
                asm += format!("frecps {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str()
            }
            MirInstKind::Eor {rd, rn, rm} => {
                asm += format!("eor {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str();
            }
            MirInstKind::Eori {rd, rn, imm} => {
                asm += format!("eor {}, {}, #{}", rd.display(mctx), rn.display(mctx), imm).as_str();
            }
            MirInstKind::Scvtf { rd, rn } => {
                asm += format!("scvtf {}, {}", rd.display(mctx), rn.display(mctx)).as_str()
            }
            MirInstKind::Fcvtzs { rd, rn } => {
                asm += format!("fcvtzs {}, {}", rd.display(mctx), rn.display(mctx)).as_str()
            }
            MirInstKind::Sxtw { rd, rn } => {
                asm += format!("sxtw {}, {}", rd.display(mctx), rn.display(mctx)).as_str()
            }
            MirInstKind::Call { target , .. } => {
                asm += format!("bl {}", target.clone().display(mctx)).as_str()
            }
            MirInstKind::Cmp { rn, rm , ..} => {
                asm += format!("cmp {}, {}", rn.display(mctx), rm.display(mctx)).as_str()
            }
            MirInstKind::Cmpi { rn, imm, ..} => {
                asm += format!("cmp {}, #{}", rn.display(mctx), imm).as_str()
            }
            MirInstKind::Fcmp { rn, rm, .. } => {
                asm += format!("fcmp {}, {}", rn.display(mctx), rm.display(mctx)).as_str()
            }
            MirInstKind::Fcmpi { rn, ..} => {
                asm += format!("fcmp {}, 0.0", rn.display(mctx)).as_str()
            }
            MirInstKind::Cset { rd,cond, .. } => {
                asm += format!("cset {}, {}", rd.display(mctx), cond.display(mctx)).as_str()
            }
            MirInstKind::Subs {rd, rn, rm, ..} => {
                asm += format!("subs {}, {}, {}", rd.display(mctx), rn.display(mctx), rm.display(mctx)).as_str();
            }
            MirInstKind::Subsi {rd, rn, imm, ..} => {
                asm += format!("subs {}, {}, #{}", rd.display(mctx), rn.display(mctx), imm).as_str();
            }
            MirInstKind::Beq { target , ..} => {
                asm += format!("b.eq {}", target.clone().display(mctx)).as_str()
            }
            MirInstKind::Bne { target, .. } => {
                asm += format!("b.ne {}", target.clone().display(mctx)).as_str()
            }
            MirInstKind::Blt { target, .. } => {
                asm += format!("b.lt {}", target.clone().display(mctx)).as_str()
            }
            MirInstKind::Bgt { target, .. } => {
                asm += format!("b.gt {}", target.clone().display(mctx)).as_str()
            }
            MirInstKind::Ble { target, .. } => {
                asm += format!("b.le {}", target.clone().display(mctx)).as_str()
            }
            MirInstKind::Bge { target, .. } => {
                asm += format!("b.ge {}", target.clone().display(mctx)).as_str()
            }
            MirInstKind::Lsl { rd, rn, imm} => {
                asm += format!("lsl {}, {}, #{}", rd.display(mctx), rn.display(mctx), imm).as_str();
            }
            MirInstKind::Lsr { rd, rn, imm } => {
                asm += format!("lsr {}, {}, #{}", rd.display(mctx), rn.display(mctx), imm).as_str();
            }
            MirInstKind::Asr { rd, rn, imm } => {
                asm += format!("asr {}, {}, #{}", rd.display(mctx), rn.display(mctx), imm).as_str();
            }
        }
        asm
    }
}

impl Display for MemLoc {
    fn display(self, mctx: &MirContext) -> String {
        let mut asm = String::new();
        match self {
            MemLoc::Reg { address } => {
                asm += format!("[{}]", address.display(mctx)).as_str()
            }
            MemLoc::RegOffset { base, offset } => {
                if offset == 0 {
                    asm += format!("[{}]", base.display(mctx)).as_str()
                }
                else{
                    asm += format!("[{}, #{}]", base.display(mctx), offset).as_str()
                }
            }
            MemLoc::Reg2Offset { base, offset } => {
                if offset.is_address() {
                    asm += format!("[{}, {}]", base.display(mctx), offset.display(mctx)).as_str()
                } else {
                    asm += format!("[{}, {}, SXTW]", base.display(mctx), offset.display(mctx)).as_str()
                }
            }
        }
        asm
    }
}

impl Display for Condition {
    fn display(self, _mctx: &MirContext) -> String {
        let mut asm = String::new();
        match self.kind() {
            ConditionKind::Eq => asm += format!("eq").as_str(),
            ConditionKind::Ne => asm += format!("ne").as_str(),
            ConditionKind::Lt => asm += format!("lt").as_str(),
            ConditionKind::Gt => asm += format!("gt").as_str(),
            ConditionKind::Le => asm += format!("le").as_str(),
            ConditionKind::Ge => asm += format!("ge").as_str(),
        }
        asm
    }
}

impl MirContext {
    pub fn get_asm(&self) -> String {
        let mut asm = String::new();
        asm += "\t.arch armv8-a\n\t.text\n";
        let runtime_funcs = vec![
            "getint", "getch", "getfloat", "getarray", "getfarray",
            "putint", "putch", "putfloat", "putarray", "putfarray",
            "_sysy_starttime", "_sysy_stoptime"
        ];
        for func in runtime_funcs {
            asm += format!("\t.extern {}\n", func).as_str();
        }
        asm += "\n";
        for func_data in self.funcs.iter() {
            let func = func_data.self_ptr();
            if func.is_external(self) {
                continue;
            }
            asm += format!("\t.global {}\n", func.label(self).clone().display(self)).as_str();
            asm += format!("\t.type {}, %function\n", func.label(self).clone().display(self)).as_str();
            asm += format!("\t.p2align 3\n").as_str();
            asm += format!("{}:\n", func.label(self).clone().display(self)).as_str();

            for block in func.iter(self) {
                let block_label = block.label(self).clone().display(self);
                let clean_label = block_label.trim_start_matches('.');
                // 函数的第一个基本块不输出标签
                if block != func.head(self).unwrap() {
                    asm += format!("{}:\n", clean_label).as_str();
                }
                for inst in block.iter(self) {
                    asm += format!("\t{}\n", inst.display(self)).as_str();
                }
            }
            asm += "\n";
        }
        asm += "\n";
        if !self.raw_data.is_empty() {
            for (label, raw_data) in self.raw_data.iter() {
                let label_name = label.clone().display(self);
                // 全局变量声明
                match raw_data {
                    RawData::Bytes(bytes,size) => {
                        asm += "\t.section .data\n";
                        asm += "\t.p2align 3\n";
                        asm += format!("\t.global {}\n", label_name).as_str();
                        asm += format!("\t.type {}, %object\n", label_name).as_str();
                        asm += format!("{}:\n", label_name).as_str();
                        // 按4字节分组输出.word
                        for chunk in bytes {
                            asm += format!("\t.word {}\n", chunk).as_str();
                        }
                        // 没有赋值的填zeros
                        if *size > 32 * bytes.len() {
                            asm += format!("\t.zero {}\n", size - 32 * bytes.len()).as_str();
                        }
                    }
                    RawData::Bss(size) => {
                        asm += "\t.section .bss\n";
                        asm += "\t.p2align 3\n";
                        asm += format!("\t.global {}\n", label_name).as_str();
                        asm += format!("\t.type {}, %object\n", label_name).as_str();
                        asm += format!("{}:\n", label_name).as_str();
                        asm += format!("\t.zero {}\n", size).as_str();
                    }
                }
                asm += "\n";
            }
        }
        asm
    }
}

impl MirBlock {
    pub fn dbg(&self, mctx: &MirContext) -> String {
        let mut asm = String::new();
        asm += format!("{}:\n", self.label(mctx).clone().display(mctx)).as_str();
        for inst in self.iter(mctx) {
            asm += format!("\t{}\n", inst.display(mctx)).as_str();
        }
        asm
    }
}

impl MirFunction {
    pub fn dbg(&self, mctx: &MirContext) -> String {
        let mut asm = String::new();
        for block in self.iter(mctx) {
            asm += block.dbg(mctx).as_str();
        }
        asm
    }
}
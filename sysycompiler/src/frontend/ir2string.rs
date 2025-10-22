use super::ir::{
    basicblock::BasicBlock,
    context::Context,
    function::Function,
    global::{Global, GlobalData},
    instruction::{
        BinaryOp as IBinaryOp, BitBinaryOp, ConversionOp, FCompCond, ICompCond, InstructionKind,
        MemAccessOp, TerminatorOp, UnaryOp as IUnaryOp,
    },
    typ::{Typ, TypeData},
    value::{Value, ValueKind},
};
use crate::{
    frontend::ir::instruction::Instruction,
    passes::pass::structure::GetName,
    utils::{
        linked_list::LinkedListContainer,
        storage::{ArenaPtr, Idx},
    },
};

pub trait Display {
    fn display(self, ctx: &Context) -> String;
}

impl Context {
    pub fn get_ir_string(&self) -> String {
        let mut ir = String::new();
        for decl in self.syslibdecls.iter() {
            ir += &format!(
                "declare {} @{}(",
                decl.get_return_type().display(self),
                decl.get_name()
            );
            let params_typ = decl.get_parameters_typ();
            for (i, param_typ) in params_typ.iter().enumerate() {
                if i == 0 {
                    ir += &format!("{}", param_typ.display(self));
                } else {
                    ir += &format!(", {}", param_typ.display(self));
                }
            }
            ir += ")\n";
        }
        for GlobalData {
            self_ptr: global, ..
        } in self.globals.iter()
        {
            ir += &format!("{}\n", global.display(self));
        }

        for func in self.get_functions() {
            ir += &format!("{}\n", func.display(self));
        }
        // ir += crate::frontend::ir::my_lib::MEMSET_IR;
        ir
    }
}

impl Display for Global {
    fn display(self, ctx: &Context) -> String {
        let mut ir = String::new();
        let typ = self.value(ctx).typ().display(ctx);
        if self.value(ctx).is_undef() || self.value(ctx).is_zero() {
            ir += &format!("@{} = global {} zeroinitializer\n", self.name(ctx), typ);
        } else {
            if self.value(ctx).typ().is_float(ctx) {
                ir += &format!(
                    "@{} = global {} {}\n",
                    self.name(ctx),
                    typ,
                    self.value(ctx).to_string(ctx)
                );
            } else {
                ir += &format!(
                    "@{} = global {} {}\n",
                    self.name(ctx),
                    typ,
                    self.value(ctx).to_string(ctx)
                );
            }
        }
        ir
    }
}

impl Display for Function {
    fn display(self, ctx: &Context) -> String {
        let mut ir = String::new();
        ir += &format!(
            "define {} @{}(",
            self.get_return_type(ctx).display(ctx),
            self.get_id(ctx)
        );

        for (i, param) in self.get_parameters(ctx).iter().enumerate() {
            if i != 0 {
                ir += ", ";
            }
            ir += &format!("{} {}", param.kind(ctx).display(ctx), param.display(ctx));
        }

        ir += ") {\n";

        for bbk in self.iter(ctx) {
            ir += &format!("{}\n", bbk.display(ctx));
        }

        ir += "}";

        ir
    }
}

impl Display for Typ {
    fn display(self, ctx: &Context) -> String {
        let mut ir = String::new();
        match self.deref(ctx).unwrap() {
            TypeData::Void => {
                ir += "void";
            }
            TypeData::Bool => {
                ir += "i1";
            }
            TypeData::Int32 => {
                ir += "i32";
            }
            TypeData::Float32 => {
                ir += "float";
            }
            TypeData::Ptr { typ_of_ptr: typ } => {
                if typ.is_void(ctx) {
                    ir += &format!("i8*")
                } else {
                    ir += &format!("{}*", typ.display(ctx));
                }
            }
            TypeData::Array { element_type, len } => {
                // println!("DISPLAY ARRAY: {:?}", self.deref(ctx).unwrap());
                ir += &format!("[{} x ", len);
                // ir += "[";
                // println!("element_type: {:?}", element_type.deref(ctx).unwrap());
                // let element_type = match element_type.deref(ctx).unwrap() {
                //     TypeData::Array { element_type: element_type2, len:_ } => {
                //         element_type2
                //     },
                //     _ => {
                //         element_type
                //     }
                // };
                ir += &element_type.display(ctx);
                ir += "]";
            }
        }
        ir
    }
}

impl Display for BasicBlock {
    fn display(self, ctx: &Context) -> String {
        let mut ir = String::new();
        ir += &format!("{}:", self.get_name(ctx));
        for inst in self.iter(ctx) {
            ir += &format!("\n\t{}", inst.display(ctx));
        }
        ir
    }
}

impl Display for Instruction {
    fn display(self, ctx: &Context) -> String {
        let mut ir = String::new();
        if let Some(result) = self.get_result(ctx) {
            ir += &format!("{} = ", result.display(ctx));
        }

        match self.get_kind(ctx) {
            InstructionKind::Terminator { op } => match op {
                TerminatorOp::Ret => {
                    if let Some(ret_val) = self.get_operand(ctx, 0) {
                        ir += &format!(
                            "ret {} {}",
                            ret_val.kind(ctx).display(ctx),
                            ret_val.display(ctx)
                        );
                    } else {
                        ir += "ret void";
                    }
                }
                TerminatorOp::Br => {
                    ir += &format!(
                        "br label %{}",
                        self.get_operand_bbk(ctx, 0)
                            .expect("Failed to get the first operand of the br instruction")
                            .get_name(ctx)
                    );
                }
                TerminatorOp::Icbr => {
                    ir += &format!(
                        "br i1 {}, label %{}, label %{}",
                        self.get_operand(ctx, 0).unwrap().display(ctx),
                        self.get_operand_bbk(ctx, 0)
                            .expect("Failed to get the first operand bbk  of the br instruction")
                            .get_name(ctx),
                        self.get_operand_bbk(ctx, 1)
                            .expect("Failed to get the second operand bbk  of the br instruction")
                            .get_name(ctx)
                    );
                }
                TerminatorOp::Fcbr => {
                    ir += &format!(
                        "br i1 {}, label %{}, label %{}",
                        self.get_operand(ctx, 0).unwrap().display(ctx),
                        self.get_operand_bbk(ctx, 0)
                            .expect("Failed to get the first operand bbk  of the br instruction")
                            .get_name(ctx),
                        self.get_operand_bbk(ctx, 1)
                            .expect("Failed to get the second operand bbk  of the br instruction")
                            .get_name(ctx)
                    );
                }
            },
            InstructionKind::Binary { op } => match op {
                IBinaryOp::Add => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "add {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
                IBinaryOp::Fadd => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "fadd {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
                IBinaryOp::Sub => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "sub {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
                IBinaryOp::FSub => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "fsub {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
                IBinaryOp::Mul => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "mul {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
                IBinaryOp::Fmul => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "fmul {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
                IBinaryOp::Sdiv => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "sdiv {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
                IBinaryOp::Udiv => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "udiv {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
                IBinaryOp::Fdiv => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "fdiv {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
                IBinaryOp::Urem => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "urem {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
                IBinaryOp::Srem => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "srem {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
                IBinaryOp::Frem => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "frem {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
            },
            InstructionKind::Unary { op } => match op {
                IUnaryOp::Fneg => {
                    let typ = self.result_typ(ctx);
                    ir += &format!(
                        "fneg {} {}",
                        typ.display(ctx),
                        self.get_operand(ctx, 0).unwrap().display(ctx)
                    );
                }
                IUnaryOp::Not => {
                    let typ = self.result_typ(ctx);
                    ir += &format!(
                        "xor {} {}, 1",
                        typ.display(ctx),
                        self.get_operand(ctx, 0).unwrap().display(ctx)
                    );
                }
            },
            InstructionKind::BitBinary { op } => match op {
                BitBinaryOp::And => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "and {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
                BitBinaryOp::Or => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "or {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
                BitBinaryOp::Xor => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "xor {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
                BitBinaryOp::Shl => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "shl {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
                BitBinaryOp::Lshr => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "lshr {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
                BitBinaryOp::Ashr => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let op2 = self.get_operand(ctx, 1).unwrap();
                    let typ = op1.kind(ctx);
                    ir += &format!(
                        "ashr {} {}, {}",
                        typ.display(ctx),
                        op1.display(ctx),
                        op2.display(ctx)
                    );
                }
            },
            InstructionKind::MemAccess { op } => {
                match op {
                    MemAccessOp::Alloca { typ } => {
                        ir += &format!("alloca {}", typ.display(ctx));
                    }
                    MemAccessOp::Load => {
                        let typ = self.result_typ(ctx);
                        let op1 = self.get_operand(ctx, 0).unwrap();
                        // println!("Load => op1: {}", op1.display(ctx));
                        let typ_of_op1 = op1.kind(ctx);
                        // println!("Load => typ_of_op1: {}", typ_of_op1.display(ctx));
                        ir += &format!(
                            "load {}, {}* {}",
                            typ.display(ctx),
                            typ_of_op1.display(ctx),
                            op1.display(ctx)
                        );
                    }
                    MemAccessOp::Store => {
                        let op1 = self.get_operand(ctx, 0).unwrap();
                        // println!("Store => op1: {}", op1.display(ctx));
                        let typ_of_op1 = op1.kind(ctx);
                        let op2 = self.get_operand(ctx, 1).unwrap();
                        let typ_of_op2 = op2.kind(ctx);
                        // if typ_of_op1.is_float32(ctx ) || typ_of_op1.is_float64(ctx){
                        //     println!("store {} {}, {:.10}* {}", typ_of_op1.display(ctx), op1.display(ctx),
                        //                                         typ_of_op2.display(ctx), op2.display(ctx));
                        // }
                        if op1.is_constant(ctx) && typ_of_op2.is_float(ctx) {
                            // Problem Here
                            ir += &format!(
                                "store {} {}, {}* {}",
                                typ_of_op1.display(ctx),
                                op1.display(ctx),
                                typ_of_op2.display(ctx),
                                op2.display(ctx)
                            );
                            // println!("store {} {}, {}* {}", typ_of_op1.display(ctx), op1.display(ctx),
                            // typ_of_op2.display(ctx), op2.display(ctx));
                        } else {
                            ir += &format!(
                                "store {} {}, {}* {}",
                                typ_of_op1.display(ctx),
                                op1.display(ctx),
                                typ_of_op2.display(ctx),
                                op2.display(ctx)
                            );
                        }
                    }
                    MemAccessOp::GetElementPtr { typ: _ } => {
                        let op1 = self.get_operand(ctx, 0).unwrap();
                        let mut typ_of_op1 = op1.kind(ctx);
                        if typ_of_op1.is_ptr(ctx) {
                            typ_of_op1 = typ_of_op1.get_ptr_inner_type(ctx).expect(
                                "Failed to get the inner type of the pointer in ir2string ",
                            );
                            ir += &format!(
                                "getelementptr {}, {}* {}",
                                typ_of_op1.display(ctx),
                                typ_of_op1.display(ctx),
                                op1.display(ctx)
                            );
                            for i in 1..self.get_operands(ctx).len() {
                                let op = self.get_operand(ctx, i).unwrap();
                                let typ_of_op = op.kind(ctx);
                                ir += &format!(", {} {}", typ_of_op.display(ctx), op.display(ctx));
                            }
                        } else {
                            ir += &format!(
                                "getelementptr {}, {}* {}",
                                typ_of_op1.display(ctx),
                                typ_of_op1.display(ctx),
                                op1.display(ctx)
                            );
                            for i in 1..self.get_operands(ctx).len() {
                                let op = self.get_operand(ctx, i).unwrap();
                                let typ_of_op = op.kind(ctx);
                                ir += &format!(", {} {}", typ_of_op.display(ctx), op.display(ctx));
                            }
                        }
                    }
                };
            }
            InstructionKind::Conversion { op } => match op {
                ConversionOp::Trunc => {
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let typ_of_op1 = op1.kind(ctx);
                    let typ = self.result_typ(ctx);
                    ir += &format!(
                        "trunc {} {} to {}",
                        typ_of_op1.display(ctx),
                        op1.display(ctx),
                        typ.display(ctx)
                    );
                }
                ConversionOp::ZExt => {
                    let typ = self.result_typ(ctx);
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let typ_of_op1 = op1.kind(ctx);
                    ir += &format!(
                        "zext {} {} to {}",
                        typ_of_op1.display(ctx),
                        op1.display(ctx),
                        typ.display(ctx)
                    );
                }
                ConversionOp::SExt => {
                    let typ = self.result_typ(ctx);
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let typ_of_op1 = op1.kind(ctx);
                    ir += &format!(
                        "sext {} {} to {}",
                        typ_of_op1.display(ctx),
                        op1.display(ctx),
                        typ.display(ctx)
                    );
                }
                ConversionOp::FpToSi => {
                    let typ = self.result_typ(ctx);
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let typ_of_op1 = op1.kind(ctx);
                    ir += &format!(
                        "fptosi {} {} to {}",
                        typ_of_op1.display(ctx),
                        op1.display(ctx),
                        typ.display(ctx)
                    );
                }
                ConversionOp::SiToFp => {
                    let typ = self.result_typ(ctx);
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let typ_of_op1 = op1.kind(ctx);
                    ir += &format!(
                        "sitofp {} {} to {}",
                        typ_of_op1.display(ctx),
                        op1.display(ctx),
                        typ.display(ctx)
                    );
                }
                ConversionOp::Bitcast => {
                    let typ = self.result_typ(ctx);
                    let op1 = self.get_operand(ctx, 0).unwrap();
                    let typ_of_op1 = op1.kind(ctx);
                    ir += &format!(
                        "bitcast {}* {} to {}",
                        typ_of_op1.display(ctx),
                        op1.display(ctx),
                        typ.display(ctx)
                    );
                }
            },
            InstructionKind::IComp { cond } => {
                let op1 = self.get_operand(ctx, 0).unwrap();
                let typ_of_op1 = op1.kind(ctx);
                ir += &format!(
                    "icmp {} {} {}, {}",
                    cond.display(ctx),
                    typ_of_op1.display(ctx),
                    op1.display(ctx),
                    self.get_operand(ctx, 1).unwrap().display(ctx)
                );
            }
            InstructionKind::FComp { cond } => {
                let op1 = self.get_operand(ctx, 0).unwrap();
                let typ_of_op1 = op1.kind(ctx);
                ir += &format!(
                    "fcmp {} {} {}, {}",
                    cond.display(ctx),
                    typ_of_op1.display(ctx),
                    op1.display(ctx),
                    self.get_operand(ctx, 1).unwrap().display(ctx)
                )
            }
            InstructionKind::Phi => {
                let typ = self.result_typ(ctx);
                ir += &format!("phi {}", typ.display(ctx));
                let mut first = true;
                for (block, value) in self.get_phi(ctx).iter() {
                    if !first {
                        ir += ", ";
                    }
                    first = false;
                    ir += &format!("[{}, %{}]", value.display(ctx), block.get_name(ctx));
                }
            }
            InstructionKind::Call { .. } => {
                if self.get_result(ctx).is_none() {
                    ir += "call void ";
                    for (i, arg) in self.get_operands(ctx).iter().enumerate() {
                        let typ_of_arg = arg.kind(ctx);
                        if i == 0 {
                            ir += &format!("@{}(", arg.display(ctx));
                        } else if i == 1 {
                            ir += &format!("{} {}", typ_of_arg.display(ctx), arg.display(ctx));
                        } else {
                            ir += &format!(",{} {}", typ_of_arg.display(ctx), arg.display(ctx));
                        }
                    }
                    ir += ")";
                } else {
                    let typ = self.result_typ(ctx);
                    ir += &format!("call {}", typ.display(ctx));
                    for (i, arg) in self.get_operands(ctx).iter().enumerate() {
                        let typ_of_arg = arg.kind(ctx);
                        if i == 0 {
                            ir += &format!("@{}(", arg.display(ctx));
                        } else if i == 1 {
                            ir += &format!("{} {}", typ_of_arg.display(ctx), arg.display(ctx));
                        } else {
                            ir += &format!(",{} {}", typ_of_arg.display(ctx), arg.display(ctx));
                        }
                    }
                    ir += ")";
                }
            }
        }
        ir
    }
}

impl Display for Value {
    fn display(self, ctx: &Context) -> String {
        let mut ir = String::new();
        match &self.deref(ctx).unwrap().kind {
            ValueKind::InstResult {
                instruction: _,
                typ: _,
            }
            | ValueKind::Parameter {
                function: _,
                index: _,
                typ: _,
            } => {
                // We use the arena index directly as the value number. This is not a good way
                // to number values in a real compiler, but only for debugging purposes.
                ir += &format!("%v{}", self.0.index());
            }
            ValueKind::Constant { value } => {
                ir += &format!("{}", value.to_string(ctx));
                // println!("Constant: {}", value.to_string(ctx));
            }
            ValueKind::Function {
                function,
                ret_type: _,
            } => {
                ir += &format!("{}", function);
            }
        };
        ir
    }
}

impl Display for ICompCond {
    fn display(self, _ctx: &Context) -> String {
        match self {
            ICompCond::Eq => "eq".to_string(),
            ICompCond::Ne => "ne".to_string(),
            ICompCond::Ugt => "ugt".to_string(),
            ICompCond::Uge => "uge".to_string(),
            ICompCond::Ult => "ult".to_string(),
            ICompCond::Ule => "ule".to_string(),
            ICompCond::Sgt => "sgt".to_string(),
            ICompCond::Sge => "sge".to_string(),
            ICompCond::Slt => "slt".to_string(),
            ICompCond::Sle => "sle".to_string(),
        }
    }
}

impl Display for FCompCond {
    fn display(self, _ctx: &Context) -> String {
        match self {
            FCompCond::False => "fasle".to_string(),
            FCompCond::True => "true".to_string(),
            FCompCond::Oeq => "oeq".to_string(),
            FCompCond::One => "one".to_string(),
            FCompCond::Ogt => "ogt".to_string(),
            FCompCond::Oge => "oge".to_string(),
            FCompCond::Ord => "ord".to_string(),
            FCompCond::Ole => "ole".to_string(),
            FCompCond::Olt => "olt".to_string(),
            FCompCond::Ueq => "ueq".to_string(),
            FCompCond::Une => "une".to_string(),
            FCompCond::Ugt => "ugt".to_string(),
            FCompCond::Uge => "uge".to_string(),
            FCompCond::Ult => "ult".to_string(),
            FCompCond::Ule => "ule".to_string(),
            FCompCond::Uno => "uno".to_string(),
        }
    }
}

use std::collections::{VecDeque};

use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use super::mir::{
    mir_block::MirBlock,
    mir_context::{MirContext,RawData},
    mir_function::MirFunction,
    mir_inst::{MirInst, Condition},
    mir_label::MirLabel,
    mir_operand::{MirOperand, MirOperandKind, MemLoc},
    regs::{self, Reg, PReg},
};

use crate::{
    frontend::ir::{
            self, basicblock::BasicBlock, context::Context, function::Function, global::GlobalData, instruction::{InstructionKind, MemAccessOp}, value::{ConstantValue, Value}
        }, passes::pass::{self, structure::{ControlFlowGraph, GetName}}, utils::{
        linked_list::{LinkedListContainer, LinkedListNode},
        storage::ArenaPtr,
    }
};

pub struct CodegenContext<'s> {
    pub(super) mctx: MirContext,
    pub(super) ctx: &'s ir::context::Context,
    pub(super) table: HashMap<Value, MirOperand>,
    pub funcs: HashMap<String, MirFunction>,
    pub blocks: HashMap<BasicBlock, MirBlock>,
    pub globals: HashMap<String, MirLabel>,
    pub(super) curr_func: Option<MirFunction>,
    pub(super) curr_block: Option<MirBlock>,
    pub func_map: HashMap<MirFunction, Function>,
    pub release: HashSet<Value>,
    pub curr_cond: Option<(Value, InstructionKind)>,
    pub movedimm: HashMap<i32, Reg>,
}

pub trait CodeGen {
    fn codegen(&self, codegen: &mut CodegenContext);
}

impl CodeGen for GlobalData {
    fn codegen(&self, codegen: &mut CodegenContext) {
        let global = self.self_ptr;
        let name = global.name(codegen.ctx).to_string();
        let label_name = name.trim_start_matches('@').to_string();
        let label = MirLabel::from(label_name.clone());
        codegen.globals.insert(name.clone(), label.clone());
        
        // 递归展开常量值到字节序列
        fn flatten_constant_value(value: &ConstantValue, ctx: &ir::context::Context) -> Vec<i32> {
            match value {
                ConstantValue::Int32 { value, .. } => vec![*value],
                ConstantValue::Float32 { typ:_, value} => vec![value.to_bits() as i32],
                ConstantValue::Bool { value, .. } => vec![*value as i32],
                ConstantValue::Array { elements, typ:_ } => {
                                let mut bytes = Vec::new();
                                for element in elements {
                                    if let ConstantValue::Int32 { value, .. } = element {
                                        bytes.push(*value);
                                    }
                                    else {
                                        bytes.extend(flatten_constant_value(element, ctx));
                                    }
                                }
                                bytes
                            }
                ConstantValue::Undef { typ } | ConstantValue::Zero { typ } => {
                                vec![0; typ.bytewidth(ctx)/4]
                }
                ConstantValue::GlobalPtr { .. } => todo!(),
            }
        }
        match global.value(codegen.ctx) {
            ConstantValue::Int32 { value, .. } => {
                        let new_label = MirLabel::from(label_name.clone());
                        codegen.mctx.add_raw_data(new_label, RawData::Bytes(vec![*value], 4));
                    }
            ConstantValue::Float32 { typ:_, value } => {
                        let new_label = MirLabel::from(label_name.clone());
                        let bits = value.to_bits(); // 将浮点数转换为位表示
                        codegen.mctx.add_raw_data(new_label, RawData::Bytes(vec![bits as i32], 4));
                    }
            ConstantValue::Array { elements, .. } => {
                        let mut bytes: Vec<i32> = Vec::new();
                        for element in elements {
                            if let ConstantValue::Int32 { value, .. } = element {
                                bytes.push(*value);
                            }
                            else {
                                bytes.extend(flatten_constant_value(element, codegen.ctx))
                            }
                        }
                        let size = self.self_ptr.typ(codegen.ctx).bytewidth(codegen.ctx);
                        codegen.mctx.add_raw_data(label, RawData::Bytes(bytes, size));
                    }
            ConstantValue::Zero {typ} | ConstantValue::Undef { typ }=> {
                        let size = typ.bytewidth(codegen.ctx);
                        codegen.mctx.add_raw_data(label.clone(), RawData::Bss(size));
                    }
            ConstantValue::GlobalPtr { name: global_name, .. } => {
                        codegen.globals.insert(global_name.clone(), MirLabel::from(global_name));
                    }
            ConstantValue::Bool { value , .. } => {
                let new_label = MirLabel::from(label_name.clone());
                codegen.mctx.add_raw_data(new_label, RawData::Bytes(vec![*value as i32], 4));
            }
        }
    }
}

const ALLOW_FLOATS: [f32; 20] = [
    0.0, -0.0,
    0.125, 0.25, 0.5, 1.0, 2.0, 4.0, 8.0, 16.0,
    0.1875, 0.375, 0.75, 1.5, 3.0, 6.0, 12.0, 24.0,
    31.875, -31.875
];
// SYsY运行时函数链表
const SY_RUNTIME_FUNCS: &[&str] = &[
    "getint", "getch", "getfloat", "getarray", "getfarray",
    "putint", "putch", "putfloat", "putarray", "putfarray",
    "_sysy_starttime", "_sysy_stoptime"
];
impl CodeGen for ir::function::Function {
    fn codegen(&self, codegen: &mut CodegenContext) {
        let name = self.get_id(codegen.ctx);
        let label = MirLabel::from(name.clone());
        let is_external = SY_RUNTIME_FUNCS.contains(&name.as_str());
        let mfunc = MirFunction::new(&mut codegen.mctx, label, is_external);
        codegen.curr_func = Some(mfunc);
        codegen.funcs.insert(name, mfunc);
        codegen.func_map.insert(mfunc, *self);
        // 只生成用户函数的基本快，跳过运行时函数
        if !is_external {
            for block in self.iter(codegen.ctx) {
                block.codegen(codegen);
            }
            for (i, &param_val) in self.get_parameters(codegen.ctx).iter().take(8).enumerate() {
                let new_reg = if param_val.is_ptr(codegen.ctx) {
                    codegen.mctx.generate_vreg(regs::RegKind::Address).into()
                } else if param_val.kind(codegen.ctx).is_float(codegen.ctx) {
                    codegen.mctx.generate_vreg(regs::RegKind::Float).into()
                } else { 
                    codegen.mctx.generate_vreg(regs::RegKind::Integer).into()
                };
                let preg = codegen.generate_param_reg(param_val, i as u32);
                if param_val.kind(codegen.ctx).is_float(codegen.ctx) {
                    let fmov = MirInst::fmov(&mut codegen.mctx, new_reg, preg);
                    let _ = mfunc.head(&codegen.mctx).unwrap().push_back(&mut codegen.mctx, fmov);
                } else {
                    let mov = MirInst::mov_reg(&mut codegen.mctx, new_reg, preg);
                    let _ = mfunc.head(&codegen.mctx).unwrap().push_back(&mut codegen.mctx, mov);
                }
                let mem_loc = if param_val.is_ptr(codegen.ctx) { 
                    MirOperandKind::Mem(MemLoc::Reg{address: new_reg})
                } else {
                    MirOperandKind::Reg(new_reg)
                };
                codegen.table.insert(param_val, MirOperand {
                    typ: param_val.kind(codegen.ctx),
                    kind: mem_loc,
                });
            }
        }
    }   
}

impl CodeGen for ir::basicblock::BasicBlock {
    fn codegen(&self, codegen: &mut CodegenContext) {
        let name = self.get_name(codegen.ctx);
        let label = MirLabel::from(name.clone());
        let mblock = MirBlock::new(&mut codegen.mctx, label);
        codegen.curr_block = Some(mblock);
        let _ = codegen.curr_func.as_ref().unwrap().push_back(&mut codegen.mctx, mblock);
        codegen.blocks.insert(*self, mblock);
    }
}

impl CodeGen for ir::instruction::Instruction {
    fn codegen(&self, codegen: &mut CodegenContext) {
        match self.get_kind(codegen.ctx) {
            ir::instruction::InstructionKind::Terminator { op } => {
                match op {
                    ir::instruction::TerminatorOp::Br => {
                        let target = self.get_operand_bbk(codegen.ctx,0).expect("Br instruction must have a target basic block");
                        let b = MirInst::b(&mut codegen.mctx, codegen.blocks[&target]);
                        let _ =codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, b);
                    },
                    ir::instruction::TerminatorOp::Ret => {
                        if self.get_operands(codegen.ctx).len() != 0 {
                            let val = self.get_operand(codegen.ctx, 0);
                            if let Some(val) = val {
                                codegen.gen_ret_move(val);
                            }
                        } else {
                            
                        }
                    },
                    ir::instruction::TerminatorOp::Icbr => {
                        let target1 = self.get_operand_bbk(codegen.ctx ,0).expect("Br instruction must have a target basic block");
                        let target2 = self.get_operand_bbk(codegen.ctx ,1).expect("Br instruction must have a target basic block");
                        let resource = self.get_operand(codegen.ctx, 0).unwrap();

                        let pre_is_cmp = if let Some(ir_inst) = self.pre(codegen.ctx) {
                            match ir_inst.get_kind(codegen.ctx) {
                                ir::instruction::InstructionKind::IComp { .. } | ir::instruction::InstructionKind::FComp { .. } => true,
                                _ => false
                            }
                        } else {
                            false
                        };
                        let has_cond = if let Some(current_cond) = codegen.curr_cond {
                            if current_cond.0 == resource {
                                true
                            } else {
                                false
                            }
                        } else {
                            false
                        };
                        if resource.is_constant(codegen.ctx) {
                            if let ir::value::ValueKind::Constant { value } = &resource.deref(codegen.ctx).unwrap().kind {
                                if let ConstantValue::Bool { value, .. } = value {
                                    if *value == true {
                                        let b = MirInst::b(&mut codegen.mctx, codegen.blocks[&target1]);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, b);
                                    }
                                } else {
                                    panic!("Expected bool constant for icmp instruction");
                                }
                            } else {
                            panic!("Expected bool constant for icmp instruction");
                            }
                        }else if has_cond {
                            match codegen.curr_cond.unwrap().1 {
                                ir::instruction::InstructionKind::IComp { cond } => {
                                    let bzw_reg = regs::bzw().into();
                                    match cond {
                                        ir::instruction::ICompCond::Eq => {
                                            let beq = MirInst::beq(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, beq);
                                        },
                                        ir::instruction::ICompCond::Ne => {
                                            let bne = MirInst::bne(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, bne);
                                        },
                                        ir::instruction::ICompCond::Sgt => {
                                            let bgt = MirInst::bgt(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, bgt);
                                        },
                                        ir::instruction::ICompCond::Sge => {
                                            let bge = MirInst::bge(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, bge);
                                        },
                                        ir::instruction::ICompCond::Slt => {
                                            let blt = MirInst::blt(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, blt);
                                        },
                                        ir::instruction::ICompCond::Sle => {
                                            let ble = MirInst::ble(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, ble);
                                        },
                                        _ => todo!(),
                                    }
                                }
                                ir::instruction::InstructionKind::FComp { cond } => {
                                    let bzw_reg = regs::bzw().into();
                                    match cond {
                                        ir::instruction::FCompCond::Ugt => {
                                            let bgt = MirInst::bgt(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, bgt);
                                        },
                                        ir::instruction::FCompCond::Uge => {
                                            let bge = MirInst::bge(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, bge);
                                        },
                                        ir::instruction::FCompCond::Ult => {
                                            let blt = MirInst::blt(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, blt);
                                        },
                                        ir::instruction::FCompCond::Ule => {
                                            let ble = MirInst::ble(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, ble);
                                        }
                                        ir::instruction::FCompCond::Oeq => {
                                            let beq = MirInst::beq(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, beq);
                                        }
                                        ir::instruction::FCompCond::One => {
                                            let bne = MirInst::bne(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, bne);
                                        }
                                        _ => todo!(),
                                    }
                                }
                                _ => panic!("Expected cmp instruction before icbr instruction")
                            }
                        }else if !pre_is_cmp {
                            let result_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                            let val = self.get_operand(codegen.ctx, 0).unwrap();
                            let val_opd = codegen.table[&val];
                            let val_reg = match val_opd.kind {
                                MirOperandKind::Reg(reg) => reg,
                                _ => panic!("Expected register operand for binary op"),
                            };
                            let bzw_reg = regs::bzw().into();
                            let subsi = MirInst::subsi(&mut codegen.mctx, result_reg, val_reg, 0, bzw_reg);
                            let _ =codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, subsi);
                            let bne = MirInst::bne(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                            let _ =codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, bne);
                        } else {
                            match self.pre(codegen.ctx).unwrap().get_kind(codegen.ctx) {
                                ir::instruction::InstructionKind::IComp { cond } => {
                                    let bzw_reg = regs::bzw().into();
                                    match cond {
                                        ir::instruction::ICompCond::Eq => {
                                            let beq = MirInst::beq(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, beq);
                                        },
                                        ir::instruction::ICompCond::Ne => {
                                            let bne = MirInst::bne(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, bne);
                                        },
                                        ir::instruction::ICompCond::Sgt => {
                                            let bgt = MirInst::bgt(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, bgt);
                                        },
                                        ir::instruction::ICompCond::Sge => {
                                            let bge = MirInst::bge(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, bge);
                                        },
                                        ir::instruction::ICompCond::Slt => {
                                            let blt = MirInst::blt(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, blt);
                                        },
                                        ir::instruction::ICompCond::Sle => {
                                            let ble = MirInst::ble(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, ble);
                                        },
                                        _ => todo!(),
                                    }
                                }
                                ir::instruction::InstructionKind::FComp { cond } => {
                                    let bzw_reg = regs::bzw().into();
                                    match cond {
                                        ir::instruction::FCompCond::Ugt => {
                                            let bgt = MirInst::bgt(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, bgt);
                                        },
                                        ir::instruction::FCompCond::Uge => {
                                            let bge = MirInst::bge(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, bge);
                                        },
                                        ir::instruction::FCompCond::Ult => {
                                            let blt = MirInst::blt(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, blt);
                                        },
                                        ir::instruction::FCompCond::Ule => {
                                            let ble = MirInst::ble(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, ble);
                                        }
                                        ir::instruction::FCompCond::Oeq => {
                                            let beq = MirInst::beq(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, beq);
                                        }
                                        ir::instruction::FCompCond::One => {
                                            let bne = MirInst::bne(&mut codegen.mctx, codegen.blocks[&target1], bzw_reg);
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, bne);
                                        }
                                        _ => todo!(),
                                    }
                                }
                                _ => panic!("Expected cmp instruction before icbr instruction")
                            }
                        }

                        let b = MirInst::b(&mut codegen.mctx, codegen.blocks[&target2]);
                        let _ =codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, b);
                    }
                    _ => todo!(),
                }
            },
            ir::instruction::InstructionKind::Unary { op } => {
                let result_val = self.get_result(codegen.ctx).unwrap();
                let lhs = self.get_operand(codegen.ctx, 0).unwrap();
                // 创建目标寄存器
                let temp_reg = if result_val.kind(codegen.ctx).is_int(codegen.ctx) || result_val.kind(codegen.ctx).is_bool(codegen.ctx) {
                    codegen.mctx.generate_vreg(regs::RegKind::Integer).into()
                } else {
                    codegen.mctx.generate_vreg(regs::RegKind::Float).into()
                };
                if !lhs.is_constant(codegen.ctx)  {
                    let lhs_reg = codegen.generate_opd_reg(lhs);
                    match op {
                        ir::instruction::UnaryOp::Fneg => {
                            if lhs.kind(codegen.ctx).is_float(codegen.ctx) {
                                let fneg = MirInst::fneg(&mut codegen.mctx, temp_reg, lhs_reg);
                                let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fneg);
                            } else {
                                let sub = MirInst::sub(&mut codegen.mctx, temp_reg, regs::wzr().into(), lhs_reg);
                                let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, sub);
                            }
                        }
                        ir::instruction::UnaryOp::Not => {  
                            if lhs.kind(codegen.ctx).is_float(codegen.ctx) {
                                panic!("float not implemented");
                            } 
                            let eori = MirInst::eori(&mut codegen.mctx, temp_reg, lhs_reg, 1);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, eori);
                        }
                    }
                } else {
                    // 处理常量左操作数
                    match &lhs.deref(codegen.ctx).unwrap().kind {
                        ir::value::ValueKind::Constant { value } => {
                            if let ConstantValue::Int32 { value, .. } = value {
                                let new_reg = if codegen.movedimm.contains_key(value) {
                                    codegen.movedimm[value]
                                } else {
                                    let temp = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                    codegen.generate_movimm(temp, *value);
                                    codegen.movedimm.insert(*value, temp);
                                    temp
                                };
                                match op {
                                    ir::instruction::UnaryOp::Fneg => {
                                        let sub = MirInst::sub(&mut codegen.mctx, temp_reg, regs::wzr().into(), new_reg);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, sub);
                                    }
                                    ir::instruction::UnaryOp::Not => {  
                                        let eori = MirInst::eori(&mut codegen.mctx, temp_reg, new_reg, 1);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, eori);
                                    },
                                }
                            } if let ConstantValue::Float32 { value, .. } = value {
                                let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                let bits = value.to_bits();
                                codegen.generate_movimm(new_reg, bits as i32);
                                match op {
                                    ir::instruction::UnaryOp::Fneg => {
                                        let fneg = MirInst::fneg(&mut codegen.mctx, temp_reg, new_reg);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fneg);
                                    }
                                    ir::instruction::UnaryOp::Not => {  
                                        panic!("Not operation is not supported for float32 constant")
                                    },
                                }
                            }
                            else {
                                panic!("Expected int32 constant for left operand");
                            }
                        }
                        _ => todo!()
                    }
                }
                // 记录结果
                codegen.table.insert(result_val, MirOperand {
                    typ: result_val.kind(codegen.ctx),
                    kind: MirOperandKind::Reg(temp_reg),
                });
            },
            ir::instruction::InstructionKind::Binary { op } => {
                let result_val = self.get_result(codegen.ctx).unwrap();
                let lhs = self.get_operand(codegen.ctx, 0).unwrap();
                let rhs = self.get_operand(codegen.ctx, 1).unwrap();
                // 创建目标寄存器
                let temp_reg = if result_val.kind(codegen.ctx).is_int(codegen.ctx) || result_val.kind(codegen.ctx).is_bool(codegen.ctx){
                    codegen.mctx.generate_vreg(regs::RegKind::Integer).into()
                } else {
                    codegen.mctx.generate_vreg(regs::RegKind::Float).into()
                };
                if !lhs.is_constant(codegen.ctx) && !rhs.is_constant(codegen.ctx) {
                    // 获取左操作数寄存器
                    let lhs_reg = codegen.generate_opd_reg(lhs);
                    // 获取右操作数寄存器
                    let rhs_reg = codegen.generate_opd_reg(rhs);
                    match op {
                        ir::instruction::BinaryOp::Add => {
                            // 生成加法指令
                            let add = MirInst::add(&mut codegen.mctx, temp_reg, lhs_reg, rhs_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add);
                        }
                        ir::instruction::BinaryOp::Fadd => {
                            // 生成浮点加法指令
                            let fadd = MirInst::fadd(&mut codegen.mctx, temp_reg, lhs_reg, rhs_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fadd);
                        }
                        ir::instruction::BinaryOp::Sub => {  
                            // 生成减法指令
                            let sub = MirInst::sub(&mut codegen.mctx, temp_reg, lhs_reg, rhs_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, sub);
                        },
                        ir::instruction::BinaryOp::FSub => {
                            // 生成浮点减法指令
                            let fsub = MirInst::fsub(&mut codegen.mctx, temp_reg, lhs_reg, rhs_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fsub);
                        }
                        ir::instruction::BinaryOp::Mul => {
                            // 生成乘法指令
                            let mul = MirInst::mul(&mut codegen.mctx, temp_reg, lhs_reg, rhs_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, mul);
                        }
                        ir::instruction::BinaryOp::Fmul => {
                            // 生成浮点乘法指令
                            let fmul = MirInst::fmul(&mut codegen.mctx, temp_reg, lhs_reg, rhs_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fmul);
                        }
                        ir::instruction::BinaryOp::Sdiv => {
                            // 生成除法指令
                            let div = MirInst::div(&mut codegen.mctx, temp_reg, lhs_reg, rhs_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, div);
                        }
                        ir::instruction::BinaryOp::Fdiv => {
                            // 生成浮点除法指令
                            let fdiv = MirInst::fdiv(&mut codegen.mctx, temp_reg, lhs_reg, rhs_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fdiv);
                        }
                        ir::instruction::BinaryOp::Srem => {
                            // 生成取余指令
                            let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                            let new1_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                            let div = MirInst::div(&mut codegen.mctx, new_reg, lhs_reg, rhs_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, div);
                            let mul = MirInst::mul(&mut codegen.mctx, new1_reg, new_reg, rhs_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, mul);
                            let sub = MirInst::sub(&mut codegen.mctx, temp_reg, lhs_reg, new1_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, sub);
                        }
                        _ => todo!("Unsupported binary operation: {:?}", op),
                    }
                } else if !lhs.is_constant(codegen.ctx) && rhs.is_constant(codegen.ctx) {
                    // 获取左操作数寄存器
                    let lhs_reg = codegen.generate_opd_reg(lhs);
                    // 处理常量右操作数
                    if let ir::value::ValueKind::Constant { value } = &rhs.deref(codegen.ctx).unwrap().kind {
                        if let ConstantValue::Int32 { value, .. } = value {
                            match op {
                                ir::instruction::BinaryOp::Add => {
                                    // 生成加法指令
                                    if *value > 4095 || *value < -4096{
                                        // let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                        let new_reg = if codegen.movedimm.contains_key(value) {
                                            codegen.movedimm[value]
                                        } else {
                                            let temp = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                            codegen.generate_movimm(temp, *value);
                                            codegen.movedimm.insert(*value, temp);
                                            temp
                                        };
                                        //codegen.generate_movimm(temp_reg, *value);
                                        let add = MirInst::add(&mut codegen.mctx, temp_reg, lhs_reg,new_reg);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add);
                                    } else{
                                        let addi = MirInst::addi(&mut codegen.mctx, temp_reg, lhs_reg, *value as i32);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, addi);
                                    }
                                },
                                ir::instruction::BinaryOp::Sub => {  
                                    // 生成减法指令
                                    if *value > 4095 || *value < -4096{
                                        // let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                        let new_reg = if codegen.movedimm.contains_key(value) {
                                            codegen.movedimm[value]
                                        } else {
                                            let temp = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                            codegen.generate_movimm(temp, *value);
                                            codegen.movedimm.insert(*value, temp);
                                            temp
                                        };
                                        //codegen.generate_movimm(temp_reg, *value);
                                        let sub = MirInst::sub(&mut codegen.mctx, temp_reg, lhs_reg,new_reg);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, sub);
                                    } else{
                                        let subi = MirInst::subi(&mut codegen.mctx, temp_reg, lhs_reg, *value as i32);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, subi);
                                    }
                                },
                                ir::instruction::BinaryOp::Mul => {
                                    // 生成乘法指令
                                    codegen.generate_muli(temp_reg, lhs_reg, *value );
                                }
                                ir::instruction::BinaryOp::Sdiv => {
                                    // 生成除法指令
                                    codegen.generate_sdivi(temp_reg, lhs_reg, *value);
                                    // if *value == 1 {
                                    //     let mov = MirInst::mov_reg(&mut codegen.mctx, temp_reg, lhs_reg);
                                    //     let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, mov);
                                    // } else if *value == -1 {
                                    //     let neg = MirInst::neg(&mut codegen.mctx, temp_reg, lhs_reg);
                                    //     let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, neg);
                                    // }else {
                                    //     codegen.generate_movimm(temp_reg, *value);
                                    //     let div = MirInst::div(&mut codegen.mctx, temp_reg, lhs_reg, temp_reg);
                                    //     let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, div);
                                    // }
                                }
                                ir::instruction::BinaryOp::Srem => {
                                    // 生成取余指令
                                    //let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                    //codegen.generate_movimm(new_reg, *value);
                                    //let div = MirInst::div(&mut codegen.mctx, temp_reg, lhs_reg, new_reg);
                                    //let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, div);
                                    codegen.generate_sdivi(temp_reg, lhs_reg, *value);
                                    let is_negetive = *value < 0;
                                    let abs_val = if is_negetive { -(*value) } else { *value };
                                    if *value == 1 {
                                        
                                    } else {
                                        if *value == 1 || *value == -1 || *value == 0 {
                                            codegen.generate_muli(temp_reg, temp_reg, *value);
                                        } else {
                                            if ((abs_val & (abs_val-1)) == 0) || (abs_val > 1 && (abs_val&(abs_val+1)) == 0) || (abs_val > 1 && ((abs_val-2)&(abs_val-1)) == 0)  {
                                                codegen.generate_muli(temp_reg, temp_reg, *value);
                                            } else if let Some((..)) = find_additive_decomposition(abs_val) {
                                                codegen.generate_muli(temp_reg, temp_reg, *value);
                                            } else {
                                                let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                                codegen.generate_movimm(new_reg, *value);
                                                let mul = MirInst::mul(&mut codegen.mctx, temp_reg, temp_reg, new_reg);
                                                let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, mul);
                                            }
                                        }
                                    } 
                                    
                                    let sub = MirInst::sub(&mut codegen.mctx, temp_reg, lhs_reg, temp_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, sub);
                                }
                                _ => todo!("Unsupported binary operation: {:?}", op),
                            }
                        } else if let ConstantValue::Float32 { value, .. } = value{
                            let bits = value.to_bits();
                            let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                            codegen.generate_movimm(new_reg, bits as i32);
                            let fmov = MirInst::fmov(&mut codegen.mctx, temp_reg, new_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fmov);
                            match op {
                                ir::instruction::BinaryOp::Fadd => {
                                    // 生成浮点加法指令
                                    let fadd = MirInst::fadd(&mut codegen.mctx, temp_reg, lhs_reg, temp_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fadd);
                                },
                                ir::instruction::BinaryOp::FSub => {  
                                    // 生成浮点减法指令
                                    let fsub = MirInst::fsub(&mut codegen.mctx, temp_reg, lhs_reg, temp_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fsub);
                                },
                                ir::instruction::BinaryOp::Fmul => {
                                    // 生成乘法指令
                                    let fmul = MirInst::fmul(&mut codegen.mctx, temp_reg, lhs_reg, temp_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fmul);
                                }
                                ir::instruction::BinaryOp::Fdiv => {
                                    // 生成除法指令
                                    let fdiv = MirInst::fdiv(&mut codegen.mctx, temp_reg, lhs_reg, temp_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fdiv);
                                }
                                _ => todo!("Unsupported binary operation: {:?}", op),
                            }
                        } else {
                            panic!("Expected int32 or float32 constant for right operand");
                        }
                    } else {
                        todo!();
                    }
                } else if lhs.is_constant(codegen.ctx) && rhs.is_constant(codegen.ctx) {
                    if lhs.kind(codegen.ctx).is_int(codegen.ctx) || result_val.kind(codegen.ctx).is_bool(codegen.ctx){ 
                        let l_value;
                        let r_value;
                        let imm: i32;
                        if let ir::value::ValueKind::Constant { value } = &lhs.deref(codegen.ctx).unwrap().kind {
                            if let ConstantValue::Int32 { value, .. } = value {
                                l_value = *value;
                            } else {
                                panic!("Expected int32 constant for left operand");
                            }
                        } else {
                            panic!("Expected int32 constant for left operand");
                        }
                        if let ir::value::ValueKind::Constant { value } = &rhs.deref(codegen.ctx).unwrap().kind {
                            if let ConstantValue::Int32 { value, .. } = value {
                                r_value = *value;
                            } else {
                                panic!("Expected int32 constant for left operand");
                            }
                        } else {
                            panic!("Expected int32 constant for left operand");
                        }
                        match op {
                            ir::instruction::BinaryOp::Add => imm = l_value + r_value,     
                            ir::instruction::BinaryOp::Sub => imm = l_value - r_value,
                            ir::instruction::BinaryOp::Mul => imm = l_value * r_value,
                            ir::instruction::BinaryOp::Sdiv => imm = l_value / r_value,
                            ir::instruction::BinaryOp::Srem => imm = l_value % r_value,
                            _ => todo!("Unsupported binary operation: {:?}", op),
                        }
                        codegen.generate_movimm(temp_reg, imm);
                    } else {
                        let l_value;
                        let r_value;
                        let imm: f32;
                        if let ir::value::ValueKind::Constant { value } = &lhs.deref(codegen.ctx).unwrap().kind {
                            if let ConstantValue::Float32 { value, .. } = value {
                                l_value = *value;
                            } else {
                                panic!("Expected f32 constant for left operand");
                            }
                        } else {
                            panic!("Expected f32 constant for left operand");
                        }
                        if let ir::value::ValueKind::Constant { value } = &rhs.deref(codegen.ctx).unwrap().kind {
                            if let ConstantValue::Float32 { value, .. } = value {
                                r_value = *value;
                            } else {
                                panic!("Expected f32 constant for left operand");
                            }
                        } else {
                            panic!("Expected f32 constant for left operand");
                        }
                        match op {
                            ir::instruction::BinaryOp::Fadd => {
                                imm = l_value + r_value;
                            }
                            ir::instruction::BinaryOp::FSub => {
                                imm = l_value - r_value;
                            },
                            ir::instruction::BinaryOp::Fmul => {
                                imm = l_value * r_value;
                            }
                            ir::instruction::BinaryOp::Fdiv => {
                                imm = l_value / r_value;
                            }
                            _ => todo!("Unsupported binary operation: {:?}", op),
                        }
                        let bits = imm.to_bits();
                        let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                        codegen.generate_movimm(new_reg, bits as i32);
                        let fmov = MirInst::fmov(&mut codegen.mctx, temp_reg, new_reg);
                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fmov);
                    }
                } else {
                    // 获取右操作数寄存器
                    let rhs_reg = codegen.generate_opd_reg(rhs);
                    // 处理常量左操作数
                    if let ir::value::ValueKind::Constant { value } = &lhs.deref(codegen.ctx).unwrap().kind {
                        if let ConstantValue::Int32 { value, .. } = value {
                            match op {
                                ir::instruction::BinaryOp::Add => {
                                    // 生成加法指令
                                    if *value > 4095 || *value < -4096 {
                                        let new_reg = if codegen.movedimm.contains_key(value) {
                                            codegen.movedimm[value]
                                        } else {
                                            let temp = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                            codegen.generate_movimm(temp, *value);
                                            codegen.movedimm.insert(*value, temp);
                                            temp
                                        };
                                        //codegen.generate_movimm(temp_reg, *value);
                                        let add = MirInst::add(&mut codegen.mctx, temp_reg, new_reg,rhs_reg);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add);
                                    } else {
                                        let addi = MirInst::addi(&mut codegen.mctx, temp_reg, rhs_reg, *value);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, addi);
                                    }
                                }
                                ir::instruction::BinaryOp::Sub => {  
                                    // 生成减法指令
                                    let new_reg = if codegen.movedimm.contains_key(value) {
                                        codegen.movedimm[value]
                                    } else {
                                        let temp = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                        codegen.generate_movimm(temp, *value);
                                        codegen.movedimm.insert(*value, temp);
                                        temp
                                    };
                                    //codegen.generate_movimm(temp_reg, *value);
                                    let sub = MirInst::sub(&mut codegen.mctx, temp_reg, new_reg, rhs_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, sub);
                                },
                                ir::instruction::BinaryOp::Mul => {
                                    // 生成乘法指令
                                    codegen.generate_muli(temp_reg, rhs_reg, *value);
                                }
                                ir::instruction::BinaryOp::Sdiv => {
                                    // 生成除法指令
                                    let new_reg = if codegen.movedimm.contains_key(value) {
                                        codegen.movedimm[value]
                                    } else {
                                        let temp = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                        codegen.generate_movimm(temp, *value);
                                        codegen.movedimm.insert(*value, temp);
                                        temp
                                    };
                                    //codegen.generate_movimm(temp_reg, *value);
                                    let div = MirInst::div(&mut codegen.mctx, temp_reg, new_reg, rhs_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, div);
                                }
                                ir::instruction::BinaryOp::Srem => {
                                    codegen.generate_movimm(temp_reg, *value);
                                    let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                    let div = MirInst::div(&mut codegen.mctx, new_reg, temp_reg, rhs_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, div);
                                    let mul = MirInst::mul(&mut codegen.mctx, new_reg, new_reg, rhs_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, mul);
                                    let sub = MirInst::sub(&mut codegen.mctx, temp_reg, temp_reg, new_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, sub);
                                }
                                _ => todo!("Unsupported binary operation: {:?}", op),
                            }
                        } else if let ConstantValue::Float32 { value, .. } = value{
                            let bits = value.to_bits();
                            let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                            codegen.generate_movimm(new_reg, bits as i32);
                            let fmov = MirInst::fmov(&mut codegen.mctx, temp_reg, new_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fmov);
                            match op {
                                ir::instruction::BinaryOp::Fadd => {
                                    // 生成浮点加法指令
                                    let fadd = MirInst::fadd(&mut codegen.mctx, temp_reg, temp_reg, rhs_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fadd);
                                }
                                ir::instruction::BinaryOp::FSub => {
                                    // 生成浮点减法指令
                                    let fsub = MirInst::fsub(&mut codegen.mctx, temp_reg, temp_reg, rhs_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fsub);
                                }
                                ir::instruction::BinaryOp::Fmul => {
                                    // 生成浮点乘法指令
                                    let fmul = MirInst::fmul(&mut codegen.mctx, temp_reg, temp_reg, rhs_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fmul);
                                }
                                ir::instruction::BinaryOp::Fdiv => {
                                    // 生成浮点除法指令
                                    let fdiv = MirInst::fdiv(&mut codegen.mctx, temp_reg, temp_reg, rhs_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fdiv);
                                }
                                _ =>  todo!(),
                            }
                        } else {
                            panic!("Unsupported constant type: {:?}", value);
                        }
                    } else {
                        panic!("Expected int32 constant for left operand");
                    }
                }
                // 记录结果
                codegen.table.insert(result_val, MirOperand {
                    typ: result_val.kind(codegen.ctx),
                    kind: MirOperandKind::Reg(temp_reg),
                });
            },
            ir::instruction::InstructionKind::BitBinary { op: _ } => todo!(),
            ir::instruction::InstructionKind::MemAccess { op } => {
                match op {
                    MemAccessOp::Alloca { typ } => {
                        // 在栈上分配空间
                        let mut size = typ.bitwidth(codegen.ctx);
                        size = (size + 7) / 8;
                        let current_offset = codegen.curr_func.unwrap().current_stack_size(&codegen.mctx);
                        let _ = codegen.curr_func.unwrap().add_current_stack_size(&mut codegen.mctx, size as i32);
                        let total_stack = codegen.curr_func.unwrap().calleeargs_stack_size(&codegen.mctx);

                        // 数组将基地址放到寄存器
                        if typ.is_array(codegen.ctx) {
                            let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Address).into();
                            if total_stack+current_offset > 4095 {
                                codegen.generate_movimm(new_reg, total_stack+current_offset);
                                let add = MirInst::add(&mut codegen.mctx, new_reg, regs::sp().into(), new_reg);
                                let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add);
                            } else {
                                let addi = MirInst::addi(&mut codegen.mctx, new_reg, regs::sp().into(), total_stack+current_offset);
                                let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, addi);
                            }
                            let mem_loc = MemLoc::Reg { address: new_reg };
                            let mopd = MirOperand {
                                typ,
                                kind: MirOperandKind::Mem(mem_loc),
                            };
                            codegen.table.insert(self.get_result(codegen.ctx).unwrap(), mopd);
                        } else{ 
                            // 其余用地址偏移
                            let mem_loc = MemLoc::RegOffset { base: regs::sp().into(), offset: total_stack+current_offset };
                            let mopd = MirOperand {
                                typ,
                                kind: MirOperandKind::Mem(mem_loc),
                            };
                            codegen.table.insert(self.get_result(codegen.ctx).unwrap(), mopd);
                        }
                    }
                    MemAccessOp::Store => {
                        //println!("codegen Store => inst: {}", self.display(codegen.ctx));
                        let val = self.get_operand(codegen.ctx, 0).unwrap();
                        let ptr = self.get_operand(codegen.ctx, 1).unwrap();
                        // 检查是否是全局变量
                        let is_global_ptr = 
                            if let ir::value::ValueKind::Constant { value} = &ptr.deref(codegen.ctx).unwrap().kind {
                                matches!(value, ConstantValue::GlobalPtr { .. })
                            }
                            else {
                                false
                            };
                        let mem_loc = if is_global_ptr {
                            // 提取全局变量名称
                            let name = 
                                if let ir::value::ValueKind::Constant { 
                                    value: ConstantValue::GlobalPtr { name, .. } 
                                } = &ptr.deref(codegen.ctx).unwrap().kind {name.clone()} 
                                else {
                                    unreachable!()
                                };
                            let label = codegen.globals[&name].clone();
                            let temp_reg = codegen.mctx.generate_vreg(regs::RegKind::Address).into();
                            // 生成加载全局地址的指令
                            codegen.generate_adrp(temp_reg, label);

                            // 生成加载实际值的指令
                            let mem = MemLoc::RegOffset { base: temp_reg, offset: 0 };
                            mem
                        }else {
                            // 处理局部变量存储
                            let mem = match codegen.table[&ptr].kind {
                                MirOperandKind::Mem(m) => m,
                                _ => unreachable!(),
                            };
                            mem
                        };
                        match &val.deref(codegen.ctx).unwrap().kind {
                            ir::value::ValueKind::Constant { value } => {
                                let zero_reg = regs::wzr().into();
                                match value {
                                    ConstantValue::Int32 { value, .. } => {
                                        // 使用零寄存器或加载立即数
                                        if *value == 0 {
                                            codegen.generate_str(zero_reg, mem_loc);
                                        } else {
                                            let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                            codegen.generate_movimm(new_reg, *value as i32);   
                                            codegen.generate_str(new_reg, mem_loc);                                         
                                        }
                                    }
                                    ConstantValue::Bool { value, .. } => {
                                        if *value == false {
                                            codegen.generate_str(zero_reg, mem_loc);                                         
                                        } else {
                                            let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                            codegen.generate_movimm(new_reg, 1);
                                            codegen.generate_str(new_reg, mem_loc);                                         
                                        }
                                    }
                                    ConstantValue::Undef{typ:_} => {

                                    } 
                                    ConstantValue::Zero { typ:_ }=> {
                                        codegen.generate_str(zero_reg, mem_loc);                                         
                                    }
                                    ConstantValue::Float32 { value, .. } => {
                                        let bits = value.to_bits();
                                        let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                        codegen.generate_movimm(new_reg, bits as i32);
                                        let temp_reg = codegen.mctx.generate_vreg(regs::RegKind::Float).into();
                                        let fmov = MirInst::fmov(&mut codegen.mctx, temp_reg, new_reg);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fmov).unwrap();
                                        codegen.generate_str(temp_reg, mem_loc);                                         
                                    }
                                    _ => todo!(),
                                }
                            }
                            ir::value::ValueKind::InstResult { ..} => {
                                let mopd = codegen.table[&val];
                                match mopd.kind {
                                    MirOperandKind::Reg(reg) => {
                                        codegen.generate_str(reg, mem_loc);
                                    }
                                    MirOperandKind::Mem(MemLoc::Reg { address }) => {
                                        codegen.generate_str(address, mem_loc);
                                    }
                                    MirOperandKind::Mem(MemLoc::RegOffset { base, offset }) => {
                                        let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Address).into();    
                                        if offset > 4095 || offset < -4096 {
                                            codegen.generate_movimm(new_reg, offset);
                                            let add = MirInst::add(&mut codegen.mctx, new_reg, base, new_reg);
                                            codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add).unwrap();
                                        } else {
                                            let add = MirInst::addi(&mut codegen.mctx, new_reg, base, offset);
                                            codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add).unwrap();
                                        }
                                        let mem_loc = MemLoc::Reg { address: new_reg };
                                        codegen.generate_str(new_reg, mem_loc);
                                        
                                        codegen.table.insert(val, MirOperand {
                                            typ: val.kind(codegen.ctx),
                                            kind: MirOperandKind::Mem(mem_loc),
                                        });
                                        codegen.release.insert(val);

                                    }
                                    MirOperandKind::Mem(MemLoc::Reg2Offset { base, offset }) => {
                                        let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Address).into();    
                                        let add = MirInst::add(&mut codegen.mctx, new_reg, base, offset);
                                        codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add).unwrap();
                                        codegen.generate_str(new_reg, mem_loc);

                                        let mem_loc = MemLoc::Reg { address: new_reg };
                                         codegen.table.insert(val, MirOperand {
                                            typ: val.kind(codegen.ctx),
                                            kind: MirOperandKind::Mem(mem_loc),
                                        });
                                        codegen.release.insert(val);
                                    }
                                }
                            }
                            ir::value::ValueKind::Parameter { function:_, index, typ:_ }=>{
                                if codegen.table.get(&val).is_some() {
                                    let arg_reg = match codegen.table[&val].kind {
                                        MirOperandKind::Reg(reg) => reg,
                                        MirOperandKind::Mem(MemLoc::Reg { address }) => address,
                                        _ => todo!(),
                                    };
                                    codegen.generate_str(arg_reg, mem_loc);
                                } else {
                                    if *index < 8 {
                                        panic!("Unreachable")
                                    } else {
                                        // 大于八个的参数在栈上，通过fp获取
                                        let argadress_reg = if val.is_ptr(codegen.ctx) {
                                                codegen.mctx.generate_vreg(regs::RegKind::Address).into()
                                            } else if val.kind(codegen.ctx).is_float(codegen.ctx){
                                                codegen.mctx.generate_vreg(regs::RegKind::Float).into()
                                            } else{
                                                codegen.mctx.generate_vreg(regs::RegKind::Integer).into()
                                            };
                                        codegen.generate_ldr(argadress_reg, MemLoc::RegOffset { base: regs::fp().into(), offset: ((*index - 8) * 8) as i32 });
                                        codegen.generate_str(argadress_reg, mem_loc);
                                        let mem_loc = if val.is_ptr(codegen.ctx) { 
                                            MirOperandKind::Mem(MemLoc::Reg{address: argadress_reg})
                                        } else {
                                            MirOperandKind::Reg(argadress_reg)
                                        };
                                        codegen.table.insert(val, MirOperand {
                                            typ: val.kind(codegen.ctx),
                                            kind: mem_loc,
                                        });
                                        codegen.release.insert(val);
                                        
                                    }
                                }
                            }
                            _ => todo!()
                        }
                    }       
                    MemAccessOp::Load => {
                        let ptr_val = self.get_operand(codegen.ctx, 0).unwrap();
                        let result_val = self.get_result(codegen.ctx).unwrap();
                        // 检查是否是全局变量
                        let is_global_ptr = 
                            if let ir::value::ValueKind::Constant { value} = &ptr_val.deref(codegen.ctx).unwrap().kind {
                                matches!(value, ConstantValue::GlobalPtr { .. })
                            }
                            else {
                                false
                            };
                        let mem_loc = if is_global_ptr {
                            // 提取全局变量名称
                            let name = 
                                if let ir::value::ValueKind::Constant { 
                                    value: ConstantValue::GlobalPtr { name, .. } 
                                } = &ptr_val.deref(codegen.ctx).unwrap().kind {name.clone()} 
                                else {
                                    unreachable!()
                                };
                            // 处理全局变量加载
                            let label = codegen.globals[&name].clone();
                            let temp_reg = codegen.mctx.generate_vreg(regs::RegKind::Address).into();
                            
                            // 生成加载全局地址的指令
                            codegen.generate_adrp(temp_reg, label);
                            // let mov_label = MirInst::mov_label(&mut codegen.mctx, temp_reg, label);
                            // let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, mov_label).unwrap();
                            
                            // 生成加载实际值的指令
                            let mem = MemLoc::RegOffset { base: temp_reg, offset: 0 };
                            mem
                        } else {
                            let mem = match codegen.table[&ptr_val].kind {
                                MirOperandKind::Mem(m) => m,
                                _ => unreachable!(),
                            };
                            mem
                        };
                        // let size = (result_val.kind(codegen.ctx).bitwidth(codegen.ctx) + 7) / 8;

                        let temp_reg = if result_val.is_ptr(codegen.ctx) { 
                            codegen.mctx.generate_vreg(regs::RegKind::Address).into()
                        } else if result_val.kind(codegen.ctx).is_float(codegen.ctx) {
                            codegen.mctx.generate_vreg(regs::RegKind::Float).into()
                        } else {
                            codegen.mctx.generate_vreg(regs::RegKind::Integer).into()
                        };
                        codegen.generate_ldr(temp_reg, mem_loc);
                        
                        // 记录结果
                        let mem_loc = if result_val.is_ptr(codegen.ctx) { 
                            MirOperandKind::Mem(MemLoc::Reg{address: temp_reg})
                        } else {
                            MirOperandKind::Reg(temp_reg)
                        };
                        codegen.table.insert(result_val, MirOperand {
                            typ: result_val.kind(codegen.ctx),
                            kind: mem_loc,
                        });
                    } 
                    MemAccessOp::GetElementPtr { typ: _ } => {
                        let result_val = self.get_result(codegen.ctx).unwrap();
                        let base_ptr = self.get_operand(codegen.ctx, 0).unwrap();
                        // 需要判断是不是全局数组
                        let is_global_ptr = if let ir::value::ValueKind::Constant { value} = &base_ptr.deref(codegen.ctx).unwrap().kind {
                                matches!(value, ConstantValue::GlobalPtr { .. })
                            }
                            else {
                                false
                            };
                        // 判断是不是数组
                        let is_param = base_ptr.is_parameter(codegen.ctx);
                        let marked = if codegen.table.get(&base_ptr).is_some() {
                            match codegen.table[&base_ptr].kind {
                                MirOperandKind::Mem(MemLoc::Reg { .. }) => {
                                    true
                                },
                                _ => false
                            }
                        } else {
                            false
                        };      
                        // 获取数组偏移
                        let indices = (1..self.get_operands(codegen.ctx).len()).map(|i| self.get_operand(codegen.ctx, i).unwrap()).collect::<Vec<_>>();
                        // 计算偏移量
                        let total_offset_reg= codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                        let temp_index_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                        
                        let mut now_offset = 0;
                        let mut flag = false;
                        let current_typ = base_ptr.kind(codegen.ctx);
                        // 第一个索引用于指针偏移（通常是0），后续索引用于结构体/数组
                        for (i, &index_val) in indices.iter().enumerate(){
                            // 常量索引值处理
                            if let ir::value::ValueKind::Constant { value: ConstantValue::Int32 { value, .. } } = index_val.deref(codegen.ctx).unwrap().kind {
                                let mut cur_byte = if current_typ.get_ptr_indices_bitwidth(codegen.ctx, i).is_some() {
                                    current_typ.get_ptr_indices_bitwidth(codegen.ctx, i).unwrap()
                                } else {
                                    current_typ.bitwidth(codegen.ctx)
                                };
                                cur_byte = (cur_byte + 7) / 8;
                                let cur_offset = value * cur_byte as i32;
                                now_offset += cur_offset;
                            }
                            // 变量索引值处理
                            else {
                                let index_reg = if codegen.table.get(&index_val).is_some() {
                                    let index_opd = codegen.table[&index_val];
                                    match index_opd.kind {
                                        MirOperandKind::Reg(reg) => reg,
                                        _ => panic!("Expected register operand for binary op"),
                                    }
                                } else {
                                    match &index_val.deref(codegen.ctx).unwrap().kind {
                                        ir::value::ValueKind::Parameter{ function:_, index, typ:_ } => {
                                            if *index < 8 {
                                                panic!("unreachable")
                                            } else {
                                                // 大于八个的参数在栈上，通过fp获取
                                                let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                                codegen.generate_ldr(new_reg, MemLoc::RegOffset { base: regs::fp().into(), offset: ((*index - 8) * 8) as i32 });
                                                let mem_loc = MirOperandKind::Reg(new_reg);
                                                codegen.table.insert(index_val, MirOperand {
                                                    typ: index_val.kind(codegen.ctx),
                                                    kind: mem_loc,
                                                });
                                                codegen.release.insert(index_val);
                                                new_reg
                                            }
                                        }
                                        _ => todo!(),
                                    }
                                };
                                // 获取当前维度元素大小
                                let mut element_size = if current_typ.get_ptr_indices_bitwidth(codegen.ctx, i).is_some() {
                                    current_typ.get_ptr_indices_bitwidth(codegen.ctx, i).unwrap()
                                } else {
                                    current_typ.bitwidth(codegen.ctx)
                                };
                                element_size = (element_size + 7) / 8;

                                if flag == false {
                                    if (element_size & (element_size - 1)) == 0 {
                                        let shift = element_size.trailing_zeros();
                                        let lsl = MirInst::lsl(&mut codegen.mctx, total_offset_reg, index_reg, shift);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, lsl).unwrap();
                                    } else {
                                        // codegen.generate_movimm(total_offset_reg, element_size as i32);
                                        // let mul = MirInst::mul(&mut codegen.mctx, total_offset_reg, total_offset_reg, index_reg);
                                        codegen.generate_muli(total_offset_reg, index_reg, element_size as i32);
                                        //let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, mul).unwrap();
                                    }
                                    flag = true;
                                } else {
                                    if (element_size & (element_size - 1)) == 0 {
                                        let shift = element_size.trailing_zeros();
                                        let lsl = MirInst::lsl(&mut codegen.mctx, temp_index_reg, index_reg, shift);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, lsl).unwrap();
                                        let add = MirInst::add(&mut codegen.mctx, total_offset_reg, total_offset_reg, temp_index_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add).unwrap();
                                    } else {
                                        //codegen.generate_movimm(temp_index_reg, element_size as i32);
                                        // let mul = MirInst::mul(&mut codegen.mctx, temp_index_reg, temp_index_reg, index_reg);
                                        // let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, mul).unwrap();
                                        // let add = MirInst::add(&mut codegen.mctx, total_offset_reg, total_offset_reg, temp_index_reg);
                                        // let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add).unwrap();

                                        // let madd = MirInst::madd(&mut codegen.mctx, total_offset_reg,  temp_index_reg, index_reg, total_offset_reg);
                                        // let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, madd).unwrap();

                                        codegen.generate_muli(temp_index_reg, index_reg, element_size as i32);
                                        let add = MirInst::add(&mut codegen.mctx, total_offset_reg, total_offset_reg, temp_index_reg);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add).unwrap();
                                    }
                                    
                                }
                            }
                        }
                        // 常量偏移
                        if flag==false {
                            codegen.generate_movimm(total_offset_reg, now_offset);
                        } else if now_offset != 0 {
                            if now_offset > 4095 || now_offset < -4096 {
                                codegen.generate_movimm(temp_index_reg, now_offset);
                                let add = MirInst::add(&mut codegen.mctx, total_offset_reg, total_offset_reg, temp_index_reg);
                                let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add).unwrap();
                            } else {
                                let addi = MirInst::addi(&mut codegen.mctx, total_offset_reg, total_offset_reg, now_offset);
                                let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, addi).unwrap();
                            }
                        }
                        
                        let base_reg;
                        // 获取基址指针
                        if marked {
                            match codegen.table[&base_ptr].kind {
                                MirOperandKind::Mem(MemLoc::Reg { address }) => {
                                    base_reg = address;
                                },
                                _ => panic!("Unexpected operand kind"),
                            }
                        } else if is_global_ptr {
                            base_reg = codegen.mctx.generate_vreg(regs::RegKind::Address).into();
                            // 提取全局变量名称
                            let name = 
                                if let ir::value::ValueKind::Constant { 
                                    value: ConstantValue::GlobalPtr { name, .. } 
                                } = &base_ptr.deref(codegen.ctx).unwrap().kind {name.clone()} 
                                else {
                                    unreachable!()
                                };
                            let label = codegen.globals[&name].clone();
                            // 生成加载全局地址的指令
                            codegen.generate_adrp(base_reg, label);
                            let mem_loc = MemLoc::Reg { address: base_reg };
                            let mopd = MirOperand {
                                typ: result_val.kind(codegen.ctx),
                                kind: MirOperandKind::Mem(mem_loc),
                            };
                            codegen.table.insert(base_ptr, mopd);
                            codegen.release.insert(base_ptr);
                        } else if is_param {
                            match &base_ptr.deref(codegen.ctx).unwrap().kind {
                                ir::value::ValueKind::Parameter { function:_, index, typ:_ }=>{
                                    if *index < 8 {
                                        panic!("Unreachable")
                                    } else {
                                        // 大于八个的参数在栈上，通过fp获取
                                        base_reg = codegen.mctx.generate_vreg(regs::RegKind::Address).into();
                                        codegen.generate_ldr(base_reg, MemLoc::RegOffset { base: regs::fp().into(), offset: ((*index - 8) * 8) as i32 });
                                        let mem_loc = MirOperandKind::Mem(MemLoc::Reg{address: base_reg});
                                        codegen.table.insert(base_ptr, MirOperand {
                                            typ: base_ptr.kind(codegen.ctx),
                                            kind: mem_loc,
                                        });
                                        codegen.release.insert(base_ptr);
                                    }
                                },
                                _ => panic!("Unreachable")
                            }
                        } else {
                            base_reg = codegen.mctx.generate_vreg(regs::RegKind::Address).into();
                            let base_mem_loc = codegen.table[&base_ptr];
                            match base_mem_loc.kind {
                                MirOperandKind::Mem(MemLoc::Reg2Offset { base, offset }) => {
                                    let add: MirInst = MirInst::add(&mut codegen.mctx, base_reg, base, offset);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add).unwrap();
                                },
                                MirOperandKind::Mem(MemLoc::RegOffset { base, offset }) => {
                                    if offset > 4095 || offset < -4096 {
                                        let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                        codegen.generate_movimm(new_reg, offset);
                                        let add = MirInst::add(&mut codegen.mctx, base_reg, base, new_reg);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add).unwrap();
                                    } else {
                                        let addi = MirInst::addi(&mut codegen.mctx, base_reg, base, offset);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, addi).unwrap();
                                    } 
                                }
                                _ => panic!("GEP base error"),
                            }
                        }
                        // 生成加载实际值的指令
                        let result_reg = codegen.mctx.generate_vreg(regs::RegKind::Address).into();
                        let add = MirInst::add(&mut codegen.mctx, result_reg, base_reg, total_offset_reg);
                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add).unwrap();
                        let mem_loc = MemLoc::Reg { address: result_reg };
                        
                        let mopd = MirOperand {
                            typ: result_val.kind(codegen.ctx),
                            kind: MirOperandKind::Mem(mem_loc),
                        };
                        codegen.table.insert(result_val, mopd); 

                        // let mem_loc = MemLoc::Reg2Offset { base: base_reg, offset: total_offset_reg };                        
                        // let mopd = MirOperand {
                        //     typ: result_val.kind(codegen.ctx),
                        //     kind: MirOperandKind::Mem(mem_loc),
                        // };
                        // codegen.table.insert(result_val, mopd); 
                    }
                }
            },
            ir::instruction::InstructionKind::Conversion { op} => {
                let result_val = self.get_result(codegen.ctx).unwrap();
                let src = self.get_operand(codegen.ctx, 0).unwrap();
                match op {
                    ir::instruction::ConversionOp::Bitcast => {
                        let mem_loc = match codegen.table[&src].kind {
                            MirOperandKind::Mem(mem) => mem,
                            _ => unreachable!(),
                        };
                        let mopd = MirOperand {
                            typ: result_val.kind(codegen.ctx),
                            kind: MirOperandKind::Mem(mem_loc),
                        };
                        codegen.table.insert(result_val, mopd);
                    }
                    _ => {
                        if !src.is_constant(codegen.ctx) {
                            let src_reg = codegen.generate_opd_reg(src);
                            match op {
                                ir::instruction::ConversionOp::Trunc => {
                                    todo!();
                                }
                                ir::instruction::ConversionOp::ZExt => {
                                    let result_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                    let mov_reg = MirInst::mov_reg(&mut codegen.mctx, result_reg, src_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, mov_reg);
                                    let mopd = MirOperand {
                                        typ: result_val.kind(codegen.ctx),
                                        kind: MirOperandKind::Reg(result_reg),
                                    };
                                    codegen.table.insert(result_val, mopd);
                                }
                                ir::instruction::ConversionOp::FpToSi => {
                                    let result_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                    let fcvtzs = MirInst::fcvtzs(&mut codegen.mctx, result_reg, src_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fcvtzs);
                                    let mopd = MirOperand {
                                        typ: result_val.kind(codegen.ctx),
                                        kind: MirOperandKind::Reg(result_reg),
                                    };
                                    codegen.table.insert(result_val, mopd);
                                }
                                ir::instruction::ConversionOp::SiToFp => {
                                    let result_reg = codegen.mctx.generate_vreg(regs::RegKind::Float).into();
                                    let scvtf = MirInst::scvtf(&mut codegen.mctx, result_reg, src_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, scvtf);
                                    let mopd = MirOperand {
                                        typ: result_val.kind(codegen.ctx),
                                        kind: MirOperandKind::Reg(result_reg),
                                    };
                                    codegen.table.insert(result_val, mopd);
                                }
                                _ => todo!(),
                            }
                        } else {
                            match op {
                                ir::instruction::ConversionOp::Trunc => {
                                    todo!();
                                }
                                ir::instruction::ConversionOp::ZExt => {
                                    todo!();
                                }
                                ir::instruction::ConversionOp::FpToSi => {
                                    let imm = src.get_float_const_value(codegen.ctx).unwrap();
                                    // let bits = imm.to_bits();
                                    let result_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                    let offset = imm as i32;
                                    codegen.generate_movimm(result_reg, offset);
                                    // codegen.generate_movimm(result_reg, bits as i32);
                                    // let temp_reg = codegen.mctx.generate_vreg(regs::RegKind::Float).into();
                                    // let fmov = MirInst::fmov(&mut codegen.mctx, temp_reg, result_reg);
                                    // let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fmov);
                                    // let fcvtzs = MirInst::fcvtzs(&mut codegen.mctx, result_reg, temp_reg);
                                    // let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fcvtzs);
                                    let mopd = MirOperand {
                                        typ: result_val.kind(codegen.ctx),
                                        kind: MirOperandKind::Reg(result_reg),
                                    };
                                    codegen.table.insert(result_val, mopd);
                                }
                                ir::instruction::ConversionOp::SiToFp => {
                                    let imm = src.get_int_const_value(codegen.ctx).unwrap();
                                    let result_reg = codegen.mctx.generate_vreg(regs::RegKind::Float).into();
                                    let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                    codegen.generate_movimm(new_reg, imm);
                                    let scvtf = MirInst::scvtf(&mut codegen.mctx, result_reg, new_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, scvtf);
                                    let mopd = MirOperand {
                                        typ: result_val.kind(codegen.ctx),
                                        kind: MirOperandKind::Reg(result_reg),
                                    };
                                    codegen.table.insert(result_val, mopd);
                                }
                                _ => todo!(),
                            }
                        }
                    }
                }
            }
            ir::instruction::InstructionKind::IComp { cond } => {
                // 将左右操作数相见存到结果寄存器
                let result_val = self.get_result(codegen.ctx).unwrap();
                let lhs = self.get_operand(codegen.ctx, 0).unwrap();
                let rhs = self.get_operand(codegen.ctx, 1).unwrap();
                let bzw_reg = regs::bzw().into();
                let lhs_reg = if codegen.table.get(&lhs).is_some() {
                        let lhs_opd = codegen.table[&lhs];
                        match lhs_opd.kind {
                            MirOperandKind::Reg(reg) => reg,
                            _ => panic!("Expected register operand for left operand"),
                        }
                    } else {
                        match &lhs.deref(codegen.ctx).unwrap().kind {
                            ir::value::ValueKind::Constant { value } => {
                                let temp_reg0 = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                if let ConstantValue::Int32 { value, .. } = value {
                                    codegen.generate_movimm(temp_reg0, *value);
                                } else if let ConstantValue::Bool { typ:_, value } = value {
                                    codegen.generate_movimm(temp_reg0, *value as i32);
                                }else {
                                    panic!("Expected int32 constant for left operand");
                                }
                                temp_reg0
                            }
                            ir::value::ValueKind::Parameter{ function:_, index, typ:_ } => {
                                if *index < 8 {
                                    panic!("unreachable")
                                } else {
                                    // 大于八个的参数在栈上，通过fp获取
                                    let temp_reg0 = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                    codegen.generate_ldr(temp_reg0, MemLoc::RegOffset { base: regs::fp().into(), offset: ((*index - 8) * 8) as i32 });

                                    let mem_loc = MirOperandKind::Reg(temp_reg0);
                                    codegen.table.insert(lhs, MirOperand {
                                        typ: lhs.kind(codegen.ctx),
                                        kind: mem_loc,
                                    });
                                    codegen.release.insert(lhs);
                                    temp_reg0
                                }
                            }
                            _ => todo!()
                        }
                    };
                // 查看右操作数在不在表中
                if let Some(rhs_opd) = codegen.table.get(&rhs) {
                    // 如果在表中，则直接使用寄存器
                    let rhs_reg = match rhs_opd.kind {
                        MirOperandKind::Reg(reg) => reg,
                        _ => panic!("Expected register operand for right operand"),
                    };
                    // 生成cmp指令
                    let cmp = MirInst::cmp(&mut codegen.mctx, lhs_reg, rhs_reg, bzw_reg); 
                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cmp);
                } else {
                    // 处理常量右操作数
                    let temp_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                    match &rhs.deref(codegen.ctx).unwrap().kind {
                        ir::value::ValueKind::Constant { value } => {
                            if let ConstantValue::Int32 { value, .. } = value {
                                if *value > 4095 || *value < -4096 {
                                    codegen.generate_movimm(temp_reg, *value);
                                    let cmp = MirInst::cmp(&mut codegen.mctx, lhs_reg, temp_reg, bzw_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cmp);
                                } else {
                                    let cmpi = MirInst::cmpi(&mut codegen.mctx, lhs_reg, *value, bzw_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cmpi);
                                }
                            } else if let ConstantValue::Bool {  value, ..} = value {
                                if *value == true {
                                    let cmpi = MirInst::cmpi(&mut codegen.mctx,lhs_reg, 1, bzw_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cmpi);
                                } else {
                                    let cmpi = MirInst::cmpi(&mut codegen.mctx, lhs_reg,0, bzw_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cmpi);
                                }
                            }else {
                                panic!("Expected int32 constant for left operand");
                            }
                        }
                        ir::value::ValueKind::Parameter{ function:_, index, typ:_ } => {
                            if *index < 8 {
                                panic!("unreachable")
                            } else {
                                // 大于八个的参数在栈上，通过fp获取
                                codegen.generate_ldr(temp_reg, MemLoc::RegOffset { base: regs::fp().into(), offset: ((*index - 8) * 8) as i32 });

                                let mem_loc = MirOperandKind::Reg(temp_reg);
                                codegen.table.insert(rhs, MirOperand {
                                    typ: rhs.kind(codegen.ctx),
                                    kind: mem_loc,
                                });
                                codegen.release.insert(rhs);
                                let cmp = MirInst::cmp(&mut codegen.mctx, lhs_reg,temp_reg, bzw_reg);
                                let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cmp);
                            }
                        }
                        _ => todo!()
                    }
                }
                
                //判断后继是否为有条件跳转指令
                let succ_is_cbr = if let Some(ir_inst) = self.succ(codegen.ctx) {
                    ir_inst.is_cbr(codegen.ctx)
                } else {
                    false
                };
                if !succ_is_cbr {
                    //根据cond类型为调整temp_reg
                    let temp_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                    match cond {
                        ir::instruction::ICompCond::Eq => {
                            let cset = MirInst::cset(&mut codegen.mctx, temp_reg, Condition::eq(), bzw_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cset);
                        },
                        ir::instruction::ICompCond::Ne => {
                            let cset = MirInst::cset(&mut codegen.mctx, temp_reg, Condition::ne(), bzw_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cset);
                        },
                        ir::instruction::ICompCond::Sgt => {
                            let cset = MirInst::cset(&mut codegen.mctx, temp_reg, Condition::gt(), bzw_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cset);
                        },
                        ir::instruction::ICompCond::Sge => {
                            let cset = MirInst::cset(&mut codegen.mctx, temp_reg, Condition::ge(), bzw_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cset);
                        },
                        ir::instruction::ICompCond::Slt => {
                            let cset = MirInst::cset(&mut codegen.mctx, temp_reg, Condition::lt(), bzw_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cset);
                        },
                        ir::instruction::ICompCond::Sle => {
                            let cset = MirInst::cset(&mut codegen.mctx, temp_reg, Condition::le(), bzw_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cset);
                        },
                        _ => todo!(),
                    }
                    // 记录结果
                    codegen.curr_cond = Some((result_val, self.get_kind(codegen.ctx)));
                    let result_opd = MirOperand {
                        typ: result_val.kind(codegen.ctx),
                        kind: MirOperandKind::Reg(temp_reg),
                    };
                    codegen.table.insert(result_val, result_opd);
                }
            },
            ir::instruction::InstructionKind::FComp { cond } => {
                let result_val = self.get_result(codegen.ctx).unwrap();
                let lhs = self.get_operand(codegen.ctx, 0).unwrap();
                let rhs = self.get_operand(codegen.ctx, 1).unwrap();
                let bzw_reg = regs::bzw().into();
                let lhs_reg = if codegen.table.get(&lhs).is_some() {
                        let lhs_opd = codegen.table[&lhs];
                        match lhs_opd.kind {
                            MirOperandKind::Reg(reg) => reg,
                            _ => panic!("Expected register operand for left operand"),
                        }
                     } else {
                        match &lhs.deref(codegen.ctx).unwrap().kind {
                            ir::value::ValueKind::Constant { value } => {
                            let temp_reg0 = codegen.mctx.generate_vreg(regs::RegKind::Float).into();
                                if let ConstantValue::Float32 { value, .. } = value {
                                    if ALLOW_FLOATS.contains(value) {
                                        let fmov = MirInst::fmovimm(&mut codegen.mctx, temp_reg0, *value);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fmov);
                                    } else {
                                        let bits = value.to_bits();
                                        let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                        codegen.generate_movimm(new_reg, bits as i32);
                                        let fmov = MirInst::fmov(&mut codegen.mctx, temp_reg0, new_reg);
                                        let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fmov);
                                    }
                                } else {
                                    panic!("Expected int32 constant for left operand");
                                }
                                temp_reg0
                            }
                            ir::value::ValueKind::Parameter{ function:_, index, typ:_ } => {
                                if *index < 8 {
                                    panic!("Unreachable")
                                } else {
                                    // 大于八个的参数在栈上，通过fp获取
                                    let temp_reg0 = codegen.mctx.generate_vreg(regs::RegKind::Float).into();
                                    codegen.generate_ldr(temp_reg0, MemLoc::RegOffset { base: regs::fp().into(), offset: ((*index - 8) * 8) as i32 });

                                    let mem_loc = MirOperandKind::Reg(temp_reg0);
                                    codegen.table.insert(lhs, MirOperand {
                                        typ: lhs.kind(codegen.ctx),
                                        kind: mem_loc,
                                    });
                                    codegen.release.insert(lhs);
                                    temp_reg0
                                }
                            }
                            _ => todo!()
                        }
                    };
                // 查看右操作数在不在表中
                if let Some(rhs_opd) = codegen.table.get(&rhs) {
                    // 如果在表中，则直接使用寄存器
                    let rhs_reg = match rhs_opd.kind {
                        MirOperandKind::Reg(reg) => reg,
                        _ => panic!("Expected register operand for right operand"),
                    };
                    // 生成cmp指令
                    let fcmp = MirInst::fcmp(&mut codegen.mctx,lhs_reg, rhs_reg, bzw_reg); 
                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fcmp);
                } else {
                    // 处理常量右操作数
                    match &rhs.deref(codegen.ctx).unwrap().kind {
                        ir::value::ValueKind::Constant { value } => {
                            if let ConstantValue::Float32 { value, .. } = value {
                                if *value == 0.0 {
                                    let fcmp = MirInst::fcmpi(&mut codegen.mctx, lhs_reg, 0.0, bzw_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fcmp);
                                } else {
                                    let bits = value.to_bits();
                                    let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                    codegen.generate_movimm(new_reg, bits as i32);
                                    let temp_reg = codegen.mctx.generate_vreg(regs::RegKind::Float).into();
                                    let fmov = MirInst::fmov(&mut codegen.mctx, temp_reg, new_reg);
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fmov);
                                    let fcmp = MirInst::fcmp(&mut codegen.mctx,lhs_reg, temp_reg, bzw_reg); 
                                    let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fcmp);
                                }
                            }
                            else {
                                panic!("Expected int32 constant for left operand");
                            }
                        }
                        ir::value::ValueKind::Parameter{ function:_, index, typ:_ } => {
                            if *index < 8 {
                                panic!("Unreachable")
                            } else {
                                // 大于八个的参数在栈上，通过fp获取
                                let temp_reg = codegen.mctx.generate_vreg(regs::RegKind::Float).into();
                                codegen.generate_ldr(temp_reg, MemLoc::RegOffset { base: regs::fp().into(), offset: ((*index - 8) * 8) as i32 });

                                let mem_loc = MirOperandKind::Reg(temp_reg);
                                codegen.table.insert(rhs, MirOperand {
                                    typ: rhs.kind(codegen.ctx),
                                    kind: mem_loc,
                                });
                                codegen.release.insert(rhs);
                                let fcmp = MirInst::fcmp(&mut codegen.mctx, lhs_reg, temp_reg, bzw_reg);
                                let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fcmp);
                            }
                        }
                        _ => todo!()
                    }
                }
                
                //判断后继是否为有条件跳转指令
                let succ_is_cbr = if let Some(ir_inst) = self.succ(codegen.ctx) {
                    ir_inst.is_cbr(codegen.ctx)
                } else {
                    false
                };
                if !succ_is_cbr {
                    //根据cond类型为调整temp_reg
                    let result_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                    match cond {
                        ir::instruction::FCompCond::Ugt => {
                            let cset = MirInst::cset(&mut codegen.mctx, result_reg, Condition::gt(), bzw_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cset);
                        },
                        ir::instruction::FCompCond::Uge => {
                            let cset = MirInst::cset(&mut codegen.mctx, result_reg, Condition::ge(), bzw_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cset);
                        },
                        ir::instruction::FCompCond::Ult => {
                            let cset = MirInst::cset(&mut codegen.mctx, result_reg, Condition::lt(), bzw_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cset);
                        },
                        ir::instruction::FCompCond::Ule => {
                            let cset = MirInst::cset(&mut codegen.mctx, result_reg, Condition::le(), bzw_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cset);
                        }
                        ir::instruction::FCompCond::Oeq => {
                            let cset = MirInst::cset(&mut codegen.mctx, result_reg, Condition::eq(), bzw_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cset);
                        }
                        ir::instruction::FCompCond::One => {
                            let cset = MirInst::cset(&mut codegen.mctx, result_reg, Condition::ne(), bzw_reg);
                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, cset);
                        }
                        _ => todo!(),
                    }
                    // 记录结果
                    codegen.curr_cond = Some((result_val, self.get_kind(codegen.ctx)));
                    let result_opd = MirOperand {
                        typ: result_val.kind(codegen.ctx),
                        kind: MirOperandKind::Reg(result_reg),
                    };
                    codegen.table.insert(result_val, result_opd);
                }
            },
            ir::instruction::InstructionKind::Phi => todo!(),
            ir::instruction::InstructionKind::Call { tail_call } => {
                // 准备参数
                let mut args = Vec::new();
                for i in 1..self.get_operands(codegen.ctx).len() {
                    let arg_val = self.get_operand(codegen.ctx, i).unwrap();
                    args.push(arg_val);
                }
                // 将参数移动到正确寄存器
                let mut arg_regs = vec![];
                let mut is_void = true;
                let mut rds = vec![];
                // 移入结果寄存器
                if let Some(result_val) = self.get_result(codegen.ctx) {
                    let result_typ = result_val.kind(codegen.ctx);
                    let result_reg =  if result_val.kind(codegen.ctx).is_int(codegen.ctx) ||  result_val.kind(codegen.ctx).is_bool(codegen.ctx){
                        codegen.mctx.generate_vreg(regs::RegKind::Integer).into()
                    } else {
                        codegen.mctx.generate_vreg(regs::RegKind::Float).into()
                    };
                    let mopd = MirOperand {
                        typ: result_typ,
                        kind: MirOperandKind::Reg(result_reg),
                    };
                    codegen.table.insert(result_val, mopd);
                    arg_regs.push(result_reg);
                    is_void = false;
                }

                // 移入参数
                for (i, &arg_val) in args.iter().enumerate() {
                    // 记录定义寄存器
                    if i < 8 {
                        let preg = match i {
                            0 => if arg_val.kind(codegen.ctx).is_float(codegen.ctx){
                                    regs::s0().into()
                                } else if arg_val.is_ptr(codegen.ctx) {
                                    regs::x0().into()
                                } else {
                                    regs::w0().into()
                                },
                            1 => if arg_val.kind(codegen.ctx).is_float(codegen.ctx){
                                    regs::s1().into()
                                } else if arg_val.is_ptr(codegen.ctx) {
                                    regs::x1().into()
                                } else {
                                    regs::w1().into()
                                },
                            2 => if arg_val.kind(codegen.ctx).is_float(codegen.ctx){
                                    regs::s2().into()
                                } else if arg_val.is_ptr(codegen.ctx) {
                                    regs::x2().into()
                                } else {
                                    regs::w2().into()
                                },
                            3 => if arg_val.kind(codegen.ctx).is_float(codegen.ctx){
                                    regs::s3().into()
                                } else if arg_val.is_ptr(codegen.ctx) {
                                    regs::x3().into()
                                } else {
                                    regs::w3().into()
                                },
                            4 => if arg_val.kind(codegen.ctx).is_float(codegen.ctx){
                                    regs::s4().into()
                                } else if arg_val.is_ptr(codegen.ctx) {
                                    regs::x4().into()
                                } else {
                                    regs::w4().into()
                                },
                            5 => if arg_val.kind(codegen.ctx).is_float(codegen.ctx){
                                    regs::s5().into()
                                } else if arg_val.is_ptr(codegen.ctx) {
                                    regs::x5().into()
                                } else {
                                    regs::w5().into()
                                },
                            6 => if arg_val.kind(codegen.ctx).is_float(codegen.ctx){
                                    regs::s6().into()
                                } else if arg_val.is_ptr(codegen.ctx) {
                                    regs::x6().into()
                                } else {
                                    regs::w6().into()
                                },
                            7 => if arg_val.kind(codegen.ctx).is_float(codegen.ctx){
                                    regs::s7().into()
                                } else if arg_val.is_ptr(codegen.ctx) {
                                    regs::x7().into()
                                } else {
                                    regs::w7().into()
                                },
                            _ => unreachable!(),
                        };
                        rds.push(preg);
                    }
                    
                    // 前八个直接通过寄存器传递
                    if i < 8 {
                        let temp_reg;
                        if !arg_val.is_constant(codegen.ctx) {
                            // 参数是变量
                            if let Some(..) = codegen.table.get(&arg_val) {
                                let arg_opd = codegen.table[&arg_val];
                                match arg_opd.kind {
                                    MirOperandKind::Reg(reg) => {
                                        temp_reg = reg;
                                    },
                                    MirOperandKind::Mem(mem) => {
                                        match mem {
                                            MemLoc::Reg { address} => {
                                                temp_reg = address;
                                            }
                                            MemLoc::RegOffset { base, offset } => {
                                                temp_reg = codegen.mctx.generate_vreg(regs::RegKind::Address).into();
                                                if offset > 4095 || offset < -4096 {
                                                    let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                                    codegen.generate_movimm(new_reg, offset);
                                                    let add = MirInst::add(&mut codegen.mctx, temp_reg, base, new_reg);
                                                    codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add).unwrap();
                                                } else {
                                                    let addi = MirInst::addi(&mut codegen.mctx, temp_reg, base, offset);
                                                    codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, addi).unwrap();
                                                }
                                            }
                                            MemLoc::Reg2Offset { base, offset } => {
                                                temp_reg = codegen.mctx.generate_vreg(regs::RegKind::Address).into();
                                                let add = MirInst::add(&mut codegen.mctx, temp_reg, base, offset);
                                                codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add).unwrap();
                                            }
                                        };
                                    }
                                };
                            } else {
                                match &arg_val.deref(codegen.ctx).unwrap().kind {
                                    ir::value::ValueKind::Parameter{ function:_, index, typ:_ } => {
                                        if *index < 8 {
                                            panic!("Unreachable")
                                        } else {
                                            temp_reg = if arg_val.is_ptr(codegen.ctx) {
                                                codegen.mctx.generate_vreg(regs::RegKind::Address).into()
                                            } else if arg_val.kind(codegen.ctx).is_float(codegen.ctx) {
                                                codegen.mctx.generate_vreg(regs::RegKind::Float).into()
                                            } else {
                                                codegen.mctx.generate_vreg(regs::RegKind::Integer).into()
                                            };
                                            let new_reg = if arg_val.is_ptr(codegen.ctx) {
                                                codegen.mctx.generate_vreg(regs::RegKind::Address).into()
                                            } else if arg_val.kind(codegen.ctx).is_float(codegen.ctx) {
                                                codegen.mctx.generate_vreg(regs::RegKind::Float).into()
                                            } else {
                                                codegen.mctx.generate_vreg(regs::RegKind::Integer).into()
                                            };
                                            codegen.generate_ldr(new_reg, MemLoc::RegOffset { base: regs::fp().into(), offset: ((*index - 8) * 8) as i32 });

                                            let mov = if arg_val.kind(codegen.ctx).is_float(codegen.ctx) {
                                                MirInst::fmov(&mut codegen.mctx, temp_reg, new_reg)
                                            } else {
                                                MirInst::mov_reg(&mut codegen.mctx, temp_reg, new_reg)
                                            };
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, mov).unwrap();
                                            let mem_loc = if arg_val.is_ptr(codegen.ctx) { 
                                                MirOperandKind::Mem(MemLoc::Reg{address: new_reg})
                                            } else {
                                                MirOperandKind::Reg(new_reg)
                                            };
                                            codegen.table.insert(arg_val, MirOperand {
                                                typ: arg_val.kind(codegen.ctx),
                                                kind: mem_loc,
                                            });
                                            codegen.release.insert(arg_val);
                                        }
                                    }
                                    _ => todo!(),
                                }
                            }
                        } else {
                            if let ir::value::ValueKind::Constant { value } = &arg_val.deref(codegen.ctx).unwrap().kind {
                                match value {
                                    ConstantValue::Zero { .. } | ConstantValue::Undef { .. } => {
                                        temp_reg = regs::wzr().into();
                                    }
                                    ConstantValue::Int32 { value, .. } => {
                                        if *value == 0 {
                                            temp_reg = regs::wzr().into();
                                        } else {
                                            temp_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                            codegen.generate_movimm(temp_reg, *value);
                                        }
                                    }
                                    ConstantValue::Float32 { value, .. } => {
                                        temp_reg = codegen.mctx.generate_vreg(regs::RegKind::Float).into();
                                        if *value == 0.0 {
                                            let fmov = MirInst::fmov(&mut codegen.mctx, temp_reg, regs::wzr().into());
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fmov).unwrap();
                                        }else {
                                            let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                            let bits = value.to_bits() as i32;
                                            codegen.generate_movimm(new_reg, bits);
                                            let fmov = MirInst::fmov(&mut codegen.mctx, temp_reg, new_reg);
                                            codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fmov).unwrap();
                                        }
                                    }
                                    _ => panic!("Unexpected constant type for call argument"),
                                }
                            } else {
                                panic!("Expected a constant for call argument");
                            }
                        }
                        arg_regs.push(temp_reg);
                    } else {
                        let vreg;
                        if !arg_val.is_constant(codegen.ctx) {
                            if let Some(..) = codegen.table.get(&arg_val) {
                                let arg_opd = codegen.table[&arg_val];
                                match arg_opd.kind {
                                    MirOperandKind::Reg(reg) => {
                                        vreg = reg;
                                    },
                                    MirOperandKind::Mem(mem) => {
                                        match mem {
                                            MemLoc::Reg { address } => {
                                                vreg = address;
                                            }
                                            MemLoc::RegOffset { base, offset } => {
                                                vreg = codegen.mctx.generate_vreg(regs::RegKind::Address).into();
                                                if offset > 4095 || offset < -4096 {
                                                    let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                                    codegen.generate_movimm(new_reg, offset);
                                                    let add = MirInst::add(&mut codegen.mctx, vreg, base, new_reg);
                                                    codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add).unwrap();
                                                } else {
                                                    let addi = MirInst::addi(&mut codegen.mctx, vreg, base, offset);
                                                    codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, addi).unwrap();
                                                }
                                            }   
                                            MemLoc::Reg2Offset { base, offset } => {
                                                vreg = codegen.mctx.generate_vreg(regs::RegKind::Address).into();
                                                let add = MirInst::add(&mut codegen.mctx, vreg, base, offset);
                                                codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, add).unwrap();
                                            }
                                        };
                                    }
                                };
                            } else {
                                match &arg_val.deref(codegen.ctx).unwrap().kind {
                                    ir::value::ValueKind::Parameter{ function:_, index, typ:_ } => {
                                        if *index < 8 {
                                            panic!("Unreachable")
                                        } else {
                                            let temp_reg = if arg_val.is_ptr(codegen.ctx) {
                                                codegen.mctx.generate_vreg(regs::RegKind::Address).into()
                                            } else if arg_val.kind(codegen.ctx).is_float(codegen.ctx) {
                                                codegen.mctx.generate_vreg(regs::RegKind::Float).into()
                                            } else {
                                                codegen.mctx.generate_vreg(regs::RegKind::Integer).into()
                                            };
                                            vreg = if arg_val.is_ptr(codegen.ctx) {
                                                codegen.mctx.generate_vreg(regs::RegKind::Address).into()
                                            } else if arg_val.kind(codegen.ctx).is_float(codegen.ctx) {
                                                codegen.mctx.generate_vreg(regs::RegKind::Float).into()
                                            } else {
                                                codegen.mctx.generate_vreg(regs::RegKind::Integer).into()
                                            };
                                            codegen.generate_ldr(temp_reg, MemLoc::RegOffset { base: regs::fp().into(), offset: ((*index - 8) * 8) as i32 });

                                            let mov = if arg_val.kind(codegen.ctx).is_float(codegen.ctx) {
                                                MirInst::fmov(&mut codegen.mctx,vreg, temp_reg)
                                            } else {
                                                MirInst::mov_reg(&mut codegen.mctx, vreg, temp_reg)
                                            };
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, mov).unwrap();
                                            let mem_loc = if arg_val.is_ptr(codegen.ctx) { 
                                                MirOperandKind::Mem(MemLoc::Reg{address: temp_reg})
                                            } else {
                                                MirOperandKind::Reg(temp_reg)
                                            };
                                            codegen.table.insert(arg_val, MirOperand {
                                                typ: arg_val.kind(codegen.ctx),
                                                kind: mem_loc,
                                            });
                                            codegen.release.insert(arg_val);
                                        }
                                    }
                                    _ => todo!()
                                }
                            }
                        } else {
                            if let ir::value::ValueKind::Constant { value } = &arg_val.deref(codegen.ctx).unwrap().kind {
                                match value {
                                    ConstantValue::Zero { .. } | ConstantValue::Undef { .. } => {
                                        vreg = regs::wzr().into();
                                    }
                                    ConstantValue::Int32 { value, .. } => {
                                        if *value == 0 {
                                            vreg = regs::wzr().into();
                                        } else {
                                            vreg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                            codegen.generate_movimm(vreg, *value);
                                        }
                                    }
                                    ConstantValue::Float32 { value, .. } => {
                                        vreg = codegen.mctx.generate_vreg(regs::RegKind::Float).into();
                                        if *value == 0.0 {
                                            let fmov = MirInst::fmov(&mut codegen.mctx, vreg, regs::wzr().into());
                                            let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fmov);
                                        } else {
                                            let new_reg = codegen.mctx.generate_vreg(regs::RegKind::Integer).into();
                                            let bits = value.to_bits() as i32;
                                            codegen.generate_movimm(new_reg, bits);
                                            let fmov = MirInst::fmov(&mut codegen.mctx, vreg, new_reg);
                                            codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, fmov).unwrap();
                                        }
                                    }
                                    _ => panic!("Unexpected constant type for call argument"),
                                }
                            } else {
                                panic!("Unexpected operand kind for call argument");
                            }
                        }
                        codegen.generate_str(vreg, MemLoc::RegOffset { base: regs::sp().into(), offset: ((i-8)*8) as i32 });
                        // let str = MirInst::str(&mut codegen.mctx, vreg, MemLoc::RegOffset { base: regs::sp().into(), offset: ((i-8)*8) as i32 });
                        // let _ = codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, str);
                    }
                }
                // 生成调用指令
                let func_val = self.get_operand(codegen.ctx, 0).unwrap();
                let func_name = if let ir::value::ValueKind::Function {function, .. } = &func_val.deref(codegen.ctx).unwrap().kind {
                    function
                } else {
                    panic!("Call instruction must have a function as first operand");
                };
                let call = MirInst::call(&mut codegen.mctx, MirLabel::new(func_name.clone()), tail_call, is_void, rds, arg_regs);
                codegen.curr_block.as_ref().unwrap().push_back(&mut codegen.mctx, call).unwrap();
            },
        }
    }
}

/*
* 栈空间布局
*
* ---------------------------------------------------      <----- 当前fp（上一次的sp）
*  被调用者保存 (callee_saved_registers）
* ---------------------------------------------------   
*  调用者保护 (caller_saved_registers)
* ---------------------------------------------------  
*  寄存器分配溢出 (spill)
* ---------------------------------------------------  
*  局部变量 (Alloca)
* ---------------------------------------------------   
*  被调用者函数参数
* ---------------------------------------------------      <----- 当前sp
*/

impl<'s> CodegenContext<'s> {
    pub fn new(ctx: &'s ir::context::Context) -> Self {
        Self {
            mctx: MirContext::default(),
            ctx,
            table: HashMap::default(),
            funcs: HashMap::default(),
            blocks: HashMap::default(),
            globals: HashMap::default(),
            curr_func: None,
            curr_block: None,
            func_map: HashMap::default(),
            release: HashSet::default(),
            curr_cond: None,
            movedimm: HashMap::default(),
        }
    }
    
    pub fn mircontext(self) -> MirContext { self.mctx }
    
    pub fn codegen(&mut self) {
        // 处理全局变量
        for global_data in self.ctx.globals.iter() {
            global_data.codegen(self);
        }
        // 为所有函数和基本块创建占位符
        // for i in SY_RUNTIME_FUNCS {
        //     let mfunc = MirFunction::new(&mut self.mctx, i, true);
        //     self.funcs.insert(i.to_string(), mfunc);
        // }
        for func in self.ctx.get_functions() {
            //println!("Translating function: {}", func.get_id(self.ctx));
            func.codegen(self);
        }
        // 翻译指令
        for func in self.ctx.get_functions() {
            self.curr_func = Some(self.funcs[&func.get_id(self.ctx)]);
            let mut stack_size = 0;
            //println!("{}",func.display(&self.ctx));
            // 计算函数局部变量栈空间（Alloca指令）
            for block in func.iter(self.ctx) {
                self.curr_block = Some(self.blocks[&block]);
                for inst in block.iter(self.ctx) {
                    if let ir::instruction::InstructionKind::MemAccess { op: MemAccessOp::Alloca { typ } } = inst.get_kind(self.ctx) {
                        stack_size += typ.bitwidth(self.ctx);
                    } 
                }
            }
            stack_size = (stack_size + 7) / 8;
            // 调用者保护临时寄存器空间x9-x15
            self.curr_func.unwrap().add_storage_stack_size(&mut self.mctx, stack_size as i32);
            // 计算函数内部给被调用函数分配的参数空间
            let mut callee_stack = 0;
            for block in func.iter(self.ctx) {
                self.curr_block = Some(self.blocks[&block]);
                for inst in block.iter(self.ctx) {
                    if let ir::instruction::InstructionKind::Call { .. } = inst.get_kind(self.ctx) {
                        if inst.get_operands(self.ctx).len() > 8 && inst.get_operands(self.ctx).len() -8 > callee_stack {
                            callee_stack = inst.get_operands(self.ctx).len()-8;
                        }
                    }
                }
            }
            callee_stack = callee_stack*8;
            self.curr_func.unwrap().add_calleeargs_stack_size(&mut self.mctx, callee_stack as i32);

            // 翻译基本块
            let cfg = match pass::analysis::cfa::CFAnalysis::execute_function(&func, self.ctx) {
                Ok((_, result)) => result.cfg,
                Err(e) => panic!("Failed to execute function analysis: {:?}", e),
            };
            
            //let blocks = bfs_order(&cfg);

            let mut visited = HashSet::default();
            let mut blocks = vec![];
            postorder(self.ctx, &cfg, &cfg.entry_block.unwrap(), &mut visited, &mut blocks);
            blocks.reverse();

            for block in blocks {
                self.curr_cond = None;
                self.curr_block = Some(self.blocks[&block]);
                for inst in block.iter(self.ctx) {
                    inst.codegen(self);
                }

                let mut temp_table = HashMap::default();
                for (val, opd) in self.table.iter() {
                    if self.release.contains(val){
                        continue;
                    } else {
                        temp_table.insert(*val, *opd);
                    }
                }
                self.release = HashSet::default();
                self.movedimm = HashMap::default();
                self.table = temp_table;   
            }
            
        }
    }
    
    pub fn gen_ret_move(&mut self, val: Value) {
        let curr_block = self.curr_block.unwrap();
        let ret_reg = self.generate_ret_reg(val.kind(self.ctx)); // 返回寄存器
        
        match &val.deref(self.ctx).unwrap().kind {
            ir::value::ValueKind::Constant { value } => {
                match value {
                    ConstantValue::Int32 { value, .. } => {
                        // 直接移动到返回寄存器
                        self.generate_movimm(regs::Reg::P(ret_reg), *value);
                    }
                    ConstantValue::Bool { value, .. } => {
                        // 直接移动到返回寄存器
                        self.generate_movimm(regs::Reg::P(ret_reg), *value as i32);
                    }
                    ConstantValue::Float32 { value, .. } => {
                        // 直接移动到返回寄存器
                        let bits = value.to_bits();
                        let new_reg = self.mctx.generate_vreg(regs::RegKind::Integer).into();
                        self.generate_movimm(new_reg, bits as i32);
                        let fmov = MirInst::mov_reg(&mut self.mctx, regs::Reg::P(ret_reg), new_reg);
                        curr_block.push_back(&mut self.mctx, fmov).unwrap();
                    }
                    _ => todo!(),
                }
            }
            ir::value::ValueKind::InstResult { .. } => {
                let mopd = self.table[&val];
                match mopd.kind {
                    MirOperandKind::Reg(reg) => {
                        if reg != regs::Reg::P(ret_reg) {
                            // 移动值到返回寄存器
                            match reg.kind() {
                                regs::RegKind::Float => {
                                    let fmov = MirInst::fmov(&mut self.mctx, regs::Reg::P(ret_reg), reg);
                                    curr_block.push_back(&mut self.mctx, fmov).unwrap();
                                } 
                                _ => {
                                    let mov = MirInst::mov_reg(&mut self.mctx, regs::Reg::P(ret_reg), reg);
                                    curr_block.push_back(&mut self.mctx, mov).unwrap();
                                }
                            }   
                        }
                    }
                    MirOperandKind::Mem(mem_loc) => {
                        self.generate_ldr(regs::Reg::P(ret_reg), mem_loc);

                    }
                }
            }
            ir::value::ValueKind::Parameter { function:_, index, typ }=>{
                if *index < 8 {
                    let arg_reg = match *index {
                        0 => if typ.is_float(self.ctx) {
                            regs::s0().into()
                        } else {
                            regs::w0().into()
                        }
                        1 => if typ.is_float(self.ctx) {
                            regs::s1().into()
                        } else {
                            regs::w1().into()
                        }
                        2 => if typ.is_float(self.ctx) {
                            regs::s2().into()
                        } else {
                            regs::w2().into()
                        }
                        3 => if typ.is_float(self.ctx) {
                            regs::s3().into()
                        } else {
                            regs::w3().into()
                        }
                        4 => if typ.is_float(self.ctx) {
                            regs::s4().into()
                        } else {
                            regs::w4().into()
                        }
                        5 => if typ.is_float(self.ctx) {
                            regs::s5().into()
                        } else {
                            regs::w5().into()
                        }
                        6 => if typ.is_float(self.ctx) {
                            regs::s6().into()
                        } else {
                            regs::w6().into()
                        }
                        7 => if typ.is_float(self.ctx) {
                            regs::s7().into()
                        } else {
                            regs::w7().into()
                        }
                        _ => unreachable!(),
                    };
                    if typ.is_int(self.ctx) || typ.is_bool(self.ctx){
                        let mov = MirInst::mov_reg(&mut self.mctx, regs::Reg::P(ret_reg), arg_reg);
                        curr_block.push_back(&mut self.mctx, mov).unwrap();
                    } else {
                        let fmov = MirInst::fmov(&mut self.mctx, regs::Reg::P(ret_reg), arg_reg);
                        curr_block.push_back(&mut self.mctx, fmov).unwrap();
                    }
                } else {
                    // 大于八个的参数在栈上，通过fp获取
                    // let argadress_reg = if typ.is_int(self.ctx)  || typ.is_bool(self.ctx) {
                    //     self.mctx.generate_vreg(regs::RegKind::Integer).into()
                    // } else {
                    //     self.mctx.generate_vreg(regs::RegKind::Float).into()
                    // };
                    let mem_loc = if *index*8 < 4096 {
                        MemLoc::RegOffset { base: regs::fp().into(), offset: (*index*8) as i32 } 
                    } else {
                        let new_reg = self.mctx.generate_vreg(regs::RegKind::Integer).into();
                        self.generate_movimm(new_reg, (*index*8) as i32);
                        MemLoc::Reg2Offset { base: regs::fp().into(), offset: new_reg }
                    };  
                    self.generate_ldr(regs::Reg::P(ret_reg), mem_loc);       
                }
            }
            _ => todo!(),
        }
    }
    
    pub fn generate_ret_reg(&mut self, ty: ir::typ::Typ) -> PReg {
        if ty.is_ptr(self.ctx)  {
            regs::x0().into()
        } else if ty.is_float(self.ctx) {
            regs::s0().into()
        }else {
            regs::w0().into()
        }
    }
    
    pub fn generate_param_reg(&mut self, arg: ir::value::Value, index: u32) -> Reg {
        match index {
            0 => {
                if arg.is_ptr(self.ctx){
                    regs::x0().into()
                } else if arg.kind(self.ctx).is_float(self.ctx) {
                    regs::s0().into()
                } else {
                    regs::w0().into()
                }
            },
            1 => {
                if arg.is_ptr(self.ctx){
                    regs::x1().into()
                } else if arg.kind(self.ctx).is_float(self.ctx){
                    regs::s1().into()
                } else {
                    regs::w1().into()
                }
            },
            2 => {
                if arg.is_ptr(self.ctx){
                    regs::x2().into()
                } else if arg.kind(self.ctx).is_float(self.ctx){
                    regs::s2().into()
                } else {
                    regs::w2().into()
                }
            },
            3 => {
                if arg.is_ptr(self.ctx){
                    regs::x3().into()
                } else if arg.kind(self.ctx).is_float(self.ctx) {
                    regs::s3().into()
                } else {
                    regs::w3().into()
                }
            },
            4 => {
                if arg.is_ptr(self.ctx){
                    regs::x4().into()
                } else if arg.kind(self.ctx).is_float(self.ctx){
                    regs::s4().into()
                } else {
                    regs::w4().into()
                }
            },
            5 => {
                if arg.is_ptr(self.ctx){
                    regs::x5().into()
                } else if arg.kind(self.ctx).is_float(self.ctx){
                    regs::s5().into()
                } else {
                    regs::w5().into()
                }
            },
            6 => {
                if arg.is_ptr(self.ctx){
                    regs::x6().into()
                } else if arg.kind(self.ctx).is_float(self.ctx){
                    regs::s6().into()
                } else {
                    regs::w6().into()
                }
            },
            7 => {
                if arg.is_ptr(self.ctx){
                    regs::x7().into()
                } else if arg.kind(self.ctx).is_float(self.ctx){
                    regs::s7().into()
                } else {
                    regs::w7().into()
                }
            }
            _ => unreachable!()
        }
    }
    
    pub fn generate_opd_reg(&mut self, opd: ir::value::Value) -> Reg {
        if self.table.get(&opd).is_some() {
            let v_opd = self.table[&opd];
            match v_opd.kind {
                MirOperandKind::Reg(reg) => reg,
                _ => panic!("Expected register operand for binary op"),
            }
        } else {
            match &opd.deref(self.ctx).unwrap().kind {
                ir::value::ValueKind::Parameter{ function:_, index, typ:_ } => {
                    if *index < 8 {
                        panic!("Unreachable")
                    } else {
                        // 大于八个的参数在栈上，通过fp获取
                        let new_reg = if opd.kind(self.ctx).is_int(self.ctx) || opd.kind(self.ctx).is_bool(self.ctx){
                            self.mctx.generate_vreg(regs::RegKind::Integer).into()
                        } else {
                            self.mctx.generate_vreg(regs::RegKind::Float).into()
                        };
                        self.generate_ldr(new_reg, MemLoc::RegOffset { base: regs::fp().into(), offset: ((*index - 8) * 8) as i32 });
                        let mem_loc = if opd.is_ptr(self.ctx) { 
                            MirOperandKind::Mem(MemLoc::Reg{address: new_reg})
                        } else {
                            MirOperandKind::Reg(new_reg)
                        };
                        self.table.insert(opd, MirOperand {
                            typ: opd.kind(self.ctx),
                            kind: mem_loc,
                        });
                        new_reg
                    }
                }
                _ => panic!("Expected register operand for binary op")
            }
        }
    }
    
    pub fn generate_movimm(&mut self, rn: Reg, imm: i32) {
        let low = imm & 0xFFFF;
        let high = (imm >> 16) & 0xFFFF;
        let movz = MirInst::movz(&mut self.mctx, rn, low);
        let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, movz);
        if high != 0 {
            let movk = MirInst::movk(&mut self.mctx, rn, high);
            let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, movk);
        }
    }

    pub fn generate_muli(&mut self, rd: Reg, rn: Reg, imm: i32) {
        if imm == 1 {
            let mov = MirInst::mov_reg(&mut self.mctx, rd, rn);
            let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, mov);
        } else if imm == -1 {
            let neg = MirInst::neg(&mut self.mctx, rd, rn);
            let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, neg);
        } else if imm == 0 {
            let mov = MirInst::mov_reg(&mut self.mctx, rd, regs::wzr().into());
            let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, mov);
        } else {
            let is_negative = imm < 0;
            let abs_val = if is_negative { -imm } else { imm };
            if (abs_val & (abs_val-1)) == 0 {
                let lsl = MirInst::lsl(&mut self.mctx, rd, rn, abs_val.trailing_zeros());
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, lsl);
                if is_negative {
                    let neg = MirInst::neg(&mut self.mctx, rd, rd);
                    let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, neg);
                }
            } else if abs_val > 1 && (abs_val&(abs_val+1)) == 0  {
                let shift = (abs_val+1).trailing_zeros();
                let temp_reg = self.mctx.generate_vreg(regs::RegKind::Integer).into();
                let lsl = MirInst::lsl(&mut self.mctx, temp_reg, rn, shift);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, lsl);
                if is_negative {
                    let sub = MirInst::sub(&mut self.mctx, rd, rn, temp_reg);
                    let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, sub);
                } else {
                    let sub = MirInst::sub(&mut self.mctx, rd, temp_reg, rn);
                    let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, sub);
                }
            } else if abs_val > 1 && ((abs_val-2)&(abs_val-1)) == 0  {
                let shift = (abs_val-1).trailing_zeros();
                let temp_reg = self.mctx.generate_vreg(regs::RegKind::Integer).into();
                let lsl = MirInst::lsl(&mut self.mctx, temp_reg, rn, shift);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, lsl);
                let add = MirInst::add(&mut self.mctx, rd, temp_reg, rn);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, add);
                if is_negative {
                    let neg = MirInst::neg(&mut self.mctx, rd, rd);
                    let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, neg);
                }
            } else if let Some((a,b)) = find_additive_decomposition(abs_val) {
                let temp_reg = self.mctx.generate_vreg(regs::RegKind::Integer).into();
                let lsl1 = MirInst::lsl(&mut self.mctx, temp_reg, rn, a);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, lsl1);

                let temp_reg1 = self.mctx.generate_vreg(regs::RegKind::Integer).into();
                let lsl2 = MirInst::lsl(&mut self.mctx, temp_reg1, rn, b);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, lsl2);
                let add = MirInst::add(&mut self.mctx, rd, temp_reg, temp_reg1);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, add);
                if is_negative {
                    let neg = MirInst::neg(&mut self.mctx, rd, rd);
                    let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, neg);
                }
            } else {
                self.generate_movimm(rd, imm);
                let mul = MirInst::mul(&mut self.mctx, rd, rd, rn);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, mul);
            }
        }
    }

    pub fn generate_sdivi(&mut self, rd: Reg, rn: Reg, imm: i32) {
        if imm == 1 {
            let mov = MirInst::mov_reg(&mut self.mctx, rd, rn);
            let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, mov);
        } else if imm == -1 {
            let neg = MirInst::neg(&mut self.mctx, rd, rn);
            let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, neg);
        }else {
            let abs_val = imm.abs() as u32;
            // 处理2的幂次方除数优化
            if abs_val&(abs_val-1) == 0 {
                //println!("sadsadas");
                let shift = abs_val.trailing_zeros();
                let reg1 = self.mctx.generate_vreg(regs::RegKind::Integer).into();
                let reg2 = self.mctx.generate_vreg(regs::RegKind::Integer).into();

                // 1. 算术右移31位：reg1 = lhs_reg >> 31
                let asr_inst = MirInst::asr(&mut self.mctx, reg1, rn, 31);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, asr_inst);

                // 2. 逻辑右移 (32 - shift) 位：reg1 = reg1 >>> (32 - shift)
                let shift1 = 32 - shift;
                let lsr_inst = MirInst::lsr(&mut self.mctx, reg1, reg1, shift1);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, lsr_inst);

                // 3. 加法：reg3 = lhs_reg + reg1
                let add_inst = MirInst::add(&mut self.mctx, reg2, rn, reg1);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, add_inst);

                // 4. 算术右移shift位：temp_reg = reg2 >> shift
                let asr_inst2 = MirInst::asr(&mut self.mctx, rd, reg2, shift);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, asr_inst2);

                // 5. 处理负除数：对结果取负
                if imm < 0 {
                    let neg_inst = MirInst::neg(&mut self.mctx, rd, rd);
                    let _ = self.curr_block.as_mut().unwrap().push_back(&mut self.mctx, neg_inst);
                }
                // let new_reg = self.mctx.generate_vreg(regs::RegKind::Integer).into();
                // self.generate_movimm(new_reg, imm);
                // let div = MirInst::div(&mut self.mctx, rd, rn, new_reg);
                // let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, div);
            } else {
                // 魔法数优化
                let flag = imm < 0;
                let div_imm = imm.abs();
                let nc: i64 = (1i64 << 31) - ((1i64 << 31) % div_imm as i64) - 1;
                let mut p = 32;
                while (1i64 << p) <= nc * (div_imm as i64 - (1i64 << p) % div_imm as i64) {
                    p += 1;
                }
                let m: i64 = ((1i64<< p) + div_imm as i64 - (1i64 << p) % div_imm as i64) / div_imm as i64;
                let n = (m & 0xFFFF_FFFF) as i32;
                let shift = (p - 32) as u32;
                // println!("{},{}, {}",imm,n, p);

                let reg1 = self.mctx.generate_vreg(regs::RegKind::Integer).into();
                let reg2 = self.mctx.generate_vreg(regs::RegKind::Address).into();
                let reg_hi = self.mctx.generate_vreg(regs::RegKind::Integer).into();

                self.generate_movimm(reg1, n as i32);
                let smull = MirInst::smull(&mut self.mctx, reg2, rn, reg1);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, smull);
                let lsr0 = MirInst::lsr(&mut self.mctx, reg2, reg2, 32);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, lsr0);
                let mov = MirInst::mov_reg(&mut self.mctx, reg_hi, reg2);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, mov);
                
                if m >= 0x80000000 {                
                    let add = MirInst::add(&mut self.mctx, reg_hi, reg_hi, rn);
                    let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, add);
                }
                
                // 对高32位算数右移
                let reg3 = self.mctx.generate_vreg(regs::RegKind::Integer).into();
                let asr = MirInst::asr(&mut self.mctx, reg3, reg_hi, shift);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, asr);

                // 提取符号位
                let sign_reg = self.mctx.generate_vreg(regs::RegKind::Integer).into();
                let lsr = MirInst::lsr(&mut self.mctx, sign_reg, rn, 31);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, lsr);
                // 添加符号位
                let add = MirInst::add(&mut self.mctx, rd, sign_reg, reg3);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, add);
                if flag {
                    let neg = MirInst::neg(&mut self.mctx, rd, rd);
                    let _ = self.curr_block.as_mut().unwrap().push_back(&mut self.mctx, neg);
                }
                //   {
                //     let new_reg = self.mctx.generate_vreg(regs::RegKind::Integer).into();
                //     self.generate_movimm(new_reg, imm);
                //     let div = MirInst::div(&mut self.mctx, rd, rn, new_reg);
                //     let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, div);
                // }
            } 
        }
    }

    pub fn generate_str(&mut self, rm: Reg, mem_loc: MemLoc) {
        match mem_loc {
        MemLoc::RegOffset { base, offset } => {
            if offset > 255 || offset < -256{
                let temp_reg = self.mctx.generate_vreg(regs::RegKind::Integer).into();
                self.generate_movimm(temp_reg, offset);
                let str = MirInst::str(&mut self.mctx, rm, MemLoc::Reg2Offset { base: base, offset: temp_reg });
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, str);
            } else {
                let str = MirInst::str(&mut self.mctx, rm, mem_loc);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, str);
            }
        },
        _ => {
            let str = MirInst::str(&mut self.mctx, rm, mem_loc);
            let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, str);
        }
    }
    }

    pub fn generate_ldr(&mut self, rd: Reg, mem_loc: MemLoc) {
        match mem_loc {
            MemLoc::RegOffset { base, offset } => {
                if offset > 255 || offset < -256{
                    let temp_reg = self.mctx.generate_vreg(regs::RegKind::Integer).into();
                    self.generate_movimm(temp_reg, offset);
                    let ldr = MirInst::ldr(&mut self.mctx, rd, MemLoc::Reg2Offset { base: base, offset: temp_reg});
                    let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, ldr);
                } else {
                    let ldr = MirInst::ldr(&mut self.mctx, rd, mem_loc);
                    let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, ldr);
                }
            }
            _ => {
                let ldr = MirInst::ldr(&mut self.mctx, rd, mem_loc);
                let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, ldr);
            }
        }
    }

    pub fn generate_adrp(&mut self, rd: Reg, label: MirLabel ) {
        let adrp = MirInst::adrp(&mut self.mctx, rd, label.clone());
        let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, adrp);
        let add_label = MirInst::add_label(&mut self.mctx, rd, rd, label.clone());
        let _ = self.curr_block.as_ref().unwrap().push_back(&mut self.mctx, add_label);
    }
}

pub fn bfs_order(cfg: &ControlFlowGraph) -> Vec<BasicBlock> {
    let mut queue: VecDeque<BasicBlock> = VecDeque::new();
    let mut result: Vec<BasicBlock> = Vec::new();

    queue.push_back(cfg.entry_block.unwrap());

    while let Some(current_block) = queue.pop_front() {
        result.push(current_block);
        if let Some(successors) = cfg.succ_set.get(&current_block) {
            for succ in successors {
                if !queue.contains(succ) && !result.contains(succ) {
                    queue.push_back(*succ);
                }
            }
        }
    }
    result  
}

fn postorder(
    ctx: &Context,
    cfg: &ControlFlowGraph,
    bbk: &BasicBlock,
    visited: &mut HashSet<BasicBlock>,
    postorder_list: &mut Vec<BasicBlock>,
) {
    visited.insert(*bbk);
    for bbk in cfg.succ_set[bbk].iter() {
        if !visited.contains(bbk) {
            postorder(ctx, cfg, bbk, visited, postorder_list);
        }
    }
    postorder_list.push(*bbk);
}

pub fn find_additive_decomposition(n: i32) -> Option<(u32, u32)> {
    if n.count_ones() == 2 {
        let a = 31 - n.leading_zeros();
        let b = n.trailing_zeros();
        //println!("n: {} -> {}, {}", n, a, b);
        Some((a, b))
    } else {
        None
    }
}

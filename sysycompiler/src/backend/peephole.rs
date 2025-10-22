use rustc_hash::FxHashMap as HashMap;

use crate::{
    backend::mir::{
        mir_block::MirBlock,
        mir_context::MirContext,
        mir_inst::{MirInst, MirInstKind},
        regs::{self, Reg},
    },
    utils::linked_list::{LinkedListContainer, LinkedListNode},
};

#[derive(Debug)]
pub struct PeepholeOptimizer {
    pub total_deleted: usize,
    pub total_replaced: usize,
}
impl PeepholeOptimizer {
    pub fn new() -> Self {
        Self {
            total_deleted: 0,
            total_replaced: 0,
        }
    }
    pub fn peephole_optimize(&mut self, ctx: &mut MirContext, time_of_call: usize) {
        let mut blocks_to_optimize = Vec::new();
        self.total_deleted = 0;
        self.total_replaced = 0;
        for func_data in ctx.funcs.iter() {
            let func = func_data.self_ptr();
            if !func.is_external(ctx) {
                let blocks: Vec<_> = func.iter(ctx).collect();
                blocks_to_optimize.extend(blocks);
            }
        }
        for block in blocks_to_optimize {
            self.optimize_block(ctx, block, time_of_call);
        }
        // println!(
        //     "Peephole Optimization[{}]: {} deleted, {} replaced",
        //     time_of_call, self.total_deleted, self.total_replaced
        // );
    }

    fn optimize_block(&mut self, ctx: &mut MirContext, mblock: MirBlock, time_of_call: usize) {
        let mut curr = mblock.head(ctx);
        while let Some(inst) = curr {
            let next = inst.succ(ctx);
            let opt_inst = self.match_patterns(ctx, inst, time_of_call);
            if opt_inst.0 == 0 {
                //println!("delete inst: {:?}", inst.display(ctx));
                self.total_deleted += 1;
                inst.unlink(ctx);
            } else if opt_inst.0 == 1 {
                //println!("pre insr: {:?}", inst.pre(ctx).unwrap().display(ctx));
                //println!("replace inst: {:?} -> {:?}", inst.display(ctx), opt_inst.1.unwrap().display(ctx));
                self.total_replaced += 1;
                inst.replace_with(ctx, opt_inst.1.unwrap()[0]);
            } else if opt_inst.0 == 3 {
                let insts = opt_inst.1.unwrap();
                let _ = inst.insert_before(ctx, insts[0]);
                let _ = inst.insert_before(ctx, insts[1]);
                let _ = inst.insert_before(ctx, insts[2]);
                self.total_replaced += 1;
                inst.replace_with(ctx, insts[3]);
                println!("insert inst: {:?}", crate::backend::asm2string::Display::display(insts[0], ctx));
                println!("insert inst: {:?}", crate::backend::asm2string::Display::display(insts[1], ctx));
                println!("insert inst: {:?}", crate::backend::asm2string::Display::display(insts[2], ctx));
                println!("replace inst: {:?} -> {:?}", crate::backend::asm2string::Display::display(inst, ctx), crate::backend::asm2string::Display::display(insts[3], ctx));
            }
            curr = next;
        }
    }

    // 0-> delete, 1-> replace, 2-> do nothing
    fn match_patterns(
        &mut self,
        ctx: &mut MirContext,
        inst: MirInst,
        time_of_call: usize,
    ) -> (usize, Option<Vec<MirInst>>) {
        let kind = inst.kind(ctx);

        // 模式1: 删除自身到自身的移动
        // mov regA, regA -> 删除
        if let MirInstKind::MovReg { rd, rn } = kind {
            if *rd == *rn {
                return (0, None);
            }
        }
        if let MirInstKind::Addi { rd, rn, imm } = kind {
            if *rd == *rn && *imm == 0 {
                return (0, None);
            }
        }
        if let MirInstKind::Subi { rd, rn, imm } = kind {
            if *rd == *rn && *imm == 0 {
                return (0, None);
            }
        }

        // 模式2：加减法去0
        // add regA, regB, #0 -> mov regA, regB
        if let MirInstKind::Addi { rd, rn, imm } = kind {
            if *imm == 0 {
                let new_inst = MirInst::mov_reg(ctx, *rd, *rn);
                return (1, Some(vec![new_inst]));
            }
        }
        if let MirInstKind::Subi { rd, rn, imm } = kind {
            if *imm == 0 {
                let new_inst = MirInst::mov_reg(ctx, *rd, *rn);
                return (1, Some(vec![new_inst]));
            }
        }

        // 模式3： 删除连续定值且无依赖关系
        // mov regA, regB
        // mov regA, regC -> 删除第一个mov
        if let Some(next) = inst.succ(ctx) {
            if !inst.is_branch(ctx) && !next.is_branch(ctx) {
                let def1 = inst.def(ctx);
                let def2 = next.def(ctx);
                if def1.len() == 1 && def2.len() == 1 {
                    let reg1 = def1[0];
                    let reg2 = def2[0];
                    let uses = next.uses(ctx);
                    if reg1 == reg2 && !uses.contains(&reg1) {
                        return (0, None);
                    }
                }
            }
        }

        // 模式4： 删除连续str和ldr
        // str regA, [regB, regC]
        // ldr regA, [regB, regC] -> 删除ldr
        if let MirInstKind::Ldr { rd, mem } = kind {
            if let Some(pre) = inst.pre(ctx) {
                if let MirInstKind::Str { rm, mem: next_mem } = pre.kind(ctx) {
                    if *rd == *rm && *mem == *next_mem {
                        return (0, None);
                    }
                }
            }
        }

        // 模式5： ldr -> mov
        // str regA, [regB, regC]
        // ldr regD, [regB, regC] -> mov regD, regA
        if let MirInstKind::Ldr { rd, mem } = kind {
            if let Some(pre) = inst.pre(ctx) {
                if let MirInstKind::Str { rm, mem: next_mem } = pre.kind(ctx) {
                    if *rd != *rm && *mem == *next_mem {
                        let new_inst = MirInst::mov_reg(ctx, *rd, *rm);
                        return (1, Some(vec![new_inst]));
                    }
                }
            }
        }

        // 模式6: 优化冗余指令
        //mov regA, regB
        //mov regC, regA -> mov regC, RegB
        if time_of_call == 1 {
            if let MirInstKind::MovReg { rd, rn } = kind {
                if let Some(pre) = inst.pre(ctx) {
                    if let MirInstKind::MovReg {
                        rd: pre_rd,
                        rn: pre_rn,
                    } = pre.kind(ctx)
                    {
                        if *rn == *pre_rd {
                            let new_inst = MirInst::mov_reg(ctx, *rd, *pre_rn);
                            return (1, Some(vec![new_inst]));
                        }
                    }
                    if let MirInstKind::Movz { rd: pre_rd, imm } = pre.kind(ctx) {
                        if *rn == *pre_rd {
                            let new_inst = MirInst::movz(ctx, *rd, *imm);
                            return (1, Some(vec![new_inst]));
                        }
                    }
                }
            }
        }

        // 模式7：乘加合并
        // mul regA, regB, regC
        // add regE, regA, regD -> madd regE, regB, regC, regD
        if time_of_call == 1 {
            if let MirInstKind::Add { rd, rn, rm } = kind {
                if let Some(pre) = inst.pre(ctx) {
                    if let MirInstKind::Mul {
                        rd: pre_rd,
                        rn: pre_rn,
                        rm: pre_rm,
                    } = pre.kind(ctx)
                    {
                        if *pre_rd != *pre_rn && *pre_rd != *pre_rm {
                            if *pre_rd == *rn {
                                let new_inst = MirInst::madd(ctx, *rd, *pre_rn, *pre_rm, *rm);
                                return (1, Some(vec![new_inst]));
                            }
                            if *pre_rd == *rm {
                                let new_inst = MirInst::madd(ctx, *rd, *pre_rn, *pre_rm, *rn);
                                return (1, Some(vec![new_inst]));
                            }
                        }
                    }
                }
            }
            if let MirInstKind::Sub { rd, rn, rm } = kind {
                if let Some(pre) = inst.pre(ctx) {
                    if let MirInstKind::Mul {
                        rd: pre_rd,
                        rn: pre_rn,
                        rm: pre_rm,
                    } = pre.kind(ctx)
                    {
                        if *pre_rd != *pre_rn && *pre_rd != *pre_rm {
                            if *pre_rd == *rm {
                                let new_inst = MirInst::msub(ctx, *rd, *pre_rn, *pre_rm, *rn);
                                return (1, Some(vec![new_inst]));
                            }
                        }
                    }
                }
            }
            if let MirInstKind::Neg { rd, rn } = kind {
                if let Some(pre) = inst.pre(ctx) {
                    if let MirInstKind::Mul {
                        rd: pre_rd,
                        rn: pre_rn,
                        rm: pre_rm,
                    } = pre.kind(ctx)
                    {
                        if *pre_rd != *pre_rn && *pre_rd != *pre_rm {
                            if *pre_rd == *rn {
                                let new_inst = MirInst::mneg(ctx, *rd, *pre_rn, *pre_rm);
                                return (1, Some(vec![new_inst]));
                            }
                        }
                    }
                }
            }
            // if let MirInstKind::Fadd { rd, rn, rm } = kind {
            //     if let Some(pre) = inst.pre(ctx) {
            //         if let MirInstKind::Fmul { rd: pre_rd, rn: pre_rn, rm: pre_rm } = pre.kind(ctx) {
            //             if *pre_rd != *pre_rn && *pre_rd != *pre_rm {
            //                 if *pre_rd == *rn {
            //                     let new_inst = MirInst::fmadd(ctx, *rd, *pre_rn, *pre_rm, *rm);
            //                     return (1, Some(new_inst));
            //                 }
            //                 if *pre_rd == *rm {
            //                     let new_inst = MirInst::fmadd(ctx, *rd, *pre_rn, *pre_rm, *rn);
            //                     return (1, Some(new_inst));
            //                 }
            //             }
            //         }
            //     }
            // }
            // if let MirInstKind::Fsub { rd, rn, rm } = kind {
            //     if let Some(pre) = inst.pre(ctx) {
            //         if let MirInstKind::Fmul { rd: pre_rd, rn: pre_rn, rm: pre_rm } = pre.kind(ctx) {
            //             if *pre_rd != *pre_rn && *pre_rd != *pre_rm {
            //                 if *pre_rd == *rm {
            //                     let new_inst = MirInst::fmsub(ctx, *rd, *pre_rn, *pre_rm, *rn);
            //                     return (1, Some(new_inst));
            //                 }
            //                 if *pre_rd == *rn {
            //                     let new_inst = MirInst::fnmsub(ctx, *rd, *pre_rn, *pre_rm, *rm);
            //                     return (1, Some(new_inst));
            //                 }
            //             }
            //         }
            //     }
            // }
            // if let MirInstKind::Fneg { rd, rn } = kind {
            //     if let Some(pre) = inst.pre(ctx) {
            //         if let MirInstKind::Fmul { rd: pre_rd, rn: pre_rn, rm: pre_rm } = pre.kind(ctx) {
            //             if *pre_rd != *pre_rn && *pre_rd != *pre_rm {
            //                 if *pre_rd == *rn {
            //                     let new_inst = MirInst::fmneg(ctx, *rd, *pre_rn, *pre_rm);
            //                     return (1, Some(new_inst));
            //                 }
            //             }
            //         }
            //     }
            // }
        }

        // 模式8: 浮点除法优化
        // fdiv rd, rn, rm -> frecpe rd, rm; fmul rd, rd, rn
        // if time_of_call == 1 {
        //     if let MirInstKind::Fdiv { rd, rn, rm } = kind {
        //         if *rd != *rn && *rd != *rm {
        //             let rd_val = *rd;
        //             let rm_val = *rm;
        //             let rn_val = *rn;
        //             let new_reg = ctx.generate_vreg(regs::RegKind::Float);
        //             let new_reg1 = ctx.generate_vreg(regs::RegKind::Float);
        //             // 然后创建指令
        //             let pre0 = MirInst::frecpe(ctx, new_reg.into(), rm_val);
        //             let pre1 = MirInst::frecps(ctx, new_reg1.into(), rm_val, new_reg.into());
        //             let pre2 = MirInst::fmul(ctx, new_reg.into(), new_reg.into(), new_reg1.into());
        //             let new_inst = MirInst::fmul(ctx, rd_val, rn_val, new_reg.into());
        //             return (3, Some(vec![pre0, pre1, pre2, new_inst]));
        //         }
        //     }
        // }
        
        (2, None)
    }
}

// 执行窥孔优化
pub fn peephole_optimize(ctx: &mut MirContext, time_of_call: usize) {
    let mut opt = PeepholeOptimizer::new();
    opt.peephole_optimize(ctx, time_of_call);
}

// 删除只有定值的指令
pub fn delete_dead_inst(ctx: &mut MirContext, time_of_call: usize) {
    let mut def_use: HashMap<Reg, bool> = HashMap::default();
    for func_data in ctx.funcs.iter() {
        let func = func_data.self_ptr();
        if !func.is_external(ctx) {
            for block in func.iter(ctx) {
                let mut curr = block.head(ctx);
                while let Some(inst) = curr {
                    let next = inst.succ(ctx);
                    for reg in inst.def(ctx) {
                        def_use.insert(reg, false);
                    }
                    curr = next;
                }
            }

            for block in func.iter(ctx) {
                let mut curr = block.head(ctx);
                while let Some(inst) = curr {
                    let next = inst.succ(ctx);
                    for reg in inst.uses(ctx) {
                        if let Some(used) = def_use.get_mut(&reg) {
                            *used = true;
                        }
                    }
                    curr = next;
                }
            }
        }
    }
    let mut insts_to_delete = Vec::new();
    for func_data in ctx.funcs.iter() {
        let func = func_data.self_ptr();
        if !func.is_external(ctx) {
            for block in func.iter(ctx) {
                let mut curr = block.head(ctx);
                if curr.is_none() {
                    continue;
                }
                let end = block.tail(ctx).unwrap();
                while let Some(inst) = curr {
                    let next = inst.succ(ctx);
                    if !inst.is_branch(ctx) && inst != end {
                        let def = inst.def(ctx);
                        if def.len() == 1 {
                            let reg = def[0];
                            if let Some(used) = def_use.get(&reg) {
                                if !(*used) {
                                    insts_to_delete.push(inst);
                                }
                            }
                        }
                    }
                    curr = next;
                }
            }
        }
    }
    // println!(
    //     "Delete never used [{}]: {} deleted",
    //     time_of_call,
    //     insts_to_delete.len()
    // );
    for inst in insts_to_delete {
        //println!("Delete unused inst: {:?}", inst.display(ctx));
        inst.unlink(ctx);
    }
}

/// continous instruction elimination (CIE) pass
/// 不移除入口块
use core::panic;
use rustc_hash::FxHashSet as HashSet;

use crate::{
    frontend::ir::{context::Context, function::Function, instruction::Instruction},
    passes::{
        pass::structure::{ControlFlowAnalysisResult, GetName},
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::{
        linked_list::{LinkedListContainer, LinkedListNode},
        storage::Idx,
    },
};

pub struct CIE;

impl Pass for CIE {
    type Output = bool;
    type Error = PassError;

    fn pass_type(&self) -> PassType {
        PassType::FunctionBasedOptimization
    }

    /// 执行遍处理
    fn execute(
        &mut self,
        input: &mut Context,
        pctx: &mut PassContext,
    ) -> Result<bool, Self::Error> {
        let mut changed = false;
        let mut _removed_num_i = 0;
        let mut _removed_num_b = 0;

        let functions: Vec<_> = input.get_functions().collect(); // 收集所有函数
        for function in functions {
            let (changed_func, removed_num_func_i, removed_num_func_b) = self.execute_function(
                &function,
                input,
                pctx.cfginfo.get_mut(&function.get_id(input)).unwrap(),
            );
            changed |= changed_func;
            _removed_num_i += removed_num_func_i;
            _removed_num_b += removed_num_func_b;
        }

        #[cfg(debug_assertions)]
        println!(
            "···[CIE]Removed {} bbks, {} insts",
            _removed_num_b, _removed_num_i
        );
        Ok(changed)
    }

    /// 获取遍名称（用于依赖管理）
    fn name(&self) -> &'static str {
        "CininuousInstructionElimination"
    }

    /// 声明依赖的遍
    fn dependencies(&self) -> &[&'static str] {
        &["CFAnalysis"]
    }

    fn effects(&self) -> &[&'static str] {
        &["DominatorRelAnalysis", "LoopSimplify"]
    }
}

impl CIE {
    fn execute_function(
        &mut self,
        input: &Function,
        ctx: &mut Context,
        cfg_info: &mut ControlFlowAnalysisResult,
    ) -> (bool, usize, usize) {
        let mut removed_num_b = 0;
        let mut removed_num_i = 0;

        // 使用深度优先搜索来遍历并分析基本块
        let mut stack = vec![input.head(ctx).unwrap()];
        let mut visited = HashSet::default();
        visited.insert(input.head(ctx).unwrap());

        // 构建控制流图
        while let Some(current_block) = stack.pop() {
            // 获取当前基本块的后继基本块
            let succesors = cfg_info.cfg.get_successors(&current_block);
            if succesors.is_none() {
                continue; // 如果没有后继基本块，跳过
            }
            // 遍历后继基本块并将其加入栈中
            for successor in succesors.unwrap() {
                if !visited.contains(&successor) {
                    visited.insert(*successor);
                    stack.push(*successor);
                }
            }

            let cur_inst = current_block.head(ctx);
            if cur_inst.unwrap().is_br(ctx) && !current_block.is_entry(ctx) {
                let tar_bbk = cur_inst.unwrap().get_operand_bbk(ctx, 0).unwrap();
                let pre_bbks = cfg_info.cfg.get_predecessors(&current_block);
                if pre_bbks.is_some() && !tar_bbk.head(ctx).unwrap().is_phi(ctx) {
                    // let num_pre = pre_bbks.unwrap().len();
                    let pre_bbks = pre_bbks.unwrap().clone();
                    for pre_bbk in pre_bbks {
                        let pre_tail = pre_bbk.get_tail(ctx);
                        if pre_tail.is_some() && pre_tail.unwrap().is_br(ctx) {
                            pre_tail.unwrap().replace_operand_bbk(ctx, 0, tar_bbk);
                            cfg_info.cfg.add_edge(&pre_bbk, &tar_bbk);
                        } else if pre_tail.is_some() && pre_tail.unwrap().is_cbr(ctx) {
                            let t_bbk = pre_tail.unwrap().get_operand_bbk(ctx, 0).unwrap();
                            let f_bbk = pre_tail.unwrap().get_operand_bbk(ctx, 1).unwrap();
                            if current_block.0.index() == t_bbk.0.index() {
                                if f_bbk.0.index() == tar_bbk.0.index() {
                                    pre_tail.unwrap().remove(ctx);
                                    let br = Instruction::br(ctx, tar_bbk);
                                    let _ = pre_bbk.push_back(ctx, br);
                                } else {
                                    pre_tail.unwrap().replace_operand_bbk(ctx, 0, tar_bbk);
                                }
                            } else if current_block.0.index() == f_bbk.0.index() {
                                if t_bbk.0.index() == tar_bbk.0.index() {
                                    pre_tail.unwrap().remove(ctx);
                                    let br = Instruction::br(ctx, tar_bbk);
                                    let _ = pre_bbk.push_back(ctx, br);
                                } else {
                                    pre_tail.unwrap().replace_operand_bbk(ctx, 1, tar_bbk);
                                }
                            } else {
                                panic!(
                                    "[CIE] Error: Cbr target block not found Target:{}, T_BBK:{}, F_BBK:{}",
                                    tar_bbk.get_name(ctx),
                                    t_bbk.get_name(ctx),
                                    f_bbk.get_name(ctx)
                                );
                            }
                            cfg_info.cfg.add_edge(&pre_bbk, &tar_bbk);
                        }
                    }
                    cfg_info.cfg.remove_bbk(&current_block);
                    current_block.remove(ctx);
                    removed_num_b += 1;
                    removed_num_i += 1;
                    continue;
                }
            } else if cur_inst.unwrap().is_cbr(ctx) && !current_block.is_entry(ctx) {
                let tar_bbk1 = cur_inst.unwrap().get_operand_bbk(ctx, 0).unwrap();
                let tar_bbk2 = cur_inst.unwrap().get_operand_bbk(ctx, 1).unwrap();
                let pre_bbks = cfg_info.cfg.get_predecessors(&current_block);
                if pre_bbks.is_some()
                    && pre_bbks.unwrap().len() == 1
                    && !tar_bbk1.head(ctx).unwrap().is_phi(ctx)
                    && !tar_bbk2.head(ctx).unwrap().is_phi(ctx)
                    && current_block != tar_bbk1
                    && current_block != tar_bbk2
                {
                    let pre_bbk = pre_bbks.unwrap().iter().next().unwrap().clone();
                    let pre_tail = pre_bbk.get_tail(ctx);
                    if pre_tail.is_some() && pre_tail.unwrap().is_br(ctx) {
                        cur_inst.unwrap().unlink(ctx);
                        pre_tail
                            .unwrap()
                            .insert_after(ctx, cur_inst.unwrap())
                            .unwrap();
                        removed_num_i += 1;
                        pre_tail.unwrap().remove(ctx);
                        cfg_info.cfg.add_edge(&pre_bbk, &tar_bbk1);
                        cfg_info.cfg.add_edge(&pre_bbk, &tar_bbk2);
                        removed_num_b += 1;
                        cfg_info.cfg.remove_bbk(&current_block);
                        current_block.remove(ctx);
                    } else if pre_tail.is_some() && pre_tail.unwrap().is_cbr(ctx) {
                        continue;
                    }
                }
            }
        }
        return (
            (removed_num_b > 0 || removed_num_i > 0),
            removed_num_i,
            removed_num_b,
        );
    }
}

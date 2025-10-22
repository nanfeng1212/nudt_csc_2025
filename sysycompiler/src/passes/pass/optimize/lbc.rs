use crate::{
    frontend::ir::{
        basicblock::BasicBlock, context::Context, function::Function, instruction::Instruction,
    },
    passes::{
        pass::structure::{ControlFlowAnalysisResult, ControlFlowGraph},
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::{LinkedListContainer, LinkedListNode},
};
use rustc_hash::FxHashSet as HashSet;
/// Linear BasicBlock Coalescing (LBC) Pass
///
/// This pass merges basic blocks that have only one predecessor and one successor.
/// This can be done because the control flow can only go in one direction.
use std::collections::VecDeque;

pub struct LinearBasicBlockCoalescing;

impl Pass for LinearBasicBlockCoalescing {
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
            "···[LBC]Removed {} bbks,  {} insts",
            _removed_num_b, _removed_num_i
        );
        Ok(changed)
    }

    /// 获取遍名称（用于依赖管理）
    fn name(&self) -> &'static str {
        "LinearBasicBlockCoalescing"
    }

    /// 声明依赖的遍
    fn dependencies(&self) -> &[&'static str] {
        &["CFAnalysis"]
    }

    fn effects(&self) -> &[&'static str] {
        &[
            "DominatorRelAnalysis",
            "LoopAnalysis",
            "FunctionCallAnalysis",
            "LoopSimplify",
        ]
    }
}

impl LinearBasicBlockCoalescing {
    fn execute_function(
        &mut self,
        input: &Function,
        ctx: &mut Context,
        cfg_info: &mut ControlFlowAnalysisResult,
    ) -> (bool, usize, usize) {
        let mut removed_num_b = 0;
        let mut removed_num_i = 0;

        let mut worklist = VecDeque::from([input.head(ctx).unwrap()]);
        let mut visited = HashSet::default();
        visited.insert(input.head(ctx).unwrap());

        // 遍历基本块
        while let Some(current_block) = worklist.pop_front() {
            // 获取当前基本块的后继基本块
            let succesors = cfg_info.cfg.get_successors(&current_block).unwrap().clone();
            if succesors.is_empty() {
                continue; // 如果没有后继基本块，跳过
            }
            let succ_of_cur = succesors.len();
            if succ_of_cur == 1 {
                let successor = succesors.iter().next().unwrap();
                let pre_of_succ = cfg_info.cfg.get_predecessors(&successor).unwrap().len();
                if pre_of_succ == 1 && !successor.is_ret(ctx) {
                    // 如果后继基本块只有一个前驱，当前基本块只有一个后继，则可以进行合并
                    // 不合并ret块保证exit block的存在且唯一
                    // println!(
                    //     "Function: \n{}",
                    //     crate::frontend::ir2string::Display::display(*input, ctx)
                    // );
                    Self::coalesce_bbks(ctx, &mut cfg_info.cfg, &vec![current_block], &successor);
                    removed_num_b += 1;
                    removed_num_i += 1;
                    // 如果发生了合并需要把基本块放回去，有可能继续合并
                    visited.remove(&current_block);
                    worklist.push_back(current_block);
                }
            } else {
                // 遍历后继基本块并将其加入栈中
                for successor in succesors {
                    if !visited.contains(&successor) {
                        visited.insert(successor);
                        worklist.push_back(successor);
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

    pub fn coalesce_bbks(
        ctx: &mut Context,
        cfg: &mut ControlFlowGraph,
        bbks: &Vec<BasicBlock>,
        successor: &BasicBlock,
    ) {
        // println!(
        //     "coalesce_bbks: bbks: \n{}, \nsuccessor: \n{}",
        //     crate::frontend::ir2string::Display::display(bbks[0], ctx),
        //     crate::frontend::ir2string::Display::display(*successor, ctx)
        // );
        for cur_bbk in bbks {
            if let Some(tail) = cur_bbk.get_tail(ctx) {
                let insts = successor.iter(ctx).collect::<Vec<Instruction>>();
                for inst in insts {
                    inst.unlink(ctx);
                    let push_result = cur_bbk.push_back(ctx, inst);
                    if let Err(e) = push_result {
                        println!("Error: {:?}", e);
                    }
                }
                tail.remove(ctx)
            };
            let succ_succs = cfg.get_successors(successor).unwrap().clone();
            for succ_succ in succ_succs {
                cfg.add_edge(cur_bbk, &succ_succ);
                let insts = succ_succ.iter(ctx).collect::<Vec<Instruction>>();
                for inst in insts {
                    if inst.is_phi(ctx) {
                        // println!(
                        //     "Successor_succ has phi: {}",
                        //     crate::frontend::ir2string::Display::display(inst, ctx)
                        // );
                        //后继如果有phi，需要进行修改
                        let to_value = inst.get_phi_operand(ctx, *successor).unwrap();
                        inst.replace_phi_operand(ctx, *successor, *cur_bbk, to_value);
                    } else {
                        break;
                    }
                }
            }
        }
        cfg.remove_bbk(successor);
        let _is_removed = successor.remove_only(ctx);
        // debug_assert!(is_removed, "Failed to remove successor");
    }
}

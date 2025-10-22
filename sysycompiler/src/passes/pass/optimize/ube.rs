use std::collections::VecDeque;

// unreachble block elimination pass
use crate::{
    frontend::ir::{basicblock::BasicBlock, context::Context},
    passes::{
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::{LinkedListContainer, LinkedListNode},
};

pub struct UBE;

impl Pass for UBE {
    type Output = bool; // 输出数据类型
    type Error = PassError; // 错误类型

    /// 遍类型
    fn pass_type(&self) -> PassType {
        PassType::GlobalBasedOptimization
    }

    /// 执行遍处理
    fn execute(&mut self, input: &mut Context, pctx: &mut PassContext) -> Result<bool, PassError> {
        let mut changed = false;
        let mut to_remove: VecDeque<BasicBlock> = VecDeque::new();
        for func in input.get_functions() {
            let unreachable = &pctx
                .cfginfo
                .get(&func.get_id(input))
                .unwrap()
                .unreachable_blocks;
            for bbk in unreachable {
                to_remove.push_back(*bbk);
            }
        }

        changed |= !to_remove.is_empty();
        #[cfg(debug_assertions)]
        println!("···Removed {} BasicBlocks", to_remove.len());

        for bbk in to_remove {
            let tail = bbk.tail(input);
            if let Some(tail) = tail {
                //可能存在不可达基本块在死代码删除后导致基本块为空
                if tail.is_br(input) {
                    let tar_bbk = tail.get_operand_bbk(input, 0).unwrap();
                    Self::check_phis(input, tar_bbk, bbk);
                } else if tail.is_cbr(input) {
                    let tar_bbk1 = tail.get_operand_bbk(input, 0).unwrap();
                    let tar_bbk2 = tail.get_operand_bbk(input, 1).unwrap();
                    Self::check_phis(input, tar_bbk1, bbk);
                    Self::check_phis(input, tar_bbk2, bbk);
                }
            }
            bbk.remove_without_check(input);
        }
        // while let Some(bbk) = to_remove.pop_front() {
        //     //这个地方需要考虑BBK的删除实现，bbk在删除时如果使用着不为空，是不会出删除的
        //     // 所及重新加入队列，等待使用者使用删除完毕后再删除
        //     if !bbk.remove(input) {
        //         to_remove.push_back(bbk);
        //     }
        // }

        Ok(changed)
    }

    /// 获取遍名称（用于依赖管理）
    fn name(&self) -> &'static str {
        "UnreachableBlockElimination"
    }

    /// 声明依赖的遍
    fn dependencies(&self) -> &[&'static str] {
        &["CFAnalysis"]
    }

    fn effects(&self) -> &[&'static str] {
        &[]
    }
}

impl UBE {
    fn check_phis(input: &mut Context, tar_bbk: BasicBlock, bbk: BasicBlock) {
        if !tar_bbk.is_removed(input) {
            let mut cur_inst = tar_bbk.head(input);
            while let Some(inst) = cur_inst {
                if inst.is_phi(input) {
                    inst.remove_phi_operand(input, bbk);
                } else {
                    break;
                }
                cur_inst = inst.succ(input);
            }
        }
    }
}

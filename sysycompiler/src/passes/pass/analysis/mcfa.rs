use crate::{
    backend::mir::{mir_block::MirBlock, mir_context::MirContext, mir_function::MirFunction},
    passes::{
        pass::structure::ControlFlowGraph,
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

pub struct MirCFAnalysis;

impl Pass<MirContext> for MirCFAnalysis {
    type Output = bool;
    type Error = PassError;

    fn pass_type(&self) -> PassType {
        PassType::FunctionBasedAnalysis
    }

    /// 执行遍处理
    fn execute(
        &mut self,
        input: &mut MirContext,
        pmctx: &mut PassContext,
    ) -> Result<bool, Self::Error> {
        pmctx.mcfginfo.clear();
        let mut changed = false;
        let functions: Vec<_> = input.get_functions().collect(); // 收集所有函数
        for function in functions {
            let res = self.execute_function(&function, input);
            match res {
                Ok((tmp, fcfg)) => {
                    changed |= tmp;
                    pmctx.mcfginfo.insert(function.get_id(input), fcfg);
                }
                Err(e) => return Err(e),
            }
        }
        Ok(changed)
    }

    /// 获取遍名称（用于依赖管理）
    fn name(&self) -> &'static str {
        "MirCFAnalysis"
    }

    /// 声明依赖的遍
    fn dependencies(&self) -> &[&'static str] {
        &[]
    }

    /// 声明产生的影响
    fn effects(&self) -> &[&'static str] {
        &[]
    }
}

impl MirCFAnalysis {
    fn execute_function(
        &mut self,
        input: &MirFunction,
        mctx: &mut MirContext,
    ) -> Result<(bool, ControlFlowGraph<MirBlock>), PassError> {
        let changed = false;
        if let Some(entry) = input.get_head(mctx) {
            // 创建控制流图
            let mut cfg = ControlFlowGraph {
                blocks: HashSet::default(),
                entry_block: Some(entry),
                exit_blocks: Vec::new(),
                pre_set: HashMap::default(),
                succ_set: HashMap::default(),
            };

            // 使用深度优先搜索来遍历并分析基本块
            let mut stack = vec![cfg.entry_block.unwrap()];
            let mut visited = HashSet::default();
            visited.insert(cfg.entry_block.unwrap());

            // 构建控制流图
            while let Some(current_block) = stack.pop() {
                // 将当前基本块添加到控制流图中
                cfg.blocks.insert(current_block);
                cfg.succ_set.insert(current_block, HashSet::default());

                // 获取当前基本块的后继基本块
                let tail_inst = current_block.get_tail(mctx);
                // 如果当前基本块是终止基本块（即没有后继），则将其标记为出口块
                if tail_inst.is_none() || !tail_inst.unwrap().is_br(mctx) {
                    cfg.exit_blocks.push(current_block);
                    continue;
                }
                let before_tail = tail_inst.unwrap().get_pre(mctx);
                let mut succesors = vec![];
                // println!("MCFA: current_block: \n {} \n tail_inst: {}", current_block.dbg(mctx), tail_inst.unwrap().display(mctx));
                if before_tail.is_some() && before_tail.unwrap().is_cbr(mctx) {
                    succesors.push(before_tail.unwrap().get_operand_bbk(mctx));
                }
                succesors.push(tail_inst.unwrap().get_operand_bbk(mctx));

                // 遍历后继基本块并将其加入栈中
                for successor in succesors {
                    if !visited.contains(&successor) {
                        visited.insert(successor);
                        stack.push(successor);
                        cfg.pre_set.insert(successor, HashSet::default());
                    }
                    // 将当前基本块添加到后继基本块的前趋集合中
                    cfg.pre_set
                        .get_mut(&successor)
                        .unwrap()
                        .insert(current_block);
                    // 将后继基本块添加到当前基本块的后继集合中
                    cfg.succ_set
                        .get_mut(&current_block)
                        .unwrap()
                        .insert(successor);
                }
            }

            Ok((changed, cfg))
        } else {
            let cfg = ControlFlowGraph {
                blocks: HashSet::default(),
                entry_block: None,
                exit_blocks: Vec::new(),
                pre_set: HashMap::default(),
                succ_set: HashMap::default(),
            };
            // 如果没有入口基本块，直接返回
            return Ok((changed, cfg));
        }
    }
}

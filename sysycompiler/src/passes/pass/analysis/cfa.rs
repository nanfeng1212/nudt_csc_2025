use core::panic;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::collections::VecDeque;

use crate::{
    frontend::ir::{context::Context, function::Function},
    passes::{
        pass::structure::{ControlFlowAnalysisResult, ControlFlowGraph, GetName},
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::LinkedListContainer,
};

pub struct CFAnalysis;

impl Pass for CFAnalysis {
    type Output = bool;
    type Error = PassError;

    fn pass_type(&self) -> PassType {
        PassType::FunctionBasedAnalysis
    }

    /// 执行遍处理
    fn execute(
        &mut self,
        input: &mut Context,
        pctx: &mut PassContext,
    ) -> Result<bool, Self::Error> {
        pctx.cfginfo.clear();
        let mut changed = false;
        let functions: Vec<_> = input.get_functions().collect(); // 收集所有函数
        for function in functions {
            let res = Self::execute_function(&function, input);
            match res {
                Ok((tmp, fcfg)) => {
                    changed |= tmp;
                    pctx.cfginfo.insert(function.get_id(input), fcfg);
                }
                Err(e) => {
                    panic!("[CFAnalysis] Execution failed: {:?}", e);
                }
            }
        }
        Ok(changed)
    }

    /// 获取遍名称（用于依赖管理）
    fn name(&self) -> &'static str {
        "CFAnalysis"
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

impl CFAnalysis {
    pub fn execute_function(
        input: &Function,
        ctx: &Context,
    ) -> Result<(bool, ControlFlowAnalysisResult), PassError> {
        let changed = false;
        if let Some(entry) = input.head(ctx) {
            // 创建控制流图
            let mut cfg = ControlFlowAnalysisResult {
                cfg: ControlFlowGraph {
                    blocks: HashSet::default(),
                    entry_block: Some(input.head(ctx).unwrap()),
                    exit_blocks: Vec::new(),
                    pre_set: HashMap::default(),
                    succ_set: HashMap::default(),
                },
                reachable_blocks: HashSet::default(),
                unreachable_blocks: HashSet::default(),
            };
            cfg.reachable_blocks.insert(entry);

            // 使用深度优先搜索来遍历并分析基本块
            let mut stack = VecDeque::new();
            stack.push_back(entry);
            let mut visited = HashSet::default();
            visited.insert(entry);
            cfg.cfg.pre_set.entry(entry).or_default();

            // 构建控制流图
            while let Some(current_block) = stack.pop_front() {
                // 将当前基本块添加到控制流图中
                cfg.cfg.blocks.insert(current_block);
                cfg.cfg.succ_set.insert(current_block, HashSet::default());

                // 获取当前基本块的后继基本块
                let tail_inst = current_block.get_tail(ctx).unwrap();
                let succesors = tail_inst.get_operand_bbks(ctx);

                // 遍历后继基本块并将其加入栈中
                for successor in succesors {
                    if !visited.contains(&successor) {
                        visited.insert(successor);
                        stack.push_back(successor);
                        cfg.reachable_blocks.insert(successor);
                        cfg.cfg.pre_set.insert(successor, HashSet::default());
                    }
                    // 将当前基本块添加到后继基本块的前趋集合中
                    cfg.cfg
                        .pre_set
                        .get_mut(&successor)
                        .unwrap()
                        .insert(current_block);
                    // 将后继基本块添加到当前基本块的后继集合中
                    cfg.cfg
                        .succ_set
                        .get_mut(&current_block)
                        .unwrap()
                        .insert(successor);
                }

                // 如果当前基本块是终止基本块（即没有后继），则将其标记为出口块
                if current_block.is_ret(ctx) {
                    cfg.cfg.exit_blocks.push(current_block);
                }
            }

            for bbk in input.iter(ctx) {
                if !cfg.reachable_blocks.contains(&bbk) {
                    cfg.unreachable_blocks.insert(bbk);
                }
            }
            if cfg.cfg.exit_blocks.len() != 1 {
                panic!(
                    "Function [{}] has more than one exit block or no exit block",
                    input.get_id(ctx)
                );
            }
            Ok((changed, cfg))
        } else {
            let cfg = ControlFlowAnalysisResult {
                cfg: ControlFlowGraph {
                    blocks: HashSet::default(),
                    entry_block: None,
                    exit_blocks: Vec::new(),
                    pre_set: HashMap::default(),
                    succ_set: HashMap::default(),
                },
                reachable_blocks: HashSet::default(),
                unreachable_blocks: HashSet::default(),
            };
            // 如果没有入口基本块，直接返回
            return Ok((changed, cfg));
        }
    }
}

impl ControlFlowAnalysisResult {
    /// 输出控制流图（CFG）
    pub fn print_cfg(&self, ctx: &Context) {
        println!("控制流图 (CFG):");
        println!("入口块: {}", self.cfg.entry_block.unwrap().get_name(ctx));
        println!(
            "出口块: {:?}",
            self.cfg
                .exit_blocks
                .iter()
                .map(|&b| b.get_name(ctx))
                .collect::<Vec<_>>()
        );
        println!(
            "基本块: {:?}",
            self.cfg
                .blocks
                .iter()
                .map(|&b| b.get_name(ctx))
                .collect::<Vec<_>>()
        );
        println!("\n基本块关系:");

        for (block, successors) in &self.cfg.succ_set {
            println!(
                "  {} -> {:?}",
                block.get_name(ctx),
                successors
                    .iter()
                    .map(|&b| b.get_name(ctx))
                    .collect::<Vec<_>>()
            );
        }
    }
}

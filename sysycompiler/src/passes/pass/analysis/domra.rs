/// Dominator Realtionship Analysis
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;

use crate::{frontend::ir::{
        basicblock::BasicBlock, context::Context, function::Function
}, passes::{pass::structure::{ControlFlowAnalysisResult, DominatorInfo}, pass_context::PassContext, pass_manager::{Pass, PassError, PassType}}};

pub struct DomRelAnalysis;

impl Pass for DomRelAnalysis {
    type Output = bool;
    type Error  = PassError;

    fn pass_type(&self) -> PassType {
        PassType::FunctionBasedAnalysis
    }
    
    /// 执行遍处理
    fn execute( &mut self, input: &mut Context, pctx: &mut PassContext ) -> Result<bool, Self::Error>{
        let mut changed = false;
        let functions: Vec<_> = input.get_functions().collect(); // 收集所有函数
        for function in &functions {
            let res = self.execute_function(&function, input, pctx.cfginfo.get(&function.get_id(input)).unwrap());
            match res {
                Ok((tmp, fdom_info)) => {
                    changed |= tmp;
                    pctx.dom_info.insert(function.get_id(input), fdom_info);
                }
                Err(e) => return Err(e),
            }
        }
        // for function in functions {
        //     if let Some(dom_info) = pctx.dom_info.get(&function.get_id(input)) {
        //         println!("IDominator Info for function: {}", function.get_id(input));
        //         dom_info.display_idominators(input);
        //     }
        // }
        Ok(changed)
    }

    /// 获取遍名称（用于依赖管理）
    fn name(&self) -> &'static str{
        "DominatorRelAnalysis"
    }
    
    /// 声明依赖的遍
    fn dependencies(&self) -> &[&'static str] { &["CFAnalysis"] }

    /// 产生的影响
    fn effects(&self) -> &[&'static str] { &[] }
}

impl DomRelAnalysis {

    fn execute_function( &mut self, input: &Function, ctx: &mut Context, cfg: &ControlFlowAnalysisResult) -> Result<(bool, DominatorInfo), PassError>{
        let changed = false;
        if let Some(_entry) = input.get_head(ctx) {
            // 创建控制流图
            let mut dom_info =  DominatorInfo {
                    entry: cfg.cfg.entry_block,
                    dominators: HashMap::new(),
                    idominators: HashMap::new(),
                    dominance_frontier: HashMap::new(),
                    phantom: PhantomData,
            };

            // 构建必经节点集合
            self.compute_dominators(&mut dom_info, cfg);

            // 计算必经节点树
            self.compute_idominators(&mut dom_info, cfg);

            // 构建支配边界集合
            self.compute_dominance_frontier(&mut dom_info, cfg);
         
            Ok((changed, dom_info))
        } else {
            let dom_info = DominatorInfo {
                    entry: None,
                    dominators: HashMap::new(),
                    idominators: HashMap::new(),
                    dominance_frontier: HashMap::new(),
                    phantom: PhantomData,
            };
            // 如果没有入口基本块，直接返回
            return Ok((changed, dom_info));
        }
    }

    fn compute_dominators(&self, dom_info: &mut DominatorInfo, cfg: &ControlFlowAnalysisResult) {
        let all_blocks = &cfg.reachable_blocks;
        let entry = cfg.cfg.entry_block.expect("Entry block must exist");
        
        // 初始化支配节点集合
        let mut dominators = HashMap::new();
        
        // 入口节点的支配节点是它自己
        let mut entry_doms = HashSet::new();
        entry_doms.insert(entry);
        dominators.insert(entry, entry_doms);
        
        // 其他节点的支配节点初始化为所有节点
        for &block in all_blocks.iter().filter(|&&b| b != entry) {
            dominators.insert(block, all_blocks.clone());
        }
        
        // 迭代直到收敛
        let mut changed = true;
        while changed {
            changed = false;
            
            for &block in all_blocks.iter().filter(|&&b| b != entry) {
                // 计算前驱支配节点的交集
                let mut new_doms = if let Some(preds) = cfg.cfg.pre_set.get(&block) {
                    preds.iter()
                        .filter_map(|pred| dominators.get(pred))
                        .fold(all_blocks.clone(), |acc, set| {
                            acc.intersection(set).copied().collect()
                        })
                } else {
                    HashSet::new()
                };
                
                // 添加自身
                new_doms.insert(block);
                
                // 更新支配节点集合
                if new_doms != *dominators.get(&block).unwrap() {
                    dominators.insert(block, new_doms);
                    changed = true;
                }
            }
        }
        
        // 更新到CFG结构中
        dom_info.dominators = dominators;
    }

    fn compute_idominators(&self, dom_info: &mut DominatorInfo, cfg: &ControlFlowAnalysisResult) {
        let dominators = &dom_info.dominators;
        let entry = cfg.cfg.entry_block.expect("Entry block must exist");

        // 可选：先预处理每个块的支配深度
        let mut depth = HashMap::new();
        for &b in cfg.reachable_blocks.iter() {
            let mut d = 0;
            let mut cur = b;
            while cur != entry {
                // 取支配树父亲
                let parent = dominators[&cur].iter().cloned().filter(|&x| x != cur).max_by_key(|x| dominators[x].len());
                if let Some(p) = parent {
                    cur = p;
                    d += 1;
                } else {
                    break;
                }
            }
            depth.insert(b, d);
        }

        let mut idoms = HashMap::new();
        idoms.insert(entry, entry);

        for &b in cfg.reachable_blocks.iter().filter(|&&b| b != entry) {
            let mut candidates = dominators[&b].clone();
            candidates.remove(&b);
            if candidates.is_empty() { continue; }
            // 用深度最大选法
            let idom = candidates.into_iter().max_by_key(|d| depth[d]).unwrap();
            idoms.insert(b, idom);
        }

        dom_info.idominators = idoms;
    }

    /// 计算支配边界（Dominance Frontier）
    fn compute_dominance_frontier(&self, dom_info: &mut DominatorInfo ,cfg: &ControlFlowAnalysisResult) {
        // 初始化支配边界为空的HashSet
        let mut dominance_frontier: HashMap<BasicBlock, HashSet<BasicBlock>> = cfg
            .reachable_blocks
            .iter()
            .map(|&block| (block, HashSet::new()))
            .collect();
        
        // 获取入口块
        let entry = cfg.cfg.entry_block.expect("Entry block must exist");
        
        // 遍历所有基本块（除了入口块）
        for &block in cfg.reachable_blocks.iter().filter(|&&b| b != entry) {
            // 获取当前块的前驱
            if let Some(predecessors) = cfg.cfg.pre_set.get(&block) {
                for &pred in predecessors {
                    // 从每个前驱开始向上遍历支配树
                    let mut runner = pred;
                    
                    // 获取当前块的直接支配者
                    let idom_n = *dom_info.idominators.get(&block)
                        .expect("Immediate dominator should exist");
                    
                    // 向上遍历直到遇到block的直接支配者
                    while runner != idom_n {
                        // 将当前块加入runner的支配边界
                        dominance_frontier
                            .get_mut(&runner)
                            .unwrap()
                            .insert(block);
                        
                        // 向上移动到runner的直接支配者
                        runner = *dom_info.idominators.get(&runner)
                            .expect("Immediate dominator should exist");
                    }
                }
            }
        }
        
        // 更新到CFG结构中
        dom_info.dominance_frontier = dominance_frontier;
    }

}





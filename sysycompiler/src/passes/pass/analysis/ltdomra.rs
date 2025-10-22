/// Dominator Realtionship Analysis
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::marker::PhantomData;

use crate::utils::linked_list::LinkedListContainer;
use crate::{
    frontend::ir::{basicblock::BasicBlock, context::Context, function::Function},
    passes::{
        pass::structure::{ControlFlowAnalysisResult, DominatorInfo},
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
};

pub struct LTDomRelAnalysis;

impl Pass for LTDomRelAnalysis {
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
        pctx.dom_info.clear();
        let mut changed = false;
        let functions: Vec<_> = input.get_functions().collect(); // 收集所有函数
        for function in &functions {
            let res = self.execute_function(
                &function,
                input,
                pctx.cfginfo.get(&function.get_id(input)).unwrap(),
            );
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
    fn name(&self) -> &'static str {
        "DominatorRelAnalysis"
    }

    /// 声明依赖的遍
    fn dependencies(&self) -> &[&'static str] {
        &["CFAnalysis"]
    }

    /// 产生的影响
    fn effects(&self) -> &[&'static str] {
        &[]
    }
}

impl LTDomRelAnalysis {
    pub fn execute_function(
        &mut self,
        input: &Function,
        ctx: &mut Context,
        cfg: &ControlFlowAnalysisResult,
    ) -> Result<(bool, DominatorInfo), PassError> {
        let changed = false;
        if let Some(_entry) = input.head(ctx) {
            // 创建控制流图
            let mut dom_info = DominatorInfo {
                entry: cfg.cfg.entry_block,
                dominators: HashMap::default(),
                idominators: HashMap::default(),
                dominance_frontier: HashMap::default(),
                phantom: PhantomData,
            };

            // 计算必经节点树
            self.compute_idominators(&mut dom_info, cfg, ctx);

            // 构建必经节点集合
            self.compute_dominators(&mut dom_info, cfg);

            // 构建支配边界集合
            self.compute_dominance_frontier(&mut dom_info, cfg);

            Ok((changed, dom_info))
        } else {
            let dom_info = DominatorInfo {
                entry: None,
                dominators: HashMap::default(),
                idominators: HashMap::default(),
                dominance_frontier: HashMap::default(),
                phantom: PhantomData,
            };
            // 如果没有入口基本块，直接返回
            return Ok((changed, dom_info));
        }
    }

    fn compute_idominators(
        &self,
        dom_info: &mut DominatorInfo,
        cfg: &ControlFlowAnalysisResult,
        ctx: &mut Context,
    ) {
        let entry = match cfg.cfg.entry_block {
            Some(entry) => entry,
            None => return, // 无入口块直接返回
        };
        let n0 = BasicBlock::new(ctx);

        // 步骤1: 初始化数据结构
        let mut ancestor: HashMap<BasicBlock, BasicBlock> = HashMap::default();
        let mut parent: HashMap<BasicBlock, BasicBlock> = HashMap::default();
        let mut child: HashMap<BasicBlock, BasicBlock> = HashMap::default(); // 用于保持树林中树的平衡，使算法达到时间下界
        let mut label: HashMap<BasicBlock, BasicBlock> = HashMap::default(); // Label(v) 是 v的祖先链中其必经节点的深度为主编号最小的一个节点
        let mut sdno: HashMap<BasicBlock, usize> = HashMap::default(); // 节点v半支配节点的深度为主编好
        let mut size: HashMap<BasicBlock, usize> = HashMap::default(); // 用于保持树林中树的平衡，使算法达到时间下界
        let mut idom: HashMap<BasicBlock, BasicBlock> = HashMap::default(); // 节点v的直接支配节点
        let mut bucket: HashMap<BasicBlock, HashSet<BasicBlock>> = HashMap::default(); // 其半必经节点是Ndfs(v)的那些节点集合
        let mut ndfs: HashMap<usize, BasicBlock> = HashMap::default(); // 节点v的半必经节点
        sdno.insert(n0, 0);
        size.insert(n0, 0);
        label.insert(n0, n0);
        ancestor.insert(n0, n0);
        bucket.insert(n0, HashSet::default());
        sdno.insert(n0, 0);
        for bbk in cfg.reachable_blocks.iter() {
            bucket.insert(*bbk, HashSet::default());
            sdno.insert(*bbk, 0);
        }
        let mut n = 0;
        Self::depth_first_search_dom(
            entry,
            cfg,
            n0,
            &mut sdno,
            &mut ndfs,
            &mut label,
            &mut ancestor,
            &mut child,
            &mut parent,
            &mut size,
            &mut n,
        );
        for i in (2..=n).rev() {
            // 计算半支配节点的初始值，将有相同半支配节点的节点放到同一bucket
            let w = ndfs[&i];
            for v in cfg.cfg.pre_set[&w].iter() {
                let u = Self::eval(*v, &mut ancestor, &mut sdno, &mut label, n0);
                if sdno[&u] < sdno[&w] {
                    sdno.insert(w, sdno[&u]);
                }
            }
            bucket.get_mut(&ndfs[&sdno[&w]]).unwrap().insert(w);
            Self::link(
                parent[&w],
                w,
                &mut ancestor,
                &mut sdno,
                &mut label,
                &mut size,
                &mut child,
                n0,
            );
            // 计算w paren的bucket中节点的直接必经节点
            let mut bucket_len = bucket[&parent[&w]].len();
            while bucket_len > 0 {
                let v = *bucket[&parent[&w]].iter().next().unwrap();
                bucket.get_mut(&parent[&w]).unwrap().remove(&v);
                let u = Self::eval(v, &mut ancestor, &mut sdno, &mut label, n0);
                if sdno[&u] < sdno[&v] {
                    idom.insert(v, u);
                } else {
                    idom.insert(v, parent[&w]);
                }
                bucket_len = bucket[&parent[&w]].len();
            }
        }
        // 调整现在节点v 直接必经节点 和 v的半支配节点的深度主编号 不一致的v的直接必经支配节点
        for i in 2..=n {
            let w = ndfs[&i];
            if idom[&w] != ndfs[&sdno[&w]] {
                idom.insert(w, idom[&idom[&w]]);
            }
        }
        n0.remove(ctx);
        dom_info.idominators = idom;
    }

    /// DFS遍历分配序号
    fn depth_first_search_dom(
        v: BasicBlock,
        cfg: &ControlFlowAnalysisResult,
        n0: BasicBlock,
        sdno: &mut HashMap<BasicBlock, usize>,
        ndfs: &mut HashMap<usize, BasicBlock>,
        label: &mut HashMap<BasicBlock, BasicBlock>,
        ancestor: &mut HashMap<BasicBlock, BasicBlock>,
        child: &mut HashMap<BasicBlock, BasicBlock>,
        parent: &mut HashMap<BasicBlock, BasicBlock>,
        size: &mut HashMap<BasicBlock, usize>,
        n: &mut usize,
    ) {
        *n = *n + 1;
        sdno.insert(v, *n);
        ndfs.insert(*n, v);
        label.insert(v, v);
        ancestor.insert(v, n0);
        child.insert(v, n0);
        size.insert(v, 1);
        for w in cfg.cfg.succ_set[&v].iter() {
            if sdno[w] == 0 {
                parent.insert(*w, v);
                Self::depth_first_search_dom(
                    *w, cfg, n0, sdno, ndfs, label, ancestor, child, parent, size, n,
                );
            }
        }
    }

    /// 压缩（祖先通向v的路径）到有着最大半支配数的节点
    fn compress(
        v: BasicBlock,
        ancestor: &mut HashMap<BasicBlock, BasicBlock>,
        sdno: &mut HashMap<BasicBlock, usize>,
        label: &mut HashMap<BasicBlock, BasicBlock>,
        n0: BasicBlock,
    ) {
        if ancestor[&ancestor[&v]] != n0 {
            Self::compress(ancestor[&v], ancestor, sdno, label, n0);
            if sdno[&label[&ancestor[&v]]] < sdno[&label[&v]] {
                label.insert(v, label[&ancestor[&v]]);
            }
            ancestor.insert(v, ancestor[&ancestor[&v]]);
        }
    }

    /// 判断v的祖先中谁的半支配节点有着
    fn eval(
        v: BasicBlock,
        ancestor: &mut HashMap<BasicBlock, BasicBlock>,
        sdno: &mut HashMap<BasicBlock, usize>,
        label: &mut HashMap<BasicBlock, BasicBlock>,
        n0: BasicBlock,
    ) -> BasicBlock {
        if ancestor[&v] == n0 {
            return label[&v];
        } else {
            Self::compress(v, ancestor, sdno, label, n0);
            if sdno[&label[&ancestor[&v]]] >= sdno[&label[&v]] {
                return label[&v];
            } else {
                return label[&ancestor[&v]];
            }
        }
    }

    pub fn link(
        v: BasicBlock,
        w: BasicBlock,
        ancestor: &mut HashMap<BasicBlock, BasicBlock>,
        sdno: &mut HashMap<BasicBlock, usize>,
        label: &mut HashMap<BasicBlock, BasicBlock>,
        size: &mut HashMap<BasicBlock, usize>,
        child: &mut HashMap<BasicBlock, BasicBlock>,
        n0: BasicBlock,
    ) {
        let mut s = w;
        while sdno[&label[&w]] < sdno[&label[&child[&s]]] {
            if size[&s] + size[&child[&child[&s]]] >= 2 * size[&child[&s]] {
                ancestor.insert(child[&s], s);
                child.insert(s, child[&child[&s]]);
            } else {
                size.insert(child[&s], size[&s]);
                ancestor.insert(s, child[&s]);
                s = ancestor[&s];
            }
        }
        label.insert(s, label[&w]);
        size.insert(v, size[&v] + size[&w]);
        if size[&v] < 2 * size[&w] {
            let tmp = s;
            s = child[&v];
            child.insert(v, tmp);
        }
        while s != n0 {
            ancestor.insert(s, v);
            s = child[&s];
        }
    }

    /// 转换序号为基本块映射
    fn compute_dominators(&self, dom_info: &mut DominatorInfo, cfg: &ControlFlowAnalysisResult) {
        let entry = cfg.cfg.entry_block.expect("Entry block must exist");
        let mut dominators_map = HashMap::default();

        let mut children: HashMap<BasicBlock, HashSet<BasicBlock>> = HashMap::default();
        for (node, parent) in dom_info.idominators.iter() {
            if node != &entry {
                children.entry(*parent).or_default().insert(*node);
            }
        }

        let mut stack = vec![(entry, HashSet::from_iter([entry]))];
        while let Some((node, dominators)) = stack.pop() {
            dominators_map.insert(node, dominators.clone());

            if let Some(child_list) = children.get(&node) {
                for &child in child_list {
                    let mut child_dominators = dominators.clone();
                    child_dominators.insert(child);
                    stack.push((child, child_dominators));
                }
            }
        }
        dom_info.dominators = dominators_map;
    }

    /// 计算支配边界（Dominance Frontier）
    fn compute_dominance_frontier(
        &self,
        dom_info: &mut DominatorInfo,
        cfg: &ControlFlowAnalysisResult,
    ) {
        // 初始化支配边界为空的HashSet
        let mut dominance_frontier: HashMap<BasicBlock, HashSet<BasicBlock>> = cfg
            .reachable_blocks
            .iter()
            .map(|&block| (block, HashSet::default()))
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
                    let idom_n = *dom_info
                        .idominators
                        .get(&block)
                        .expect("Immediate dominator should exist");

                    // 向上遍历直到遇到block的直接支配者
                    while runner != idom_n {
                        // 将当前块加入runner的支配边界
                        dominance_frontier.get_mut(&runner).unwrap().insert(block);

                        // 向上移动到runner的直接支配者
                        runner = *dom_info
                            .idominators
                            .get(&runner)
                            .expect("Immediate dominator should exist");
                    }
                }
            }
        }

        // 更新到CFG结构中
        dom_info.dominance_frontier = dominance_frontier;
    }
}

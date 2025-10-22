use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::marker::PhantomData;

/// 控制流图
pub struct ControlFlowGraph<BasicBlock = crate::frontend::ir::basicblock::BasicBlock> {
    pub blocks: HashSet<BasicBlock>,                        // 基本块集合
    pub entry_block: Option<BasicBlock>,                    // 入口块ID
    pub exit_blocks: Vec<BasicBlock>,                       // 出口块ID列表
    pub pre_set: HashMap<BasicBlock, HashSet<BasicBlock>>,  // 前趋关系
    pub succ_set: HashMap<BasicBlock, HashSet<BasicBlock>>, // 后继关系
}

impl<BasicBlock> ControlFlowGraph<BasicBlock>
where
    BasicBlock: Clone + Eq + std::hash::Hash,
{
    pub fn get_exit_bbks(&self) -> &Vec<BasicBlock> {
        &self.exit_blocks
    }

    pub fn get_successors(&self, block: &BasicBlock) -> Option<&HashSet<BasicBlock>> {
        self.succ_set.get(block)
    }

    pub fn get_predecessors(&self, block: &BasicBlock) -> Option<&HashSet<BasicBlock>> {
        self.pre_set.get(block)
    }

    pub fn remove_edge(&mut self, from: &BasicBlock, to: &BasicBlock) {
        if let Some(succs) = self.succ_set.get_mut(from) {
            succs.remove(to);
        }
        if let Some(preds) = self.pre_set.get_mut(to) {
            preds.remove(from);
        }
    }

    pub fn add_edge(&mut self, from: &BasicBlock, to: &BasicBlock) {
        if let Some(succs) = self.succ_set.get_mut(from) {
            succs.insert(to.clone());
        } else {
            self.succ_set.insert(from.clone(), HashSet::default());
            self.succ_set.get_mut(from).unwrap().insert(to.clone());
        }
        if let Some(preds) = self.pre_set.get_mut(to) {
            preds.insert(from.clone());
        } else {
            self.pre_set.insert(to.clone(), HashSet::default());
            self.pre_set.get_mut(to).unwrap().insert(from.clone());
        }
    }

    pub fn remove_bbk(&mut self, block: &BasicBlock) {
        // 从所有集合中移除该基本块
        self.blocks.remove(block);
        self.pre_set.remove(block);
        self.succ_set.remove(block);

        // 从后继和前趋关系中移除
        for succs in self.succ_set.values_mut() {
            succs.remove(block);
        }
        for preds in self.pre_set.values_mut() {
            preds.remove(block);
        }
    }
}

impl<BasicBlock> ControlFlowGraph<BasicBlock>
where
    BasicBlock:
        Clone + Eq + std::hash::Hash + GetName<crate::backend::mir::mir_context::MirContext>,
{
    pub fn print_mir_cfg(&self, mctx: &crate::backend::mir::mir_context::MirContext) {
        println!("控制流图 (CFG):");
        println!(
            "入口块: {}",
            self.entry_block.clone().unwrap().get_name(mctx)
        );
        println!(
            "出口块: {:?}",
            self.exit_blocks
                .iter()
                .map(|b| b.get_name(mctx))
                .collect::<Vec<_>>()
        );
        println!(
            "基本块: {:?}",
            self.blocks
                .iter()
                .map(|b| b.get_name(mctx))
                .collect::<Vec<_>>()
        );
        println!("\n基本块关系:");

        for (block, successors) in &self.succ_set {
            println!(
                "  {} -> {:?}",
                block.get_name(mctx),
                successors
                    .iter()
                    .map(|b| b.get_name(mctx))
                    .collect::<Vec<_>>()
            );
        }
    }
}

/// 控制流分析结果，主要用于IR层记录不可达基本块
pub struct ControlFlowAnalysisResult<BasicBlock = crate::frontend::ir::basicblock::BasicBlock> {
    pub cfg: ControlFlowGraph<BasicBlock>,
    pub reachable_blocks: HashSet<BasicBlock>, // 可达基本块
    pub unreachable_blocks: HashSet<BasicBlock>, // 不可达基本块
}

impl<BasicBlock> ControlFlowAnalysisResult<BasicBlock>
where
    BasicBlock: Clone + Eq + std::hash::Hash,
{
    pub fn remove_bbk(&mut self, block: &BasicBlock) {
        self.cfg.remove_bbk(block);
        self.reachable_blocks.remove(block);
        self.unreachable_blocks.remove(block);
    }
}

// 单个循环
#[derive(Clone)]
pub struct Loop<BasicBlock = crate::frontend::ir::basicblock::BasicBlock> {
    pub id: u32,                                   // 循环ID
    pub preheader: Option<BasicBlock>,             // 循环前置基本块
    pub header: BasicBlock,                        // 循环头基本块
    pub parent: Option<u32>,                       // 父循环
    pub children: Vec<u32>,                        // 子循环
    pub body: HashSet<BasicBlock>,                 // 循环体包含的基本块
    pub back_edges: Vec<(BasicBlock, BasicBlock)>, // 回边 (来源 -> 目标)
    pub depth: u32,                                // 嵌套深度
}

/// 函数内所有循环信息
#[derive(Clone)]
pub struct LoopInfo<BasicBlock = crate::frontend::ir::basicblock::BasicBlock> {
    pub loops: Vec<Loop<BasicBlock>>,            // 识别出的循环
    pub id2loop: HashMap<u32, Loop<BasicBlock>>, // 循环ID -> 循环信息
    pub roots: Vec<u32>,                         // 根循环ID
}

impl<BasicBlock> LoopInfo<BasicBlock>
where
    BasicBlock: Clone + Eq + std::hash::Hash,
{
    pub fn is_in_loop(&self, block: &BasicBlock) -> bool {
        for lop in &self.loops {
            if lop.body.contains(block) {
                return true;
            }
        }
        false
    }
}

/// 支配关系信息
pub struct DominatorInfo<
    BasicBlock = crate::frontend::ir::basicblock::BasicBlock,
    Context = crate::frontend::ir::context::Context,
> {
    pub entry: Option<BasicBlock>,                            // 入口节点
    pub dominators: HashMap<BasicBlock, HashSet<BasicBlock>>, // 支配关系
    pub idominators: HashMap<BasicBlock, BasicBlock>,         // 直接必经节点
    pub dominance_frontier: HashMap<BasicBlock, HashSet<BasicBlock>>,
    pub(crate) phantom: PhantomData<Context>,
}

pub trait GetName<Context = crate::frontend::ir::context::Context> {
    fn get_name(&self, ctx: &Context) -> String;
}

impl<BasicBlock, Context> DominatorInfo<BasicBlock, Context>
where
    BasicBlock: Clone + Eq + std::hash::Hash + GetName<Context> + Copy,
{
    pub fn display_idominators(&self, ctx: &Context) {
        println!("直接必经节点:");
        for (block, idom) in &self.idominators {
            println!("  {:} -> {:}", block.get_name(ctx), idom.get_name(ctx));
        }
    }
    /// 输出必经节点树
    pub fn print_dominator_tree(&self, ctx: &Context) {
        println!("必经节点树:");
        println!("入口块: {:}", self.entry.clone().unwrap().get_name(ctx));

        // 创建支配树结构：父节点 -> 子节点集合
        let mut tree: HashMap<BasicBlock, HashSet<BasicBlock>> = HashMap::default();
        for (&child, &parent) in &self.idominators {
            tree.entry(parent).or_default().insert(child);
        }
        for (parent, children) in &tree {
            println!(
                "  {} -> {:?}",
                parent.get_name(ctx),
                children
                    .iter()
                    .map(|&b| b.get_name(ctx))
                    .collect::<Vec<_>>()
            );
        }
    }

    /// 输出支配边界
    pub fn print_dominance_frontier(&self, ctx: &Context) {
        println!("支配边界:");
        for (block, frontier) in &self.dominance_frontier {
            if !frontier.is_empty() {
                println!(
                    "  DF({:}) = {:?}",
                    block.get_name(ctx),
                    frontier
                        .iter()
                        .map(|&b| b.get_name(ctx))
                        .collect::<Vec<_>>()
                );
            }
        }
    }
}

/// 函数调用关系信息
pub struct CallGraph<Node = crate::frontend::ir::function::Function> {
    /// 所有函数的集合
    pub functions: HashMap<String, Node>,

    pub function_call_num: HashMap<Node, u32>, // 函数调用次数

    /// 直接调用关系: caller -> [callee1, callee2, ...]
    pub direct_calls: HashMap<Node, HashSet<Node>>,

    /// 被调用关系: callee -> [caller1, caller2, ...]
    pub reverse_calls: HashMap<Node, HashSet<Node>>,

    /// 程序的入口函数 (如main)
    pub entry_points: Option<Node>,
}

impl<Function> CallGraph<Function>
where
    Function: Clone + Eq + std::hash::Hash,
{
    pub fn new() -> Self {
        Self {
            functions: HashMap::default(),
            function_call_num: HashMap::default(),
            direct_calls: HashMap::default(),
            reverse_calls: HashMap::default(),
            entry_points: None,
        }
    }

    pub fn call_cycle(&self, func: &Function) -> bool {
        let mut visited = HashSet::default();
        let mut stack = vec![func];
        while let Some(node) = stack.pop() {
            if visited.contains(node) {
                return true;
            }
            visited.insert(node);
            if let Some(callers) = self.reverse_calls.get(node) {
                for caller in callers {
                    if caller == func {
                        return true;
                    }
                    stack.push(caller);
                }
            }
        }
        false
    }

    pub fn get_call_num(&self, func: &Function) -> u32 {
        self.function_call_num.get(func).unwrap().clone()
    }

    pub fn call_self(&self, func: &Function) -> bool {
        if let Some(callers) = self.reverse_calls.get(func) {
            for caller in callers {
                if caller == func {
                    return true;
                }
            }
        }
        false
    }

    pub fn get_post_order(&self) -> Vec<Function> {
        let mut visited = HashSet::default();
        let mut stack = Vec::new();
        let mut pre_order = Vec::new();

        // 遍历所有函数节点
        for node in self.functions.values() {
            // 如果节点未访问，则开始DFS
            if !visited.contains(node) {
                stack.push(node.clone());

                while let Some(node) = stack.pop() {
                    // 跳过已访问节点
                    if visited.contains(&node) {
                        continue;
                    }
                    // 标记为已访问
                    visited.insert(node.clone());
                    // 加入前序序列
                    pre_order.push(node.clone());

                    // 获取当前节点的子节点（被调用的函数）
                    if let Some(callees) = self.direct_calls.get(&node) {
                        for callee in callees {
                            // 只压入未访问的子节点
                            if !visited.contains(callee) {
                                stack.push(callee.clone());
                            }
                        }
                    }
                }
            }
        }

        // 反转前序序列得到后序序列
        pre_order.reverse();
        pre_order
    }
}

impl CallGraph<crate::frontend::ir::function::Function> {
    pub fn display_call_graph(&self, ctx: &crate::frontend::ir::context::Context) {
        println!("函数调用关系:");
        println!(
            "入口函数: {:}",
            self.entry_points.clone().unwrap().get_id(ctx)
        );
        println!("函数集合: {:?}", self.functions.keys().collect::<Vec<_>>());
        println!("直接调用关系:");
        for (caller, callees) in &self.direct_calls {
            println!(
                "  {:} -> {:?}",
                caller.get_id(ctx),
                callees.iter().map(|c| c.get_id(ctx)).collect::<Vec<_>>()
            );
        }
        println!("被调用关系:");
        for (callee, callers) in &self.reverse_calls {
            println!(
                "  {:} -> {:?}",
                callee.get_id(ctx),
                callers.iter().map(|c| c.get_id(ctx)).collect::<Vec<_>>()
            );
        }
    }
}

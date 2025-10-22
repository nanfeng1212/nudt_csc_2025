use std::collections::VecDeque;

use crate::{
    backend::mir::{
        mir_block::MirBlock,
        mir_context::MirContext,
        mir_function::MirFunction,
        mir_inst::MirInst,
        regs::{self, Reg},
    },
    passes::pass::structure::ControlFlowGraph,
    utils::linked_list::LinkedListContainer,
};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

/// 活跃区间结构体
#[derive(Debug, Clone, Default, Copy)]
pub struct LiveSegment {
    pub begin: usize,
    pub end: usize,
}

impl LiveSegment {
    pub fn new(begin: usize, end: usize) -> Self {
        LiveSegment { begin, end }
    }
}

#[derive(Debug, Clone, Default)]
pub struct LiveInterval {
    pub segments: Vec<LiveSegment>,
}

impl LiveInterval {
    pub fn new() -> Self {
        LiveInterval {
            segments: Vec::new(),
        }
    }

    pub fn add_segment(&mut self, segment: LiveSegment) {
        self.segments.push(segment);
    }

    pub fn optimize(&mut self) {
        if self.segments.is_empty() {
            return;
        }
        self.segments.sort_by_key(|s| s.begin);
        let mut merged = Vec::new();
        let mut current = self.segments[0];
        for segment in &self.segments[1..] {
            if segment.begin <= current.end {
                current.end = segment.end.max(current.end);
            } else {
                merged.push(current);
                current = *segment;
            }
        }
        merged.push(current);
        self.segments = merged;
    }

    pub fn intersect_with(&self, other: &LiveInterval) -> bool {
        let mut i = 0;
        let mut j = 0;
        while i < self.segments.len() && j < other.segments.len() {
            if self.segments[i].end <= other.segments[j].begin {
                i += 1;
            } else if other.segments[j].end <= self.segments[i].begin {
                j += 1;
            } else {
                return true;
            }
        }
        false
    }
}

/// 基本块的活跃信息
#[derive(Debug, Clone)]
pub struct MirBlockLiveInfo {
    pub live_in: HashMap<MirBlock, HashSet<Reg>>, // 基本块入口活跃集合
    pub live_out: HashMap<MirBlock, HashSet<Reg>>, // 基本块出口活跃集合
}

impl MirBlockLiveInfo {
    pub fn new() -> Self {
        MirBlockLiveInfo {
            live_in: HashMap::default(),
            live_out: HashMap::default(),
        }
    }

    pub fn get_live_in(&self, block: &MirBlock) -> Option<&HashSet<Reg>> {
        self.live_in.get(block)
    }

    pub fn get_live_out(&self, block: &MirBlock) -> Option<&HashSet<Reg>> {
        self.live_out.get(block)
    }

    pub fn print(&self, ctx: &MirContext) {
        for (block, live_in) in &self.live_in {
            println!("\t{:?}: live_in: {:?}", block.label(ctx), live_in);
        }
        for (block, live_out) in &self.live_out {
            println!("\t{:?}: live_out: {:?}", block.label(ctx), live_out);
        }
    }
}

/// 基本块的定义和使用信息
#[derive(Debug, Clone)]
pub struct DefUseInfo {
    pub defs: HashMap<MirBlock, HashSet<Reg>>, // 基本块定义集合
    pub uses: HashMap<MirBlock, HashSet<Reg>>, // 基本块使用集合
}

impl DefUseInfo {
    pub fn new() -> Self {
        DefUseInfo {
            defs: HashMap::default(),
            uses: HashMap::default(),
        }
    }

    pub fn get_defs(&self, block: &MirBlock) -> Option<&HashSet<Reg>> {
        self.defs.get(block)
    }

    pub fn get_uses(&self, block: &MirBlock) -> Option<&HashSet<Reg>> {
        self.uses.get(block)
    }

    pub fn print(&self, mctx: &MirContext) {
        for (block, defs) in &self.defs {
            println!("\t{:?}: defs: {:?}", block.label(mctx), defs);
        }
        for (block, uses) in &self.uses {
            println!("\t{:?}: uses: {:?}", block.label(mctx), uses);
        }
    }
}

/// 相关变量的活跃信息
#[derive(Debug, Clone)]
pub struct LiveVariablesInfo {
    pub block_live_info: MirBlockLiveInfo,        // 基本块活跃信息
    pub def_use_info: DefUseInfo,                 // 基本块定义和使用信息
    pub inst_num: HashMap<MirInst, usize>,        // 指令编号
    pub reg2interval: HashMap<Reg, LiveInterval>, // 寄存器活跃区间
}

pub fn cal_block_live_info(
    ctx: &MirContext,
    func: &MirFunction,
    cfg: &ControlFlowGraph<MirBlock>,
) -> LiveVariablesInfo {
    // 阶段1： 收集基本快的def/use信息
    let mut def_use_info = DefUseInfo::new();
    for block in func.iter(ctx) {
        let mut defs = HashSet::default();
        let mut uses = HashSet::default();
        for inst in block.iter(ctx) {
            for reg in inst.uses(ctx) {
                if !defs.contains(&reg) {
                    uses.insert(reg);
                }
            }
            for reg in inst.def(ctx) {
                defs.insert(reg);
            }
        }
        def_use_info.defs.insert(block, defs);
        def_use_info.uses.insert(block, uses);
    }

    // 阶段2： 计算活跃变量分析
    let mut live_in: HashMap<MirBlock, HashSet<Reg>> = HashMap::default();
    let mut live_out: HashMap<MirBlock, HashSet<Reg>> = HashMap::default();
    for block in func.iter(ctx) {
        live_in.insert(block, HashSet::default());
        live_out.insert(block, HashSet::default());
    }

    let mut visited = HashSet::default();
    let mut post_orders = vec![];
    postorder(ctx, cfg, &cfg.entry_block.unwrap(), &mut visited, &mut post_orders);
    let mut modified = true;
    while modified {
        modified = false;
        for block in post_orders.iter() {
            let mut new_out = HashSet::default();
            if let Some(successors) = cfg.get_successors(block) {
                for succ in successors {
                    for vreg in live_in[succ].iter() {
                        new_out.insert(*vreg);
                    }
                }
            }
            let mut new_in = HashSet::default();
            new_in.extend(def_use_info.get_uses(block).unwrap().clone());
            new_in.extend(
                new_out
                    .difference(&def_use_info.get_defs(block).unwrap())
                    .cloned(),
            );
            if new_in != live_in[block] {
                live_in.insert(*block, new_in);
                modified = true;
            }
            if new_out != live_out[&block] {
                live_out.insert(*block, new_out);
                modified = true;
            }
        }
    }

    let block_live_info = MirBlockLiveInfo { live_in, live_out };

    // 阶段3： 指令编号和活跃区间计算
    let mut inst_num = HashMap::default();
    let mut current = 6;
    for block in func.iter(ctx) {
        for inst in block.iter(ctx) {
            inst_num.insert(inst, current);
            current += 6;
        }
        current += 6;
    }

    let mut reg2interval: HashMap<Reg, LiveInterval> = HashMap::default();
    for block in func.iter(ctx) {
        if block.head(ctx).is_none() {
            continue;
        }
        let head_inst = block.head(ctx).unwrap();
        let tail_inst = block.tail(ctx).unwrap();
        let first_inst_num = inst_num[&head_inst];
        let last_inst_num = inst_num[&tail_inst];

        let mut cur_segments: HashMap<Reg, LiveSegment> = HashMap::default();

        // 处理基本块内的指令
        for inst in block.iter(ctx) {
            let inst_num_val = inst_num[&inst];
            for vreg in inst.uses(ctx) {
                if cur_segments.contains_key(&vreg) {
                    cur_segments.get_mut(&vreg).unwrap().end = inst_num_val + 1;
                } else {
                    let new_segment = LiveSegment::new(first_inst_num, inst_num_val + 1);
                    cur_segments.insert(vreg, new_segment);
                }
            }

            // 处理定义操作数
            for vreg in inst.def(ctx) {
                if inst.is_call(ctx) {
                    if inst.get_rds(ctx).contains(&vreg) {
                        if cur_segments.contains_key(&vreg) {
                            if cur_segments[&vreg].end == inst_num_val {
                                cur_segments.get_mut(&vreg).unwrap().end = inst_num_val + 1;
                            } else {
                                reg2interval
                                    .entry(vreg)
                                    .or_insert_with(LiveInterval::new)
                                    .add_segment(cur_segments[&vreg]);
                                cur_segments
                                    .insert(vreg, LiveSegment::new(inst_num_val, inst_num_val + 1));
                            }
                        } else {
                            cur_segments
                                .insert(vreg, LiveSegment::new(inst_num_val, inst_num_val + 1));
                        }
                    }
                    if !inst.is_void(ctx) {
                        if vreg == inst.get_ret_reg(ctx) {
                            if cur_segments.contains_key(&vreg) {
                                if cur_segments[&vreg].end == inst_num_val + 3 {
                                    cur_segments.get_mut(&vreg).unwrap().end = inst_num_val + 4;
                                } else {
                                    reg2interval
                                        .entry(vreg)
                                        .or_insert_with(LiveInterval::new)
                                        .add_segment(cur_segments[&vreg]);
                                    cur_segments.insert(
                                        vreg,
                                        LiveSegment::new(inst_num_val + 3, inst_num_val + 4),
                                    );
                                }
                            } else {
                                cur_segments.insert(
                                    vreg,
                                    LiveSegment::new(inst_num_val + 3, inst_num_val + 4),
                                );
                            }

                            let preg = if vreg.is_float() {
                                regs::s0().into()
                            } else if vreg.is_address() {
                                regs::x0().into()
                            } else {
                                regs::w0().into()
                            };
                            if cur_segments.contains_key(&preg) {
                                if cur_segments[&preg].end == inst_num_val + 2 {
                                    cur_segments.get_mut(&preg).unwrap().end = inst_num_val + 3;
                                } else {
                                    reg2interval
                                        .entry(preg)
                                        .or_insert_with(LiveInterval::new)
                                        .add_segment(cur_segments[&preg]);
                                    cur_segments.insert(
                                        preg,
                                        LiveSegment::new(inst_num_val + 2, inst_num_val + 3),
                                    );
                                }
                            } else {
                                cur_segments.insert(
                                    preg,
                                    LiveSegment::new(inst_num_val + 2, inst_num_val + 3),
                                );
                            }
                        }
                    }
                } else {
                    if cur_segments.contains_key(&vreg) {
                        if cur_segments[&vreg].end == inst_num_val + 1 {
                            cur_segments.get_mut(&vreg).unwrap().end = inst_num_val + 2;
                        } else {
                            reg2interval
                                .entry(vreg)
                                .or_insert_with(LiveInterval::new)
                                .add_segment(cur_segments[&vreg]);
                            cur_segments
                                .insert(vreg, LiveSegment::new(inst_num_val + 1, inst_num_val + 2));
                        }
                    } else {
                        cur_segments
                            .insert(vreg, LiveSegment::new(inst_num_val + 1, inst_num_val + 2));
                    }
                }
            }
        }

        // 处理块末尾的活跃区间
        for (vreg, segment) in cur_segments.iter_mut() {
            // 如果变量在live_out中，则延长到块结束
            if block_live_info.live_out[&block].contains(&vreg) {
                segment.end = last_inst_num + 2;
            }

            let interval = reg2interval.entry(*vreg).or_insert_with(LiveInterval::new);
            interval.add_segment(*segment);
        }

        // 处理在live_out中但未在块内定义的变量
        for vreg in &block_live_info.live_out[&block] {
            if block_live_info.live_in[&block].contains(vreg) && !cur_segments.contains_key(vreg) {
                let interval = reg2interval.entry(*vreg).or_insert_with(LiveInterval::new);
                interval.add_segment(LiveSegment::new(first_inst_num, last_inst_num + 2));
            }
        }
    }

    // 阶段四： 优化活跃区间
    for interval in reg2interval.values_mut() {
        interval.optimize();
    }
    LiveVariablesInfo {
        block_live_info,
        def_use_info,
        inst_num,
        reg2interval,
    }
}

/// 调试函数
pub fn debug(func: &MirFunction, ctx: &MirContext, cfg: &ControlFlowGraph<MirBlock>) {
    let live_info = cal_block_live_info(ctx, func, cfg);

    // 打印基本块活跃信息
    println!("Block Live Information:");
    for (block, live_in) in &live_info.block_live_info.live_in {
        println!("\t{:?}: live_in: {:?}", block.label(ctx), live_in);
    }
    for (block, live_out) in &live_info.block_live_info.live_out {
        println!("\t{:?}: live_out: {:?}", block.label(ctx), live_out);
    }

    // 打印寄存器活跃区间
    println!("\nRegister Live Intervals:");
    for (reg, interval) in &live_info.reg2interval {
        for segment in &interval.segments {
            println!("\t{:?}: [{}, {})", reg, segment.begin, segment.end);
        }
    }
}

pub fn bfs_order(cfg: &ControlFlowGraph<MirBlock>) -> Vec<MirBlock> {
    let mut queue: VecDeque<MirBlock> = VecDeque::new();
    let mut result: Vec<MirBlock> = Vec::new();

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
    ctx: &MirContext,
    cfg: &ControlFlowGraph<MirBlock>,
    bbk: &MirBlock,
    visited: &mut HashSet<MirBlock>,
    postorder_list: &mut Vec<MirBlock>,
) {
    visited.insert(*bbk);
    for bbk in cfg.succ_set[bbk].iter() {
        if !visited.contains(bbk) {
            postorder(ctx, cfg, bbk, visited, postorder_list);
        }
    }
    postorder_list.push(*bbk);
}
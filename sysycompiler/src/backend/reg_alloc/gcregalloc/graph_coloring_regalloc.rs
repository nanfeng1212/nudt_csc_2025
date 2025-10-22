use crate::backend::reg_alloc::gcregalloc::interference_graph::{
    InterferenceGraph, RegNumComparator, RegWeightMap,
};
use crate::backend::reg_alloc::gcregalloc::live_interval::LiveVariablesInfo;
use crate::{
    backend::{
        mir::{
            mir_block::MirBlock,
            mir_context::MirContext,
            mir_function::MirFunction,
            mir_inst::MirInst,
            mir_operand::MemLoc,
            regs::{self, PReg, Reg, RegKind, VReg},
        },
        reg_alloc::gcregalloc::live_interval::{LiveInterval, cal_block_live_info},
    },
    passes::{pass::structure::ControlFlowGraph, pass_context::PassContext},
    utils::linked_list::{LinkedListContainer, LinkedListNode},
};
use core::f64;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::{array, collections::BinaryHeap};

pub fn graph_coloring_regalloc(ctx: &mut MirContext, pctx: &PassContext) {
    let funcs: Vec<_> = ctx.get_functions().collect();
    for func in funcs {
        let func_id: String = func.get_id(&ctx);
        let mut graph_allocter = GraphColoringAllocator::new();
        graph_allocter.regalloc(ctx, &func, &pctx.mcfginfo[&func_id]);
    }
}

#[derive(Debug, Clone)]
pub struct GraphColoringAllocator {
    pub total_spill_stack: i32,
    pub spilled_regs: HashSet<Reg>,
}

impl GraphColoringAllocator {
    pub fn new() -> Self {
        Self {
            total_spill_stack: 0,
            spilled_regs: HashSet::default(),
        }
    }
    pub fn build_graph(
        &mut self,
        live_info: HashMap<Reg, LiveInterval>,
        reg_class: RegKind,
    ) -> InterferenceGraph {
        let mut graph = InterferenceGraph::new(reg_class);
        let intervals = match reg_class {
            RegKind::Integer | RegKind::Address => live_info
                .iter()
                .filter(|(reg, _)| reg.kind() == RegKind::Integer || reg.kind() == RegKind::Address)
                .collect::<Vec<_>>(),
            RegKind::Float => live_info
                .iter()
                .filter(|(reg, _)| reg.kind() == RegKind::Float)
                .collect::<Vec<_>>(),
        };

        for i in 0..intervals.len() {
            let (regi, intervali) = intervals[i];
            graph.madj.entry(*regi).or_default();
            for (regj, intervalj) in intervals.iter().skip(i + 1) {
                if intervali.intersect_with(intervalj) {
                    graph.add_edge(*regi, **regj);
                }
            }
        }
        graph
    }
    
    // 溢出最大度数优先
    pub fn compute_reg_weights(&self, graph: &InterferenceGraph) -> RegWeightMap {
        let mut weight_map = RegWeightMap::new();
        for (reg, neighbors) in graph.madj.iter() {
            if self.spilled_regs.contains(reg) {
                weight_map.insert(*reg, f64::INFINITY);
            } else {
                let degree = neighbors.len() + 1;
                weight_map.insert(*reg, 1.0 / (degree as f64));
            }
        }

        weight_map
    }

    pub fn analyze_and_protect_caller_regs(
        &mut self,
        ctx: &mut MirContext,
        func: &MirFunction,
        _reg_map: &HashMap<VReg, PReg>,
        live_info: &LiveVariablesInfo,
    ) {
        // 定义调用者保存寄存器列表
        let caller_saved_regs = vec![
            regs::x0().into(),
            regs::x1().into(),
            regs::x2().into(),
            regs::x3().into(),
            regs::x4().into(),
            regs::x5().into(),
            regs::x6().into(),
            regs::x7().into(),
            regs::x9().into(),
            regs::x10().into(),
            regs::x11().into(),
            regs::x12().into(),
            regs::x13().into(),
            regs::x14().into(),
            regs::x15().into(),
            regs::x16().into(),
            regs::x17().into(),
            regs::s0().into(),
            regs::s1().into(),
            regs::s2().into(),
            regs::s3().into(),
            regs::s4().into(),
            regs::s5().into(),
            regs::s6().into(),
            regs::s7().into(),
            regs::s16().into(),
            regs::s17().into(),
            regs::s18().into(),
            regs::s19().into(),
            regs::s20().into(),
            regs::s21().into(),
            regs::s22().into(),
            regs::s23().into(),
            regs::s24().into(),
            regs::s25().into(),
            regs::s26().into(),
            regs::s27().into(),
            regs::s28().into(),
            regs::s29().into(),
            regs::s30().into(),
            regs::s31().into(),
        ];

        let mut caller_stack = 0;
        let mut cur_block = func.head(ctx);
        while let Some(block) = cur_block {
            let mut cur_inst = block.head(ctx);
            while let Some(inst) = cur_inst {
                let next_inst = inst.succ(ctx);
                // 处理调用指令
                if inst.is_call(ctx) {
                    let mut curr_caller_stack = 0;
                    // 只保护跨call指令的
                    let call_inst_num = live_info.inst_num[&inst];
                    let mut regs_to_protect = HashSet::default();
                    for (reg, interval) in &live_info.reg2interval {
                        let active_at_all = interval
                            .segments
                            .iter()
                            .any(|seg| seg.begin <= call_inst_num && call_inst_num < seg.end - 1);
                        if active_at_all {
                            let save_reg = if reg.is_float() {
                                curr_caller_stack += 4;
                                *reg
                            } else {
                                curr_caller_stack += 8;
                                Reg::P(regs::x_reg(reg.preg_num()))
                            };
                            if caller_saved_regs.contains(&save_reg) {
                                regs_to_protect.insert(save_reg);
                            }
                        }
                    }

                    if regs_to_protect.len() > 0 {
                        generate_caller_saved(ctx, *func, &inst, &regs_to_protect);
                    }

                    // // 保存寄存器
                    // let mut e = func.storage_stack_size(ctx) + func.calleeargs_stack_size(ctx);
                    // for preg in regs_to_protect.iter() {
                    //     let unit = match preg.kind() {
                    //         regs::RegKind::Float => 4,
                    //         _ => 8,
                    //     };
                    //     let mem_loc = MemLoc::RegOffset {
                    //         base: regs::sp().into(),
                    //         offset: e,
                    //     };
                    //     generate_caller_store(ctx, *preg, mem_loc, &inst);
                    //     e += unit;
                    // }
                    // // 调用后恢复
                    // let mut e = func.storage_stack_size(ctx) + func.calleeargs_stack_size(ctx);
                    // for preg in regs_to_protect.iter() {
                    //     let unit = match preg.kind() {
                    //         regs::RegKind::Float => 4,
                    //         _ => 8,
                    //     };
                    //     let mem_loc = MemLoc::RegOffset {
                    //         base: regs::sp().into(),
                    //         offset: e,
                    //     };
                    //     generate_caller_load(ctx, *preg, mem_loc, &inst);
                    //     e += unit;
                    // }

                    if curr_caller_stack > caller_stack {
                        caller_stack = curr_caller_stack;
                    }
                }
                cur_inst = next_inst;
            }
            cur_block = block.succ(ctx);
        }
        func.add_storage_stack_size(ctx, caller_stack);
    }

    pub fn regalloc(
        &mut self,
        ctx: &mut MirContext,
        func: &MirFunction,
        cfg: &ControlFlowGraph<MirBlock>,
    ) {
        let mut reg_map = HashMap::default();
        self.spilled_regs = HashSet::default();

        // 根据寄存器类型处理
        for group in &[RegKind::Integer, RegKind::Float] {
            let mut current_result: HashMap<VReg, PReg>;
            loop {
                let mut spill_map = HashMap::default();
                current_result = HashMap::default();
                let mut stack = Vec::new();
                let mut spills = HashSet::default();

                // 可用寄存器数目
                let k = match group {
                    RegKind::Integer | RegKind::Address => 27,
                    RegKind::Float => 32,
                };
                // 计算活跃变量信息
                let live_variables_info = cal_block_live_info(ctx, func, cfg);
                let live_info = live_variables_info.reg2interval;
                // 建立冲突图
                let graph = self.build_graph(live_info, *group);

                // 计算溢出代价
                let weights = self.compute_reg_weights(&graph).weights;

                // 冲突图化简
                let mut s_graph = graph.clone();
                let mut s_queue = BinaryHeap::new();
                for (reg, _) in s_graph.madj.iter() {
                    if s_graph.degree(*reg) < k && reg.is_vreg() {
                        let weight = weights.get(reg).unwrap();
                        s_queue.push(RegNumComparator(*weight, *reg));
                    }
                }
                while !s_graph.is_empty() {
                    if let Some(reg) = s_queue.pop().map(|x| x.1) {
                        stack.push(reg);
                        for neighbor in s_graph.adj(reg).unwrap_or(&HashSet::default()) {
                            if s_graph.degree(*neighbor) == k && neighbor.is_vreg() {
                                let weight = weights.get(neighbor).unwrap();
                                s_queue.push(RegNumComparator(*weight, *neighbor));
                            }
                        }
                        s_graph.delete(reg);
                    } else {
                        let mut min_weight = f64::INFINITY;
                        let mut current_spill = None;
                        for (reg, _) in s_graph.madj.iter() {
                            if !self.spilled_regs.contains(reg) && reg.is_vreg() {
                                let weight = weights.get(reg).unwrap();
                                if *weight < min_weight {
                                    min_weight = *weight;
                                    current_spill = Some(*reg);
                                }
                            }
                        }
                        if let Some(reg) = current_spill {
                            stack.push(reg);
                            for neighbor in s_graph.adj(reg).unwrap_or(&HashSet::default()) {
                                if s_graph.degree(*neighbor) == k && neighbor.is_vreg() {
                                    let weight = weights.get(neighbor).unwrap();
                                    s_queue.push(RegNumComparator(*weight, *neighbor));
                                }
                            }
                            s_graph.delete(reg);
                        } else {
                            println!("Interference graph state before panic:");
                            for (reg, _) in s_graph.madj.iter() {
                                println!(
                                    "{:?} -> {:?}",
                                    reg,
                                    graph.adj(*reg).unwrap_or(&HashSet::default())
                                );
                                println!(
                                    "{:?} -> {:?}",
                                    reg,
                                    s_graph.adj(*reg).unwrap_or(&HashSet::default())
                                );
                            }
                            println!("Spilled registers: {:?}", self.spilled_regs);
                            panic!("No available register to spill");
                        }
                    }
                }

                while let Some(reg) = stack.pop() {
                    let mut available_colors: [bool; 34] = match group {
                        RegKind::Integer | RegKind::Address => array::from_fn(|i| {
                            (0..=7).contains(&i) || (9..=17).contains(&i) || (19..=28).contains(&i)
                        }),
                        RegKind::Float => {
                            array::from_fn(|i| (0..=15).contains(&i) || (16..=31).contains(&i))
                        }
                    };
                    for neighbor in graph.adj(reg).unwrap_or(&HashSet::default()) {
                        match neighbor {
                            Reg::P(preg) => {
                                let color = preg.num();
                                available_colors[color as usize] = false;
                            }
                            Reg::V(vreg) => {
                                if let Some(preg) = current_result.get(vreg) {
                                    let color = preg.num();
                                    available_colors[color as usize] = false;
                                }
                            }
                        }
                    }
                    let color = if let RegKind::Integer | RegKind::Address = reg.kind() {
                        (0..=7)
                            .find(|&i| available_colors[i as usize])
                            .or_else(|| (9..=17).find(|&i| available_colors[i as usize]))
                            .or_else(|| (19..=28).find(|&i| available_colors[i as usize]))
                    } else {
                        (0..=7)
                            .find(|&i| available_colors[i as usize])
                            .or_else(|| (16..=31).find(|&i| available_colors[i as usize]))
                            .or_else(|| (8..=15).find(|&i| available_colors[i as usize]))
                    };
                    if let Some(color) = color {
                        let preg = match reg.kind() {
                            RegKind::Integer => regs::w_reg(color as u8),
                            RegKind::Address => regs::x_reg(color as u8),
                            RegKind::Float => regs::s_reg(color as u8),
                        };
                        if let Reg::V(vreg) = reg {
                            current_result.insert(vreg, preg);
                        }
                    } else {
                        spills.insert(reg);
                    }
                }

                if spills.is_empty() {
                    break;
                }

                for reg in spills {
                    if spill_map.contains_key(&reg) {
                        continue;
                    }
                    let slot = func.storage_stack_size(ctx) + func.calleeargs_stack_size(ctx);
                    let spill_size = match reg.kind() {
                        regs::RegKind::Integer => 4,
                        regs::RegKind::Address => 8,
                        regs::RegKind::Float => 4,
                    };
                    self.total_spill_stack += spill_size;
                    func.add_storage_stack_size(ctx, spill_size);
                    spill_map.insert(reg, slot);
                    self.spilled_regs.insert(reg);
                }

                let mut cur_block = func.head(ctx);
                while let Some(block) = cur_block {
                    let mut cur_inst = block.head(ctx);
                    while let Some(inst) = cur_inst {
                        let next_inst = inst.succ(ctx);
                        for reg in inst.uses(ctx) {
                            if let Some(spill_slot) = spill_map.get(&reg) {
                                let spill_offset = *spill_slot;
                                let mem_loc = MemLoc::RegOffset {
                                    base: regs::sp().into(),
                                    offset: spill_offset,
                                };
                                generate_spill_load(ctx, reg, mem_loc, &inst);
                            }
                        }
                        for reg in inst.def(ctx) {
                            if let Some(spill_slot) = spill_map.get(&reg) {
                                let spill_offset = *spill_slot;
                                let mem_loc = MemLoc::RegOffset {
                                    base: regs::sp().into(),
                                    offset: spill_offset,
                                };
                                generate_spill_store(ctx, reg, mem_loc, &inst);
                            }
                        }
                        cur_inst = next_inst;
                    }
                    cur_block = block.succ(ctx);
                }
            }
            reg_map.extend(current_result);
        }

        // 记录callee_saved寄存器
        func.add_callee_reg(ctx, regs::fp().into());
        func.add_callee_reg(ctx, regs::x30().into());
        for preg in reg_map.values() {
            if regs::CALLEE_SAVED_REGS.contains(&preg) {
                if preg.kind() == RegKind::Integer || preg.kind() == RegKind::Address {
                    let num = preg.num();
                    let r_reg = regs::x_reg(num).into();
                    func.add_callee_reg(ctx, r_reg);
                } else {
                    func.add_callee_reg(ctx, *preg);
                }
            }
        }

        // 替换虚拟寄存器
        let mut cur_block = func.head(ctx);
        while let Some(block) = cur_block {
            let mut cur_inst = block.head(ctx);
            while let Some(inst) = cur_inst {
                let next_inst = inst.succ(ctx);
                for reg in inst.all_regs(ctx) {
                    if let Reg::V(vreg) = reg {
                        if let Some(allocated_reg) = reg_map.get(&vreg) {
                            //println!("{:?}",crate::backend::asm2string::Display::display(inst, &ctx));
                            inst.replace_regs(ctx, Reg::V(vreg), (*allocated_reg).into());
                            //println!("{:?}", crate::backend::asm2string::Display::display(inst, &ctx));
                        }
                    }
                }
                cur_inst = next_inst;
            }
            cur_block = block.succ(ctx);
        }

        // 保护跨越call函数调用的caller_saved寄存器
        let live_info = cal_block_live_info(ctx, func, cfg);
        self.analyze_and_protect_caller_regs(ctx, func, &reg_map, &live_info);

        // 生成传参数指令
        let mut cur_block = func.head(ctx);
        while let Some(block) = cur_block {
            let mut cur_inst = block.head(ctx);
            while let Some(inst) = cur_inst {
                let next_inst = inst.succ(ctx);
                if inst.is_call(ctx) {
                    generate_args_code(ctx, &inst);
                }
                cur_inst = next_inst;
            }
            cur_block = block.succ(ctx);
        }

        // 生成函数序言和尾声
        generate_prologue(ctx, *func);
        generate_epilogue(ctx, *func);
        //println!("{:?},spills:{}", func.display(ctx), self.total_spill_stack/4);
    }
}

pub fn generate_spill_load(mctx: &mut MirContext, reg: Reg, mem_loc: MemLoc, inst: &MirInst) {
    match mem_loc {
        MemLoc::RegOffset { base, offset } => {
            if offset > 255 || offset < -256 {
                let low = offset & 0xFFFF;
                let high = (offset >> 16) & 0xFFFF;
                let movz = MirInst::movz(mctx, regs::x8().into(), low);
                let _ = inst.insert_before(mctx, movz);
                if high != 0 {
                    let movk = MirInst::movk(mctx, regs::x8().into(), high);
                    let _ = inst.insert_before(mctx, movk);
                }
                let ldr = MirInst::ldr(mctx, reg.into(), MemLoc::Reg2Offset { base: base, offset: regs::x8().into() });
                let _ = inst.insert_before(mctx, ldr);
            } else {
                let load = MirInst::ldr(mctx, reg.into(), mem_loc);
                let _ = inst.insert_before(mctx, load);
            }
        },
        _ => {
            let load = MirInst::ldr(mctx, reg.into(), mem_loc);
            let _ = inst.insert_before(mctx, load);
        }
    }
}

pub fn generate_spill_store(mctx: &mut MirContext, reg: Reg, mem_loc: MemLoc, inst: &MirInst) {
    match mem_loc {
        MemLoc::RegOffset { base, offset } => {
            if offset > 255 || offset < -256{
                let low = offset & 0xFFFF;
                let high = (offset >> 16) & 0xFFFF;
                let movz = MirInst::movz(mctx, regs::x8().into(), low);                
                let str = MirInst::str(mctx, reg.into(), MemLoc::Reg2Offset { base: base, offset: regs::x8().into() });
                let _ = inst.insert_after(mctx, str);
                if high != 0 {
                    let movk = MirInst::movk(mctx, regs::x8().into(), high);
                    let _ = inst.insert_after(mctx, movk);
                }
                let _ = inst.insert_after(mctx, movz);
            } else {
                let str = MirInst::str(mctx, reg.into(), mem_loc);
                let _ = inst.insert_after(mctx, str);
            }
        },
        _ => {
            let str = MirInst::str(mctx, reg.into(), mem_loc);
            let _ = inst.insert_after(mctx, str);
        }
    }
}

pub fn generate_caller_store(mctx: &mut MirContext, reg: Reg, mem_loc: MemLoc, inst: &MirInst) {
    match mem_loc {
        MemLoc::RegOffset { base, offset } => {
            if offset > 255 || offset < -256 {
                let low = offset & 0xFFFF;
                let high = (offset >> 16) & 0xFFFF;
                let movz = MirInst::movz(mctx, regs::x8().into(), low);    
                let _ = inst.insert_before(mctx, movz);            
                if high != 0 {
                    let movk = MirInst::movk(mctx, regs::x8().into(), high);
                    let _ = inst.insert_before(mctx, movk);
                }
                let str = MirInst::str(mctx, reg.into(), MemLoc::Reg2Offset { base: base, offset: regs::x8().into() });
                let _ = inst.insert_before(mctx, str);
            } else {
                let str = MirInst::str(mctx, reg.into(), mem_loc);
                let _ = inst.insert_before(mctx, str);
            }
        },
        _ => {
            let str = MirInst::str(mctx, reg.into(), mem_loc);
            let _ = inst.insert_before(mctx, str);
        }
    }
}

pub fn generate_caller_load(mctx: &mut MirContext, reg: Reg, mem_loc: MemLoc, inst: &MirInst) {
    match mem_loc {
        MemLoc::RegOffset { base, offset } => {
            if offset > 255 || offset < -256 {
                let low = offset & 0xFFFF;
                let high = (offset >> 16) & 0xFFFF;
                let movz = MirInst::movz(mctx, regs::x8().into(), low);
                let ldr = MirInst::ldr(mctx, reg.into(), MemLoc::Reg2Offset { base: base, offset: regs::x8().into() });
                let _ = inst.insert_after(mctx, ldr);
                if high != 0 {
                    let movk = MirInst::movk(mctx, regs::x8().into(), high);
                    let _ = inst.insert_after(mctx, movk);
                }
                let _ = inst.insert_after(mctx, movz);
            } else {
                let load = MirInst::ldr(mctx, reg.into(), mem_loc);
                let _ = inst.insert_after(mctx, load);
            }
        },
        _ => {
            let load = MirInst::ldr(mctx, reg.into(), mem_loc);
            let _ = inst.insert_after(mctx, load);
        }
    }
}

pub fn generate_caller_saved(mctx: &mut MirContext, function: MirFunction, inst: &MirInst, regs_to_save: &HashSet<Reg>) {
    let mut int_regs = Vec::new();
    let mut float_regs = Vec::new();
    for reg in regs_to_save {
        if reg.is_float() {
            float_regs.push(regs::s_reg(reg.preg_num()))
        }else {
            int_regs.push(regs::x_reg(reg.preg_num()))
        }
    }
    int_regs.sort();
    float_regs.sort();

    // 调用前保护
    let mut e = 0 - function.callee_stack_size(mctx);
    let mut int_chunks = int_regs.chunks_exact(2);
    for chunk in int_chunks.by_ref() {
        if let [r1, r2] = chunk {
            e -= 16;
            if e >= -512 {
                let stp = MirInst::stp(mctx, (*r1).into(), (*r2).into(), MemLoc::RegOffset { base: regs::fp().into(), offset: e, });
                let _ = inst.insert_before(mctx, stp);
            }else {
                let low = e & 0xffff;
                let high = (e >> 16) & 0xffff;
                let movz = MirInst::movz(mctx, regs::x8().into(), low);
                let _ = inst.insert_before(mctx, movz);
                if high != 0 {
                    let movk = MirInst::movk(mctx, regs::x8().into(), high);
                    let _ = inst.insert_before(mctx, movk);
                }
                let stp = MirInst::stp(mctx,(*r1).into(),(*r2).into(),MemLoc::Reg2Offset {base: regs::fp().into(),offset: regs::x8().into()},);
                let _ = inst.insert_before(mctx, stp);
            }
        }
    }
    if let [reg] = int_chunks.remainder() {
        e -= 8;
        if e >= -256 {
            let str = MirInst::str(mctx,(*reg).into(),MemLoc::RegOffset {base: regs::fp().into(),offset: e,},);
            let _ = inst.insert_before(mctx, str);
        } else {
            let low = e & 0xffff;
            let high = (e >> 16) & 0xffff;
            let movz = MirInst::movz(mctx, regs::x8().into(), low);
            let _ = inst.insert_before(mctx, movz);
            if high != 0 {
                let movk = MirInst::movk(mctx, regs::x8().into(), high);
                let _ = inst.insert_before(mctx, movk);
            }
            let str = MirInst::str(mctx,(*reg).into(),MemLoc::Reg2Offset {base: regs::fp().into(),offset: regs::x8().into()},);
            let _ = inst.insert_before(mctx, str);
        }
    }

    let mut float_chunks = float_regs.chunks_exact(2);
    for chunk in float_chunks.by_ref() {
        if let [r1, r2] = chunk {
            e -= 8;
            if e >= -256 {
                let stp = MirInst::stp(mctx, (*r1).into(), (*r2).into(), MemLoc::RegOffset { base: regs::fp().into(), offset: e, });
                let _ = inst.insert_before(mctx, stp);
            }else {
                let low = e & 0xffff;
                let high = (e >> 16) & 0xffff;
                let movz = MirInst::movz(mctx, regs::x8().into(), low);
                let _ = inst.insert_before(mctx, movz);
                if high != 0 {
                    let movk = MirInst::movk(mctx, regs::x8().into(), high);
                    let _ = inst.insert_before(mctx, movk);
                }
                let stp = MirInst::stp(mctx,(*r1).into(),(*r2).into(),MemLoc::Reg2Offset {base: regs::fp().into(),offset: regs::x8().into()},);
                let _ = inst.insert_before(mctx, stp);
            }
        }
    }
    if let [reg] = float_chunks.remainder() {
        e -= 4;
        if e >= -256 {
            let str = MirInst::str(mctx,(*reg).into(),MemLoc::RegOffset {base: regs::fp().into(),offset: e,},);
            let _ = inst.insert_before(mctx, str);
        } else {
            let low = e & 0xffff;
            let high = (e >> 16) & 0xffff;
            let movz = MirInst::movz(mctx, regs::x8().into(), low);
            let _ = inst.insert_before(mctx, movz);
            if high != 0 {
                let movk = MirInst::movk(mctx, regs::x8().into(), high);
                let _ = inst.insert_before(mctx, movk);
            }
            let str = MirInst::str(mctx,(*reg).into(),MemLoc::Reg2Offset {base: regs::fp().into(),offset: regs::x8().into()},);
            let _ = inst.insert_before(mctx, str);
        }
    }

    // 调用后保护
    let mut e = 0 - function.callee_stack_size(mctx);
    let mut int_chunks = int_regs.chunks_exact(2);
    for chunk in int_chunks.by_ref() {
        if let [r1, r2] = chunk {
            e -= 16;
            if e >= -512 {
                let ldp = MirInst::ldp(mctx, (*r1).into(), (*r2).into(), MemLoc::RegOffset { base: regs::fp().into(), offset: e, });
                let _ = inst.insert_after(mctx, ldp);
            }else {
                let low = e & 0xffff;
                let high = (e >> 16) & 0xffff;
                let movz = MirInst::movz(mctx, regs::x8().into(), low);
                let _ = inst.insert_after(mctx, movz);
                if high != 0 {
                    let movk = MirInst::movk(mctx, regs::x8().into(), high);
                    let _ = inst.insert_after(mctx, movk);
                }
                let ldp = MirInst::ldp(mctx,(*r1).into(),(*r2).into(),MemLoc::Reg2Offset {base: regs::fp().into(),offset: regs::x8().into()},);
                let _ = inst.insert_after(mctx, ldp);
            }
        }
    }
    if let [reg] = int_chunks.remainder() {
        e -= 8;
        if e >= -256 {
            let ldr = MirInst::ldr(mctx,(*reg).into(),MemLoc::RegOffset {base: regs::fp().into(),offset: e,},);
            let _ = inst.insert_after(mctx, ldr);
        } else {
            let low = e & 0xffff;
            let high = (e >> 16) & 0xffff;
            let movz = MirInst::movz(mctx, regs::x8().into(), low);
            let _ = inst.insert_after(mctx, movz);
            if high != 0 {
                let movk = MirInst::movk(mctx, regs::x8().into(), high);
                let _ = inst.insert_after(mctx, movk);
            }
            let ldr = MirInst::ldr(mctx,(*reg).into(),MemLoc::Reg2Offset {base: regs::fp().into(),offset: regs::x8().into()},);
            let _ = inst.insert_after(mctx, ldr);
        }
    }

    let mut float_chunks = float_regs.chunks_exact(2);
    for chunk in float_chunks.by_ref() {
        if let [r1, r2] = chunk {
            e -= 8;
            if e >= -256 {
                let ldp = MirInst::ldp(mctx, (*r1).into(), (*r2).into(), MemLoc::RegOffset { base: regs::fp().into(), offset: e, });
                let _ = inst.insert_after(mctx, ldp);
            }else {
                let low = e & 0xffff;
                let high = (e >> 16) & 0xffff;
                let movz = MirInst::movz(mctx, regs::x8().into(), low);
                let _ = inst.insert_after(mctx, movz);
                if high != 0 {
                    let movk = MirInst::movk(mctx, regs::x8().into(), high);
                    let _ = inst.insert_after(mctx, movk);
                }
                let ldp = MirInst::ldp(mctx,(*r1).into(),(*r2).into(),MemLoc::Reg2Offset {base: regs::fp().into(),offset: regs::x8().into()},);
                let _ = inst.insert_after(mctx, ldp);
            }
        }
    }
    if let [reg] = float_chunks.remainder() {
        e -= 4;
        if e >= -256 {
            let ldr = MirInst::ldr(mctx,(*reg).into(),MemLoc::RegOffset {base: regs::fp().into(),offset: e,},);
            let _ = inst.insert_after(mctx, ldr);
        } else {
            let low = e & 0xffff;
            let high = (e >> 16) & 0xffff;
            let movz = MirInst::movz(mctx, regs::x8().into(), low);
            let _ = inst.insert_after(mctx, movz);
            if high != 0 {
                let movk = MirInst::movk(mctx, regs::x8().into(), high);
                let _ = inst.insert_after(mctx, movk);
            }
            let ldr = MirInst::ldr(mctx,(*reg).into(),MemLoc::Reg2Offset {base: regs::fp().into(),offset: regs::x8().into()},);
            let _ = inst.insert_after(mctx, ldr);
        }
    }

}

pub fn generate_args_code(mctx: &mut MirContext, inst: &MirInst) {
    let is_void = inst.is_void(mctx);
    let mut args = inst.get_args(mctx);
    if !is_void {
        let rd = args.remove(0);
        if rd.is_float() {
            let fmov = MirInst::fmov(mctx, rd, regs::s0().into());
            let _ = inst.insert_after(mctx, fmov);
        } else if rd.is_address() {
            let mov = MirInst::mov_reg(mctx, rd, regs::x0().into());
            let _ = inst.insert_after(mctx, mov);
        } else {
            let mov = MirInst::mov_reg(mctx, rd, regs::w0().into());
            let _ = inst.insert_after(mctx, mov);
        }
    }
    for (i, reg) in args.iter().enumerate() {
        let preg = match i {
            0 => {
                if reg.is_float() {
                    regs::s0().into()
                } else if reg.is_address() {
                    regs::x0().into()
                } else {
                    regs::w0().into()
                }
            }
            1 => {
                if reg.is_float() {
                    regs::s1().into()
                } else if reg.is_address() {
                    regs::x1().into()
                } else {
                    regs::w1().into()
                }
            }
            2 => {
                if reg.is_float() {
                    regs::s2().into()
                } else if reg.is_address() {
                    regs::x2().into()
                } else {
                    regs::w2().into()
                }
            }
            3 => {
                if reg.is_float() {
                    regs::s3().into()
                } else if reg.is_address() {
                    regs::x3().into()
                } else {
                    regs::w3().into()
                }
            }
            4 => {
                if reg.is_float() {
                    regs::s4().into()
                } else if reg.is_address() {
                    regs::x4().into()
                } else {
                    regs::w4().into()
                }
            }
            5 => {
                if reg.is_float() {
                    regs::s5().into()
                } else if reg.is_address() {
                    regs::x5().into()
                } else {
                    regs::w5().into()
                }
            }
            6 => {
                if reg.is_float() {
                    regs::s6().into()
                } else if reg.is_address() {
                    regs::x6().into()
                } else {
                    regs::w6().into()
                }
            }
            7 => {
                if reg.is_float() {
                    regs::s7().into()
                } else if reg.is_address() {
                    regs::x7().into()
                } else {
                    regs::w7().into()
                }
            }
            _ => unreachable!(),
        };
        if reg.is_float() {
            let fmov = MirInst::fmov(mctx, preg, *reg);
            let _ = inst.insert_before(mctx, fmov);
        } else {
            let mov = MirInst::mov_reg(mctx, preg, *reg);
            let _ = inst.insert_before(mctx, mov);
        }
    }
}

pub fn generate_prologue(mctx: &mut MirContext, function: MirFunction) {
    let mut stack_size = function.storage_stack_size(mctx) + function.calleeargs_stack_size(mctx) + function.callee_stack_size(mctx);
    //stack_size += (int_regs.len()+2) as i32 * 8 + float_regs.len() as i32 * 4;
    stack_size = (stack_size + 15) & !15;

    let mut int_regs = Vec::new();
    let mut float_regs = Vec::new();
    for reg in function.callee_regs(mctx).iter() {
        if *reg == regs::x30().into() || *reg == regs::fp().into() {
            continue;
        }
        if reg.is_float() {
            float_regs.push(*reg);
        } else {
            int_regs.push(*reg);
        }
    }
    int_regs.sort();
    float_regs.sort();
    
    // 存储被调用者保护寄存器
    let mut e = -16;
    let mut int_chunks = int_regs.chunks_exact(2);
    for chunk in int_chunks.by_ref() {
        if let [r1, r2] = chunk {
            e -= 16;
            if e >= -512 {
                let stp = MirInst::stp(mctx, (*r1).into(), (*r2).into(), MemLoc::RegOffset { base: regs::fp().into(), offset: e, });
                function.head(mctx).unwrap().push_front(mctx, stp).unwrap();
            }else {
                let low = e & 0xffff;
                let high = (e >> 16) & 0xffff;
                let stp = MirInst::stp(mctx,(*r1).into(),(*r2).into(),MemLoc::Reg2Offset {base: regs::fp().into(),offset: regs::x8().into()},);
                function.head(mctx).unwrap().push_front(mctx, stp).unwrap();
                if high != 0 {
                    let movk = MirInst::movk(mctx, regs::x8().into(), high);
                    function.head(mctx).unwrap().push_front(mctx, movk).unwrap();
                }
                let movz = MirInst::movz(mctx, regs::x8().into(), low);
                function.head(mctx).unwrap().push_front(mctx, movz).unwrap();
            }
        }
    }
    if let [reg] = int_chunks.remainder() {
        e -= 8;
        if e >= -256 {
            let str = MirInst::str(mctx,(*reg).into(),MemLoc::RegOffset {base: regs::fp().into(),offset: e,},);
            function.head(mctx).unwrap().push_front(mctx, str).unwrap();
        } else {
            let low = e & 0xffff;
            let high = (e >> 16) & 0xffff;
            let str = MirInst::str(mctx,(*reg).into(),MemLoc::Reg2Offset {base: regs::fp().into(),offset: regs::x8().into()},);
            function.head(mctx).unwrap().push_front(mctx, str).unwrap();
            if high != 0 {
                let movk = MirInst::movk(mctx, regs::x8().into(), high);
                function.head(mctx).unwrap().push_front(mctx, movk).unwrap();
            }
            let movz = MirInst::movz(mctx, regs::x8().into(), low);
            function.head(mctx).unwrap().push_front(mctx, movz).unwrap();
        }
    }

    let mut float_chunks = float_regs.chunks_exact(2);
    for chunk in float_chunks.by_ref() {
        if let [r1, r2] = chunk {
            e -= 8;
            if e >= -256 {
                let stp = MirInst::stp(mctx, (*r1).into(), (*r2).into(), MemLoc::RegOffset { base: regs::fp().into(), offset: e, });
                function.head(mctx).unwrap().push_front(mctx, stp).unwrap();
            }else {
                let low = e & 0xffff;
                let high = (e >> 16) & 0xffff;
                let stp = MirInst::stp(mctx,(*r1).into(),(*r2).into(),MemLoc::Reg2Offset {base: regs::fp().into(),offset: regs::x8().into()},);
                function.head(mctx).unwrap().push_front(mctx, stp).unwrap();
                if high != 0 {
                    let movk = MirInst::movk(mctx, regs::x8().into(), high);
                    function.head(mctx).unwrap().push_front(mctx, movk).unwrap();
                }
                let movz = MirInst::movz(mctx, regs::x8().into(), low);
                function.head(mctx).unwrap().push_front(mctx, movz).unwrap();
            }
        }
    }
    if let [reg] = float_chunks.remainder() {
        e -= 4;
        if e >= -256 {
            let str = MirInst::str(mctx,(*reg).into(),MemLoc::RegOffset {base: regs::fp().into(),offset: e,},);
            function.head(mctx).unwrap().push_front(mctx, str).unwrap();
        } else {
            let low = e & 0xffff;
            let high = (e >> 16) & 0xffff;
            let str = MirInst::str(mctx,(*reg).into(),MemLoc::Reg2Offset {base: regs::fp().into(),offset: regs::x8().into()},);
            function.head(mctx).unwrap().push_front(mctx, str).unwrap();
            if high != 0 {
                let movk = MirInst::movk(mctx, regs::x8().into(), high);
                function.head(mctx).unwrap().push_front(mctx, movk).unwrap();
            }
            let movz = MirInst::movz(mctx, regs::x8().into(), low);
            function.head(mctx).unwrap().push_front(mctx, movz).unwrap();
        }
    }

    if stack_size > 4095 {
        let low = stack_size & 0xffff;
        let high = (stack_size >> 16) & 0xffff;
        let temp_reg = regs::x8().into();
        let movz = MirInst::movz(mctx,  temp_reg, low);
        let sub = MirInst::sub(mctx, regs::sp().into(), regs::sp().into(), temp_reg);
        function.head(mctx).unwrap().push_front(mctx, sub).unwrap();
        if high != 0 {
            let movk = MirInst::movk(mctx, temp_reg, high);
            function.head(mctx).unwrap().push_front(mctx, movk).unwrap();
        }
        function.head(mctx).unwrap().push_front(mctx, movz).unwrap();
    } else {
        let subi = MirInst::subi(mctx, regs::sp().into(), regs::sp().into(), stack_size);
        function.head(mctx).unwrap().push_front(mctx, subi).unwrap();
    }

    let mem_loc = MemLoc::RegOffset { base: regs::sp().into(), offset: -16 };
    let mov_sp = MirInst::mov_reg(mctx, regs::fp().into(), regs::sp().into());
    function.head(mctx).unwrap().push_front(mctx, mov_sp).unwrap();

    match mem_loc {
        MemLoc::RegOffset { base, offset } => {
            if offset > 504 || offset <= -512 {
                let temp_reg = regs::x8().into();
                if offset > 4095 {
                    let low = offset & 0xffff;
                    let high = (offset >> 16) & 0xffff;
                    let movz = MirInst::movz(mctx, temp_reg, low);
                    let add = MirInst::add(mctx, temp_reg, base, temp_reg);
                    let stp = MirInst::stp(mctx, regs::fp().into(), regs::x30().into(), MemLoc::Reg{address: temp_reg});
                    let _ = function.head(mctx).unwrap().push_front(mctx, stp);
                    let _ = function.head(mctx).unwrap().push_front(mctx, add);
                    if high != 0 {
                        let movk = MirInst::movk(mctx, temp_reg, high);
                        let _ = function.head(mctx).unwrap().push_front(mctx, movk);
                    }
                    let _ = function.head(mctx).unwrap().push_front(mctx, movz);
                } else {
                    let addi = MirInst::addi(mctx, temp_reg, base, offset);
                    let stp = MirInst::stp(mctx, regs::fp().into(), regs::x30().into(), MemLoc::Reg{address: temp_reg});
                    let _ = function.head(mctx).unwrap().push_front(mctx, stp);
                    let _ = function.head(mctx).unwrap().push_front(mctx, addi);
                }
            } else {
                let stp = MirInst::stp(mctx, regs::fp().into(), regs::x30().into(), mem_loc);
                let _ = function.head(mctx).unwrap().push_front(mctx, stp);
            }
        }
        _ => todo!()
    }
}

pub fn generate_epilogue(mctx: &mut MirContext, function: MirFunction) {
    // let mut stack_size = function.storage_stack_size(mctx)
    //     + function.calleeargs_stack_size(mctx)
    //     + function.callee_regs(mctx).len() as i32 * 8;
    // stack_size = (stack_size + 15) & !15;
    let mut int_regs = Vec::new();
    let mut float_regs = Vec::new();
    for reg in function.callee_regs(mctx).iter() {
        if *reg == regs::x30().into() || *reg == regs::fp().into() {
            continue;
        }
        if reg.is_float() {
            float_regs.push(*reg);
        } else {
            int_regs.push(*reg);
        }
    }
    int_regs.sort();
    float_regs.sort();

    let mem_loc = MemLoc::RegOffset { base: regs::sp().into(), offset: -16 };
    let ret = MirInst::ret(mctx);

    // 恢复被调用者保护寄存器
    let mut e = -16;
    let mut int_chunks = int_regs.chunks_exact(2);
    for chunk in int_chunks.by_ref() {
        if let [r1, r2] = chunk {
            e -= 16;
            if e >= -512 {
                let ldp = MirInst::ldp(mctx, (*r1).into(), (*r2).into(), MemLoc::RegOffset { base: regs::fp().into(), offset: e, });
                function.tail(mctx).unwrap().push_back(mctx, ldp).unwrap();
            }else {
                let low = e & 0xffff;
                let high = (e >> 16) & 0xffff;
                let movz = MirInst::movz(mctx, regs::x8().into(), low);
                function.tail(mctx).unwrap().push_back(mctx, movz).unwrap();
                if high != 0 {
                    let movk = MirInst::movk(mctx, regs::x8().into(), high);
                    function.tail(mctx).unwrap().push_back(mctx, movk).unwrap();
                }
                let ldp = MirInst::stp(mctx,(*r1).into(),(*r2).into(),MemLoc::Reg2Offset {base: regs::fp().into(),offset: regs::x8().into()},);
                function.tail(mctx).unwrap().push_back(mctx, ldp).unwrap();
                
            }
        }
    }
    if let [reg] = int_chunks.remainder() {
        e -= 8;
        if e >= -256 {
            let ldr = MirInst::ldr(mctx,(*reg).into(),MemLoc::RegOffset {base: regs::fp().into(),offset: e,},);
            function.tail(mctx).unwrap().push_back(mctx, ldr).unwrap();
        } else {
            let low = e & 0xffff;
            let high = (e >> 16) & 0xffff;
            let movz = MirInst::movz(mctx, regs::x8().into(), low);
            function.tail(mctx).unwrap().push_back(mctx, movz).unwrap();
            if high != 0 {
                let movk = MirInst::movk(mctx, regs::x8().into(), high);
                function.tail(mctx).unwrap().push_back(mctx, movk).unwrap();
            }
            let ldr = MirInst::ldr(mctx,(*reg).into(),MemLoc::Reg2Offset { base: regs::fp().into(),offset:regs::x8().into()},);
            function.tail(mctx).unwrap().push_back(mctx, ldr).unwrap();
        }
    }

    let mut float_chunks = float_regs.chunks_exact(2);
    for chunk in float_chunks.by_ref() {
        if let [r1, r2] = chunk {
            e -= 8;
            if e >= -256 {
                let ldp = MirInst::ldp(mctx, (*r1).into(), (*r2).into(), MemLoc::RegOffset { base: regs::fp().into(), offset: e, });
                function.tail(mctx).unwrap().push_back(mctx, ldp).unwrap();
            }else {
                let low = e & 0xffff;
                let high = (e >> 16) & 0xffff;
                let movz = MirInst::movz(mctx, regs::x8().into(), low);
                function.tail(mctx).unwrap().push_back(mctx, movz).unwrap();
                if high != 0 {
                    let movk = MirInst::movk(mctx, regs::x8().into(), high);
                    function.tail(mctx).unwrap().push_back(mctx, movk).unwrap();
                }
                let ldp = MirInst::stp(mctx,(*r1).into(),(*r2).into(),MemLoc::Reg2Offset {base: regs::fp().into(),offset: regs::x8().into()},);
                function.tail(mctx).unwrap().push_back(mctx, ldp).unwrap();
                
            }
        }
    }

    if let [reg] = float_chunks.remainder() {
        e -= 4;
        if e >= -256 {
            let ldr = MirInst::ldr(mctx,(*reg).into(),MemLoc::RegOffset {base: regs::fp().into(),offset: e,},);
            function.tail(mctx).unwrap().push_back(mctx, ldr).unwrap();
        } else {
            let low = e & 0xffff;
            let high = (e >> 16) & 0xffff;
            let movz = MirInst::movz(mctx, regs::x8().into(), low);
            function.tail(mctx).unwrap().push_back(mctx, movz).unwrap();
            if high != 0 {
                let movk = MirInst::movk(mctx, regs::x8().into(), high);
                function.tail(mctx).unwrap().push_back(mctx, movk).unwrap();
            }
            let ldr = MirInst::ldr(mctx,(*reg).into(),MemLoc::Reg2Offset {base: regs::fp().into(),offset: regs::x8().into()},);
            function.tail(mctx).unwrap().push_back(mctx, ldr).unwrap();
        }
    }

    // if stack_size > 4095 {
    //     let low = stack_size & 0xffff;
    //     let high = (stack_size >> 16) & 0xffff;
    //     let temp_reg = regs::x8().into();
    //     let movz = MirInst::movz(mctx,  temp_reg, low);
    //     function.tail(mctx).unwrap().push_back(mctx, movz).unwrap();
    //     if high != 0 {
    //         let movk = MirInst::movk(mctx, temp_reg, high);
    //         function.tail(mctx).unwrap().push_back(mctx, movk).unwrap();
    //     }
    //     let add = MirInst::add(mctx, regs::sp().into(), regs::sp().into(), temp_reg);
    //     function.tail(mctx).unwrap().push_back(mctx, add).unwrap();
    // } else {
    //     let addi = MirInst::addi(mctx, regs::sp().into(), regs::sp().into(), stack_size);
    //     function.tail(mctx).unwrap().push_back(mctx, addi).unwrap();
    // }
    let mov_fp = MirInst::mov_reg(mctx, regs::sp().into(), regs::fp().into());
    function.tail(mctx).unwrap().push_back(mctx, mov_fp).unwrap();

    match mem_loc {
        MemLoc::RegOffset { base, offset } => {
            if offset > 504 || offset < -512 {
                let temp_reg = regs::x8().into();
                if offset > 4095 {
                    let low = offset & 0xffff;
                    let high = (offset >> 16) & 0xffff;
                    let movz = MirInst::movz(mctx, temp_reg, low);
                    let _ = function.tail(mctx).unwrap().push_back(mctx, movz);
                    if high != 0 {
                        let movk = MirInst::movk(mctx, temp_reg, high);
                        let _ = function.tail(mctx).unwrap().push_back(mctx, movk);
                    }
                    let add = MirInst::add(mctx, temp_reg, base, temp_reg);
                    let _ = function.tail(mctx).unwrap().push_back(mctx, add);
                    let ldp = MirInst::ldp(mctx, regs::fp().into(), regs::x30().into(), MemLoc::Reg{address: temp_reg});
                    let _ = function.tail(mctx).unwrap().push_back(mctx, ldp);
                } else {
                    let addi = MirInst::addi(mctx, temp_reg, base, offset);
                    let _ = function.tail(mctx).unwrap().push_back(mctx, addi);
                    let ldp = MirInst::ldp(mctx, regs::fp().into(), regs::x30().into(), MemLoc::Reg{address: temp_reg});
                    let _ = function.tail(mctx).unwrap().push_back(mctx, ldp);
                }
            } else {
                let ldp = MirInst::ldp(mctx, regs::fp().into(), regs::x30().into(), mem_loc);
                let _ = function.tail(mctx).unwrap().push_back(mctx, ldp);
            }
        }
        _ => todo!()
    }
    function.tail(mctx).unwrap().push_back(mctx, ret).unwrap();
}
use std::collections::BinaryHeap;

use rustc_hash::FxHashMap as HashMap;

use crate::{
    backend::{
        instrction_schedule::{
            dependence_graph::{DependenceGraph, InstComparator},
            schedule_model::{Resource, dual_emit, get_depend, get_latency, get_resource},
        },
        mir::{mir_context::MirContext, mir_inst::MirInst},
    },
    utils::linked_list::{LinkedListContainer, LinkedListNode},
};

// 执行调度
pub fn intra_block_schedule(ctx: &mut MirContext, begin: MirInst, end: MirInst) {
    let mut total_inst: usize = 0;

    // 构建依赖图
    let mut graph = DependenceGraph::new();
    let mut indexs = HashMap::default();
    let mut curr_inst = Some(begin);
    let mut degrees = HashMap::default();
    while let Some(inst) = curr_inst {
        indexs.insert(inst, total_inst);
        degrees.entry(inst).or_insert(0);
        if inst == end {
            break;
        }
        let mut next = inst.succ(ctx);
        while let Some(next_inst) = next {
            if get_depend(ctx, inst, next_inst).len() > 0 {
                graph.add_edge(inst, next_inst);
                let degree = degrees.entry(next_inst).or_insert(0);
                *degree += 1;
            }
            if next_inst == end {
                break;
            }
            next = next_inst.succ(ctx);
        }
        total_inst += 1;
        curr_inst = inst.succ(ctx);
    }
    // 构建指令遍历拓扑序
    let mut sorted = Vec::new();
    let mut queue = BinaryHeap::new();
    for (inst, degree) in degrees.iter() {
        if *degree == 0 {
            let inst_num = indexs.get(inst).unwrap();
            let node = InstComparator::new(*inst, *inst_num);
            queue.push(node);
        }
    }

    while let Some(node) = queue.pop() {
        let inst = node.inst;
        sorted.push(inst);
        for succ in graph.get_succs(&inst).iter() {
            let degree = degrees.get_mut(succ).unwrap();
            *degree -= 1;
            if *degree == 0 {
                let inst_num = indexs.get(succ).unwrap();
                let node = InstComparator::new(*succ, *inst_num);
                queue.push(node);
            }
        }
    }
    let mut result = HashMap::default();
    let mut resources = Vec::new();

    for inst in sorted {
        let mut inst_begin = 0;
        for &pre in graph.get_pres(&inst).iter() {
            let latency = if dual_emit(ctx, pre, inst) {
                0
            } else {
                get_latency(ctx, pre)
            };
            if inst_begin < result[&pre] + latency {
                inst_begin = result[&pre] + latency;
            }
        }
        let current_resources = get_resource(ctx, inst);
        loop {
            resources.resize(
                resources.len().max(inst_begin + current_resources.len()),
                Resource::init(),
            );
            let mut satisfy = true;
            for (i, &resource) in current_resources.iter().enumerate() {
                if (resources[inst_begin + i] + resource).unsatisfy(Resource::a53()) {
                    satisfy = false;
                    break;
                }
            }
            if satisfy {
                break;
            }

            inst_begin += 1;
        }

        result.insert(inst, inst_begin);

        for (i, &resource) in current_resources.iter().enumerate() {
            resources[inst_begin + i] = resources[inst_begin + i] + resource;
        }
    }

    // 指令重排
    let mut insts = Vec::new();
    let mut curr_inst = Some(begin);

    while let Some(inst) = curr_inst {
        insts.push(inst);

        if inst == end {
            break;
        }

        curr_inst = inst.succ(ctx);
    }
    insts.sort_by_key(|inst| result[inst]);
    //let i = insts[0];
    // if i.is_madd(ctx) {
    //     for j in insts.iter() {
    //         println!("{}",j.display(ctx));
    //     }
    // }
    if let Some(insertion_point) = end.succ(ctx) {
        for inst in insts {
            inst.unlink(ctx);
            let _ = insertion_point.insert_before(ctx, inst);
        }
    } else {
        let mblock = begin.container(ctx).unwrap();
        for inst in insts {
            inst.unlink(ctx);
            let _ = mblock.push_back(ctx, inst);
        }
    }
}

pub mod schedule;
pub mod dependence_graph;
pub mod schedule_model;

pub fn schedule(
    ctx: &mut crate::backend::mir::mir_context::MirContext, 
    schedule_size: usize) 
{
    let funcs = ctx.funcs.iter_mut().map(|func_data| func_data.self_ptr()).collect::<Vec<_>>();

    for func in funcs {
        if !func.is_external(ctx) {
            let blocks: Vec<_> = crate::utils::linked_list::LinkedListContainer::iter(func, ctx).collect();
            for block in blocks {
                let Some(mut segment_begin) = block.get_head(ctx) else {
                    continue;
                };
                let mut current_inst = Some(segment_begin);
                let mut inst_count = 0;
                while let Some(inst) = current_inst {
                    inst_count += 1;
                    let next_inst = crate::utils::linked_list::LinkedListNode::succ(inst, ctx);
                    let segment_boundary = inst_count >= schedule_size;
                    let block_end = next_inst.is_none();
                    if block_end || segment_boundary {
                        // 调度当前段
                        crate::backend::instrction_schedule::schedule::intra_block_schedule(ctx, segment_begin, inst);
                        // 准备下一个段
                        if let Some(next) = next_inst {
                            segment_begin = next;
                            inst_count = 0;
                        } else {
                            break;
                        }
                    }
                    current_inst = next_inst;
                }                
            }
        }
    }
}

pub fn post_schedule(
    ctx: &mut crate::backend::mir::mir_context::MirContext, 
    ) 
{
    let funcs = ctx.funcs.iter_mut().map(|func_data| func_data.self_ptr()).collect::<Vec<_>>();
    for func in funcs {
        if !func.is_external(ctx) {
            let blocks: Vec<_> = crate::utils::linked_list::LinkedListContainer::iter(func, ctx).collect();
            for block in blocks {
                if block.get_head(ctx).is_none() {
                    continue;
                }
                crate::backend::instrction_schedule::schedule::intra_block_schedule(
                    ctx, 
                    block.get_head(ctx).unwrap(), 
                    block.get_tail(ctx).unwrap()
                );
            }
        }
    }
}

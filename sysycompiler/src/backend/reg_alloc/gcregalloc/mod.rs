use crate::passes::{pass, pass_manager::Pass};
pub mod live_interval;
pub mod interference_graph;
pub mod graph_coloring_regalloc;

pub fn graph_coloring_regalloc(
    ctx: &mut crate::backend::mir::mir_context::MirContext,
    pctx: &mut crate::passes::pass_context::PassContext,
) {
    pass::analysis::mcfa::MirCFAnalysis
        .execute(ctx, pctx)
        .unwrap();
    let funcs: Vec<_> = ctx.get_functions().collect();
    for func in funcs {
        let func_id: String = func.get_id(&ctx);
        let mut graph_allocter =
            crate::backend::reg_alloc::gcregalloc::graph_coloring_regalloc::GraphColoringAllocator::new();
        graph_allocter.regalloc(ctx, &func, &pctx.mcfginfo[&func_id]);
    }
}

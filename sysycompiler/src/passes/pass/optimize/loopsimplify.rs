//! Loop Simplify Pass
//!
//! This pass transforms natural loops into a simpler form:
//! 1. Inserts preheaders for loops
//! 2. Creates dedicated exit blocks
//! 3. Ensures single backedge to loop headers

use rustc_hash::FxHashMap as HashMap;

use crate::{
    frontend::ir::{
        basicblock::BasicBlock, context::Context, function::Function, instruction::Instruction,
    },
    passes::{
        pass::{self},
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::{LinkedListContainer, LinkedListNode},
};

pub struct LoopSimplify;

impl Pass for LoopSimplify {
    type Output = bool;
    type Error = PassError;

    fn pass_type(&self) -> PassType {
        PassType::FunctionBasedOptimization
    }

    fn execute(
        &mut self,
        input: &mut Context,
        pctx: &mut PassContext,
    ) -> Result<bool, Self::Error> {
        let mut changed = false;
        let functions: Vec<_> = input.get_functions().collect();

        for function in functions {
            let res = self.execute_function(&function, input, pctx);
            // loop_info.print_loop_info(input);
            changed |= res?;
        }
        Ok(changed)
    }

    fn name(&self) -> &'static str {
        "LoopSimplify"
    }

    fn dependencies(&self) -> &[&'static str] {
        &["CFAnalysis", "LoopAnalysis", "Reg2Mem"]
    }

    fn effects(&self) -> &[&'static str] {
        &["DominatorRelAnalysis", "LoopAnalysis"]
    }
}

impl LoopSimplify {
    fn execute_function(
        &mut self,
        input: &Function,
        ctx: &mut Context,
        pctx: &mut PassContext,
    ) -> Result<bool, PassError> {
        let mut changed = false;
        let mut loop_info = pctx.loop_info.get(&input.get_id(ctx)).unwrap().clone();
        // Process loops from innermost to outermost
        loop_info.loops.sort_by(|a, b| b.depth.cmp(&a.depth));

        for loop_ in loop_info.loops.iter() {
            // 1. Insert preheader if needed
            changed |= Self::insert_preheader(input, ctx, &loop_, pctx);

            // 2. Create dedicated exit blocks
            changed |= Self::create_dedicated_exit_blocks(input, ctx, &loop_, pctx);

            // 3. Simplify backedges to single latch
            changed |= Self::simplify_backedges(input, ctx, &loop_, pctx);
        }
        Ok(changed)
    }

    /// Insert preheader block for a loop
    fn insert_preheader(
        function: &Function,
        ctx: &mut Context,
        loop_: &crate::passes::pass::structure::Loop<BasicBlock>,
        pctx: &mut PassContext,
    ) -> bool {
        let header = loop_.header;

        // Collect external predecessors (outside the loop)
        let external_preds: Vec<BasicBlock> = pctx
            .cfginfo
            .get(&function.get_id(ctx))
            .unwrap()
            .cfg
            .pre_set
            .get(&header)
            .map(|preds| {
                preds
                    .iter()
                    .filter(|pred| !loop_.body.contains(pred))
                    .cloned()
                    .collect()
            })
            .unwrap_or_default();

        // // If no external predecessors or already only one, nothing to do
        // if external_preds.len() == 1 {
        //     return false;
        // }

        // Create new preheader block
        let preheader = BasicBlock::new(ctx);
        header.insert_before(ctx, preheader).unwrap();
        let br = Instruction::br(ctx, header);
        let mut cur_inst = header.head(ctx);
        while let Some(inst) = cur_inst {
            if inst.is_alloca(ctx) {
                inst.unlink(ctx);
                preheader.push_back(ctx, inst).unwrap();
            }
            cur_inst = inst.succ(ctx);
        }
        preheader.push_back(ctx, br).unwrap();

        // Redirect all external predecessors to preheader
        for pred in external_preds {
            if loop_.body.contains(&pred) {
                continue;
            }
            let inst = pred.tail(ctx).unwrap();
            if inst.is_br(ctx) {
                inst.replace_operand_bbk(ctx, 0, preheader);
            } else if inst.is_cbr(ctx) {
                let true_bbk = inst.get_operand_bbk(ctx, 0).unwrap();
                let false_bbk = inst.get_operand_bbk(ctx, 1).unwrap();
                if true_bbk == header {
                    inst.replace_operand_bbk(ctx, 0, preheader);
                } else if false_bbk == header {
                    inst.replace_operand_bbk(ctx, 1, preheader);
                }
            } else {
                panic!("Unexpected terminator instruction in predecessor block")
            }
        }
        let (_, cfg) = pass::analysis::cfa::CFAnalysis::execute_function(function, ctx).unwrap();
        pctx.cfginfo.insert(function.get_id(ctx), cfg);
        let (_, dom_info) = pass::analysis::ltdomra::LTDomRelAnalysis
            .execute_function(
                function,
                ctx,
                pctx.cfginfo.get(&function.get_id(ctx)).unwrap(),
            )
            .unwrap();
        pctx.dom_info.insert(function.get_id(ctx), dom_info);
        let (_, loop_info) = pass::analysis::loopa::LoopAnalysis
            .execute_function(
                function,
                ctx,
                pctx.cfginfo.get(&function.get_id(ctx)).unwrap(),
                pctx.dom_info.get(&function.get_id(ctx)).unwrap(),
            )
            .unwrap();
        pctx.loop_info.insert(function.get_id(ctx), loop_info);
        true
    }

    /// Create dedicated exit blocks for a loop
    fn create_dedicated_exit_blocks(
        function: &Function,
        ctx: &mut Context,
        loop_: &crate::passes::pass::structure::Loop<BasicBlock>,
        pctx: &mut PassContext,
    ) -> bool {
        let cfg = pctx.cfginfo.get(&function.get_id(ctx)).unwrap();
        let mut changed = false;
        let mut exit_edges = HashMap::default();

        // Collect all exit edges (from inside loop to outside)
        for &block in &loop_.body {
            if let Some(successors) = cfg.cfg.succ_set.get(&block) {
                for &succ in successors {
                    if !loop_.body.contains(&succ) {
                        exit_edges.entry(succ).or_insert_with(Vec::new).push(block);
                    }
                }
            }
        }

        for (exit_succ, preds) in exit_edges {
            // Check if exit_succ has predecessors from outside the loop
            let has_external_preds = cfg
                .cfg
                .pre_set
                .get(&exit_succ)
                .map(|pre| pre.iter().any(|p| !loop_.body.contains(p)))
                .unwrap_or(false);

            if has_external_preds {
                // Create dedicated exit block
                let exit_block = BasicBlock::new(ctx);
                exit_succ.insert_before(ctx, exit_block).unwrap();
                let br = Instruction::br(ctx, exit_succ);
                exit_block.push_back(ctx, br).unwrap();

                // Redirect all loop exit edges to the new exit block
                for pred in preds {
                    let inst = pred.tail(ctx).unwrap();
                    if inst.is_br(ctx) {
                        inst.replace_operand_bbk(ctx, 0, exit_block);
                    } else if inst.is_cbr(ctx) {
                        let true_bbk = inst.get_operand_bbk(ctx, 0).unwrap();
                        let false_bbk = inst.get_operand_bbk(ctx, 1).unwrap();
                        if true_bbk == exit_succ {
                            inst.replace_operand_bbk(ctx, 0, exit_block);
                        } else if false_bbk == exit_succ {
                            inst.replace_operand_bbk(ctx, 1, exit_block);
                        }
                    } else {
                        panic!("Unexpected terminator instruction in predecessor block")
                    }
                }

                changed = true;
            }
        }
        if changed {
            let (_, cfg) =
                pass::analysis::cfa::CFAnalysis::execute_function(function, ctx).unwrap();
            pctx.cfginfo.insert(function.get_id(ctx), cfg);
            let (_, dom_info) = pass::analysis::ltdomra::LTDomRelAnalysis
                .execute_function(
                    function,
                    ctx,
                    pctx.cfginfo.get(&function.get_id(ctx)).unwrap(),
                )
                .unwrap();
            pctx.dom_info.insert(function.get_id(ctx), dom_info);
            let (_, loop_info) = pass::analysis::loopa::LoopAnalysis
                .execute_function(
                    function,
                    ctx,
                    pctx.cfginfo.get(&function.get_id(ctx)).unwrap(),
                    pctx.dom_info.get(&function.get_id(ctx)).unwrap(),
                )
                .unwrap();
            pctx.loop_info.insert(function.get_id(ctx), loop_info);
        }
        changed
    }

    /// Simplify multiple backedges to a single latch block
    fn simplify_backedges(
        function: &Function,
        ctx: &mut Context,
        loop_: &crate::passes::pass::structure::Loop<BasicBlock>,
        pctx: &mut PassContext,
    ) -> bool {
        let cfg = pctx.cfginfo.get(&function.get_id(ctx)).unwrap();
        let header = loop_.header;

        // Collect all backedges (loop body -> header)
        let backedges: Vec<BasicBlock> = cfg
            .cfg
            .pre_set
            .get(&header)
            .map(|preds| {
                preds
                    .iter()
                    .filter(|pred| loop_.body.contains(pred))
                    .cloned()
                    .collect()
            })
            .unwrap_or_default();

        // If only one backedge, nothing to do
        if backedges.len() <= 1 {
            return false;
        }

        // Create new latch block
        let latch_bbk = BasicBlock::new(ctx);
        header.insert_after(ctx, latch_bbk).unwrap();
        let br = Instruction::br(ctx, header);
        latch_bbk.push_back(ctx, br).unwrap();

        // Redirect all backedges to the new latch block
        for backedge_src in backedges {
            let inst = backedge_src.tail(ctx).unwrap();
            if inst.is_br(ctx) {
                inst.replace_operand_bbk(ctx, 0, latch_bbk);
            } else if inst.is_cbr(ctx) {
                let true_bbk = inst.get_operand_bbk(ctx, 0).unwrap();
                let false_bbk = inst.get_operand_bbk(ctx, 1).unwrap();
                if true_bbk == header {
                    inst.replace_operand_bbk(ctx, 0, latch_bbk);
                } else if false_bbk == header {
                    inst.replace_operand_bbk(ctx, 1, latch_bbk);
                }
            } else {
                panic!("Unexpected terminator instruction in predecessor block")
            }
        }
        let (_, cfg) = pass::analysis::cfa::CFAnalysis::execute_function(function, ctx).unwrap();
        pctx.cfginfo.insert(function.get_id(ctx), cfg);
        let (_, dom_info) = pass::analysis::ltdomra::LTDomRelAnalysis
            .execute_function(
                function,
                ctx,
                pctx.cfginfo.get(&function.get_id(ctx)).unwrap(),
            )
            .unwrap();
        pctx.dom_info.insert(function.get_id(ctx), dom_info);
        let (_, loop_info) = pass::analysis::loopa::LoopAnalysis
            .execute_function(
                function,
                ctx,
                pctx.cfginfo.get(&function.get_id(ctx)).unwrap(),
                pctx.dom_info.get(&function.get_id(ctx)).unwrap(),
            )
            .unwrap();
        pctx.loop_info.insert(function.get_id(ctx), loop_info);
        true
    }
}

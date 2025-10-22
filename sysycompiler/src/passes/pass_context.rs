use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::{
    backend::mir::mir_block::MirBlock,
    frontend::ir::instruction::Instruction,
    passes::pass::structure::{
        CallGraph, ControlFlowAnalysisResult, ControlFlowGraph, DominatorInfo, GetName, LoopInfo,
    },
};

pub struct PassContext {
    // 共享状态
    pub callgraph: CallGraph,
    pub cfginfo: HashMap<String, ControlFlowAnalysisResult>,
    pub dom_info: HashMap<String, DominatorInfo>,
    pub loop_info: HashMap<String, LoopInfo>,
    pub live_insts: HashSet<Instruction>,
    pub mcfginfo: HashMap<String, ControlFlowGraph<MirBlock>>,
}

impl PassContext {
    pub fn new() -> Self {
        PassContext {
            callgraph: CallGraph::new(),
            cfginfo: HashMap::default(),
            live_insts: HashSet::default(),
            dom_info: HashMap::default(),
            loop_info: HashMap::default(),
            mcfginfo: HashMap::default(),
        }
    }

    pub fn get_ir_exit_bbks(
        &self,
        name: &str,
    ) -> Option<&Vec<crate::frontend::ir::basicblock::BasicBlock>> {
        self.cfginfo.get(name).map(|cfg| &cfg.cfg.exit_blocks)
    }

    pub fn display_ir_exit_bbks(&self, ctx: &crate::frontend::ir::context::Context) {
        for (name, cfg) in self.cfginfo.iter() {
            println!("{} exit blocks: ", name);
            for bb in cfg.cfg.get_exit_bbks() {
                println!(
                    "  {}",
                    crate::frontend::ir2string::Display::display(*bb, ctx)
                );
            }
        }
    }

    pub fn display_ir_successors(&self, ctx: &crate::frontend::ir::context::Context) {
        for (name, cfg) in self.cfginfo.iter() {
            println!("CFG of function {}:", name);
            for bbk in cfg.reachable_blocks.iter() {
                print!("{}->", bbk.get_name(ctx));
                for succ in cfg.cfg.succ_set.get(bbk).unwrap() {
                    print!(" , {}", succ.get_name(ctx));
                }
                println!("");
            }
        }
    }

    pub fn display_ir_idominators(&self, ctx: &crate::frontend::ir::context::Context) {
        for (name, dom_info) in self.dom_info.iter() {
            println!("{} idominators: ", name);
            let reachable_blocks = self.cfginfo.get(name).unwrap().reachable_blocks.clone();
            for bb in reachable_blocks.iter() {
                println!(
                    "  {}: {}",
                    bb.get_name(ctx),
                    dom_info.idominators.get(bb).unwrap().get_name(ctx)
                );
            }
        }
    }

    pub fn display_ir_dominators(&self, ctx: &crate::frontend::ir::context::Context) {
        for (name, dom_info) in self.dom_info.iter() {
            println!("{} dominators: ", name);
            let reachable_blocks = self.cfginfo.get(name).unwrap().reachable_blocks.clone();
            for bb in reachable_blocks.iter() {
                println!("  {}: ", bb.get_name(ctx));
                for dom in dom_info.dominators.get(bb).unwrap() {
                    println!("    {}", dom.get_name(ctx));
                }
            }
        }
    }

    pub fn display_ir_loop_info(&self, ctx: &crate::frontend::ir::context::Context) {
        for (name, loop_info) in self.loop_info.iter() {
            println!("Loop info of function {}:", name);
            loop_info.print_loop_info(ctx);
        }
    }

    pub fn display_ir_cfg(&self, ctx: &crate::frontend::ir::context::Context) {
        for (name, cfg) in self.cfginfo.iter() {
            println!("\n>>>CFG of function {}:", name);
            cfg.print_cfg(ctx);
        }
    }

    pub fn display_ir_dtree(&self, ctx: &crate::frontend::ir::context::Context) {
        for (name, dom_info) in self.dom_info.iter() {
            println!("\n>>>Dominator tree of function {}:", name);
            dom_info.print_dominator_tree(ctx);
        }
    }

    pub fn display_ir_dfrontier(&self, ctx: &crate::frontend::ir::context::Context) {
        for (name, dom_info) in self.dom_info.iter() {
            println!("\n>>>Dominator frontier of function {}:", name);
            dom_info.print_dominance_frontier(ctx);
        }
    }

    pub fn display_mir_cfg(&self, mctx: &crate::backend::mir::mir_context::MirContext) {
        for (name, cfg) in self.mcfginfo.iter() {
            println!("\n>>>CFG of function {}:", name);
            cfg.print_mir_cfg(mctx);
        }
    }

    pub fn display_ir_callgraph(&self, ctx: &crate::frontend::ir::context::Context) {
        self.callgraph.display_call_graph(ctx);
    }
}

/// Dead Function Elimination
use crate::{
    passes::pass_manager::{Pass, PassError},
    utils::{
        linked_list::{LinkedListContainer, LinkedListNode},
        storage::Arena,
    },
};

pub struct DFE;

impl Pass for DFE {
    type Output = bool;

    type Error = PassError;

    fn pass_type(&self) -> crate::passes::pass_manager::PassType {
        todo!()
    }

    fn execute(
        &mut self,
        ctx: &mut crate::frontend::ir::context::Context,
        pctx: &mut crate::passes::pass_context::PassContext,
    ) -> Result<Self::Output, Self::Error> {
        let mut to_remove = Vec::new();
        for (func, num) in pctx.callgraph.function_call_num.iter() {
            if *num == 0 && func.get_id(ctx) != "main" {
                to_remove.push(*func);
            }
        }
        let remove_num = to_remove.len();
        for func in to_remove {
            let mut cur_bbk = func.head(ctx);
            while let Some(bbk) = cur_bbk {
                cur_bbk = bbk.succ(ctx);
                bbk.remove_without_check(ctx);
            }
            ctx.functions.dealloc(func.0);
        }

        #[cfg(debug_assertions)]
        println!("···[DFE]: {} functions removed", remove_num);
        Ok(remove_num > 0)
    }

    fn name(&self) -> &'static str {
        "DeadFunctionElimination"
    }

    fn dependencies(&self) -> &[&'static str] {
        &["FunctionCallAnalysis"]
    }

    fn effects(&self) -> &[&'static str] {
        &["FunctionCallAnalysis"]
    }
}

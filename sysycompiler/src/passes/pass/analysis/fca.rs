/// Funncation call analysis pass
///
/// 分析函数的调用关系，生成调用关系图
///
use crate::{
    passes::{
        pass::structure::CallGraph,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::LinkedListContainer,
};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::u32;

pub struct FunctionCallAnalysis;

impl Pass for FunctionCallAnalysis {
    type Output = bool;
    type Error = PassError;

    fn pass_type(&self) -> crate::passes::pass_manager::PassType {
        PassType::GlobalBasedAnalsysis
    }

    fn execute(
        &mut self,
        ctx: &mut crate::frontend::ir::context::Context,
        pctx: &mut crate::passes::pass_context::PassContext,
    ) -> Result<Self::Output, Self::Error> {
        let mut fcg = CallGraph {
            functions: HashMap::default(),
            function_call_num: HashMap::default(),
            direct_calls: HashMap::default(),
            reverse_calls: HashMap::default(),
            entry_points: None,
        };

        // 初始化
        for func in ctx.get_functions() {
            fcg.functions.insert(func.get_id(ctx), func);
            fcg.function_call_num.insert(func, 0);
            fcg.direct_calls.insert(func, HashSet::default());
            fcg.reverse_calls.insert(func, HashSet::default());
        }

        for func in ctx.get_functions() {
            for bbk in func.iter(ctx) {
                for inst in bbk.iter(ctx) {
                    if inst.is_call(ctx) {
                        // println!("Analyze [{}]", inst.display(ctx));
                        let func_name = inst
                            .get_operand(ctx, 0)
                            .unwrap()
                            .get_func_name(ctx)
                            .unwrap();
                        let callee = fcg.functions.get(&func_name);
                        if callee.is_none() {
                            // 跳过对库函数的分析
                            continue;
                        }
                        let callee = callee.unwrap();
                        if *fcg.function_call_num.get(callee).unwrap() != u32::MAX {
                            if pctx
                                .loop_info
                                .get(&func.get_id(ctx))
                                .unwrap()
                                .is_in_loop(&bbk)
                            {
                                // println!("[{}] calls [{}] in loop", func.get_id(ctx), callee.get_id(ctx));
                                fcg.function_call_num.insert(*callee, u32::MAX);
                            } else {
                                // println!("[{}] calls [{}]", func.get_id(ctx), callee.get_id(ctx));
                                *fcg.function_call_num.get_mut(callee).unwrap() += 1;
                            }
                        }
                        fcg.direct_calls.get_mut(&func).unwrap().insert(*callee);
                        fcg.reverse_calls.get_mut(callee).unwrap().insert(func);
                    }
                }
            }
        }

        fcg.entry_points = Some(fcg.functions.get("main").unwrap().clone());

        pctx.callgraph = fcg;
        Ok(false)
    }

    fn name(&self) -> &'static str {
        "FunctionCallAnalysis"
    }

    fn dependencies(&self) -> &[&'static str] {
        &["LoopAnalysis"]
    }

    fn effects(&self) -> &[&'static str] {
        &[]
    }
}

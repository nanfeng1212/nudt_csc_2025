use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::{
    frontend::ir::{
        basicblock::BasicBlock,
        context::Context,
        function::Function,
        instruction::{Instruction, MemAccessOp},
        value::Value,
    },
    passes::{
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::{LinkedListContainer, LinkedListNode},
};

pub struct Reg2Mem;

impl Pass for Reg2Mem {
    type Output = bool;
    type Error = PassError;

    fn pass_type(&self) -> PassType {
        PassType::FunctionBasedOptimization
    }

    fn execute(&mut self, ctx: &mut Context, _pctx: &mut PassContext) -> Result<bool, Self::Error> {
        let mut num_insert = 0;
        let functions: Vec<_> = ctx.get_functions().collect();

        for function in functions {
            num_insert += self.optimize_function(&function, ctx)?;
        }

        #[cfg(debug_assertions)]
        println!("···[Reg2Mem]Inserted {} instructions", num_insert);
        Ok(num_insert > 0)
    }

    fn name(&self) -> &'static str {
        "Reg2Mem"
    }

    fn dependencies(&self) -> &[&'static str] {
        &["Mem2Reg"]
    }

    fn effects(&self) -> &[&'static str] {
        &["Mem2Reg"]
    }
}

impl Reg2Mem {
    fn optimize_function(&self, function: &Function, ctx: &mut Context) -> Result<i32, PassError> {
        let mut num_insert = 0;

        let mut phi_alloca = HashMap::default();
        let phi_bbks = self.collect_phi_bbks(function, ctx, &mut phi_alloca);

        for (bbk, phi_insts) in phi_bbks {
            for phi_inst in phi_insts {
                let bbk_value = phi_inst.get_phi(ctx);
                let typ = phi_inst.result_typ(ctx).clone();
                for (pre_bbk, value) in bbk_value {
                    let store = Instruction::memaccess(
                        ctx,
                        MemAccessOp::Store,
                        typ,
                        vec![value, phi_alloca[&phi_inst]],
                    );
                    let _ = pre_bbk.get_tail(ctx).unwrap().insert_before(ctx, store);
                    num_insert += 1;
                }
                let load = Instruction::load_with_result(
                    ctx,
                    phi_inst.get_result(ctx).unwrap(),
                    phi_alloca[&phi_inst],
                );
                let _ = bbk.push_front(ctx, load);
                num_insert += 1;
                phi_inst.remove(ctx);
            }
        }

        Ok(num_insert)
    }

    fn collect_phi_bbks(
        &self,
        function: &Function,
        ctx: &mut Context,
        phi_alloca: &mut HashMap<Instruction, Value>,
    ) -> HashMap<BasicBlock, HashSet<Instruction>> {
        let entry = function.head(ctx).unwrap();
        let mut phi_bbks = HashMap::default();
        let bbks: Vec<BasicBlock> = function.iter(ctx).collect();
        for bbk in bbks {
            let mut inst = bbk.get_head(ctx);
            if inst.unwrap().is_phi(ctx) {
                phi_bbks.insert(bbk, HashSet::default());
            }
            while inst.is_some() {
                if inst.unwrap().is_phi(ctx) {
                    let typ = inst.unwrap().result_typ(ctx).clone();
                    let alloca =
                        Instruction::memaccess(ctx, MemAccessOp::Alloca { typ }, typ, vec![]);
                    let _ = entry.push_front(ctx, alloca);
                    phi_alloca.insert(inst.unwrap(), alloca.get_result(ctx).unwrap());
                    phi_bbks.get_mut(&bbk).unwrap().insert(inst.unwrap());
                } else {
                    break;
                }
                inst = inst.unwrap().succ(ctx);
            }
        }
        phi_bbks
    }
}

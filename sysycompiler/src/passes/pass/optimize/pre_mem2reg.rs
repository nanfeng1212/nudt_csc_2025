use crate::frontend::ir::defuse::Useable;
use crate::utils::linked_list::{LinkedListContainer, LinkedListNode};
use crate::{
    frontend::ir::{
        context::Context,
        function::Function,
        instruction::{InstructionKind, MemAccessOp},
        value::{ConstantValue, Value},
    },
    passes::{
        pass::{structure::ControlFlowAnalysisResult, structure::DominatorInfo},
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
};
use rustc_hash::FxHashSet as HashSet;

pub struct PreMem2Reg;

impl Pass for PreMem2Reg {
    type Output = bool;
    type Error = PassError;

    fn pass_type(&self) -> PassType {
        PassType::FunctionBasedOptimization
    }

    fn execute(&mut self, ctx: &mut Context, pctx: &mut PassContext) -> Result<bool, Self::Error> {
        let mut _num_remove = 0;
        let functions: Vec<Function> = ctx.get_functions().collect();
        // println!("[Preprocess Mem2Reg]Optimizing {} functions", functions.len());
        for function in functions {
            if let Some(cfg_info) = pctx.cfginfo.get(&function.get_id(ctx)) {
                if let Some(dom_info) = pctx.dom_info.get(&function.get_id(ctx)) {
                    let (_changed_f, num_remove_f) =
                        self.optimize_function(&function, ctx, cfg_info, dom_info)?;
                    _num_remove += num_remove_f;
                }
            }
        }
        #[cfg(debug_assertions)]
        println!("···[PreMem2Reg]Removed {} instructions", _num_remove);
        Ok(_num_remove > 0)
    }

    fn name(&self) -> &'static str {
        "PreprocessMem2Reg"
    }

    fn dependencies(&self) -> &[&'static str] {
        &["CFAnalysis", "DominatorRelAnalysis"]
    }

    fn effects(&self) -> &[&'static str] {
        &[]
    }
}

impl PreMem2Reg {
    fn optimize_function(
        &self,
        function: &Function,
        ctx: &mut Context,
        _cfg_info: &ControlFlowAnalysisResult,
        dom_info: &DominatorInfo,
    ) -> Result<(bool, i32), PassError> {
        // println!("[Preprocess Mem2Reg]Optimizing function {}", function.get_id(ctx));
        let mut num_remove = 0;
        let entry_block = function
            .head(ctx)
            .ok_or(PassError::InvalidInput("No entry block".to_string()))?;

        // 步骤1: 收集所有可提升的alloca指令
        let mut allocas = Vec::new();
        let mut geps = Vec::new();
        let mut inst = entry_block.get_head(ctx);
        while inst.is_some() {
            if inst.unwrap().is_alloca(ctx) {
                if Self::is_promotable(inst.unwrap().get_result(ctx).unwrap(), ctx) {
                    allocas.push(inst.unwrap());
                    // println!("Promotable alloca: {}", crate::frontend::ir2string::Display::display(inst.unwrap(), ctx));
                }
            } else if inst.unwrap().is_gep(ctx) {
                geps.push(inst.unwrap());
            }
            inst = inst.unwrap().succ(ctx);
        }

        if allocas.is_empty() && geps.is_empty() {
            return Ok((false, num_remove));
        }

        for &alloca in &allocas {
            let a_num_remove = Self::collect_stores(alloca.get_result(ctx).unwrap(), ctx, dom_info);
            num_remove += a_num_remove;
        }
        // let mut gep_remove = 0; //有待修改实现
        // for &gep in &geps {
        //     let a_num_remove = Self::collect_stores(gep.get_result(ctx).unwrap(), ctx, dom_info);
        //     gep_remove += a_num_remove;
        //     num_remove += a_num_remove;
        // }
        // if gep_remove > 0 {
        //     panic!("[Preprocess Mem2Reg] GEP should not be optimized");
        // }
        // println!("[Preprocess Mem2Reg]{}: Removed {} instructions", function.get_id(ctx), num_remove);
        Ok((num_remove > 0, num_remove))
    }

    /// 检查alloca是否可提升
    /// 未被用于 volatile 指令
    /// 直接被用于 LoadInst 或 StoreInst
    fn is_promotable(alloca: Value, ctx: &Context) -> bool {
        // println!("Checking promotable for {} with {} users", crate::frontend::ir2string::Display::display(alloca, ctx), alloca.users(ctx).into_iter().count());
        for user in alloca.users(ctx) {
            match user.get_instruction().get_kind(ctx) {
                InstructionKind::MemAccess {
                    op: MemAccessOp::Store { .. },
                } => {
                    // println!("{}: {}", crate::frontend::ir2string::Display::display(alloca, ctx),crate::frontend::ir2string::Display::display(user.get_instruction(), ctx));
                }
                InstructionKind::MemAccess {
                    op: MemAccessOp::Load { .. },
                } => {
                    // println!("{}: {}", crate::frontend::ir2string::Display::display(alloca, ctx),crate::frontend::ir2string::Display::display(user.get_instruction(), ctx));
                }
                InstructionKind::MemAccess {
                    op: MemAccessOp::Alloca { .. },
                } => {
                    // println!("{}: {}", crate::frontend::ir2string::Display::display(alloca, ctx),crate::frontend::ir2string::Display::display(user.get_instruction(), ctx))
                }
                _ => return false, // 非load/store使用
            }
        }
        true
    }

    /// 收集所有store指令,即alloca对应的定值指令
    /// 1.如果一个 alloca 只有一个 def，那么 users 可以替换成这个 store 的值。这里需要保证下面两个条件（见函数 rewriteSingleStoreAlloca）
    ///     如果 load 和 store 在同一个基本块，则 store 应该在 load 前面
    ///     如果二者在不同基本块，则需要保证 load 指令能被 store 支配
    /// 2.如果某个 alloca 的所有 defs 和 users 都在同一个基本块内，且 store 在 load 前面，则可以将基本块内的 load 替换成对应的 store
    pub fn collect_stores(alloca: Value, ctx: &mut Context, dom_info: &DominatorInfo) -> i32 {
        let mut num_remove = 0;
        let mut stores = Vec::new();
        let mut loads = Vec::new();

        // let user_count = alloca.users(ctx).into_iter().count();
        // if user_count == 1 {
        //     let user = alloca.users(ctx).into_iter().next().unwrap();
        //     let instruction_to_remove = user.get_instruction();
        //     // println!("Removing single-use alloca: {}", crate::frontend::ir2string::Display::display(instruction_to_remove, ctx));
        //     instruction_to_remove.remove(ctx);
        //     num_remove += 1;
        //     return num_remove;
        // }

        // 收集所有store指令和load指令
        for user in alloca.users(ctx) {
            let user = user.get_instruction();
            if user.is_store(ctx) {
                stores.push(user);
            }
            // else if user.is_gep(ctx) {
            //     return num_remove;
            // }
            else if user.is_load(ctx) {
                loads.push(user);
            }
        }

        if stores.is_empty() {
            for load in loads {
                let mut zero_const = ConstantValue::int32(ctx, 0);
                if load.result_typ(ctx).is_float(ctx) {
                    zero_const = ConstantValue::float32(ctx, 0.0);
                }
                let zero = Value::constant(ctx, zero_const);
                num_remove += load.replace_all_uses_with(ctx, zero);
                load.remove(ctx);
            }
            return num_remove;
        }

        if stores.len() == 1 {
            let store = stores[0];
            let store_value = store.get_operand(ctx, 0).unwrap();
            for load in &loads {
                if load.has_same_bbk(ctx, store) {
                    if !load.is_before(ctx, store) {
                        num_remove += load.replace_all_uses_with(ctx, store_value);
                    } else {
                        let mut zero_const = ConstantValue::int32(ctx, 0);

                        if load.result_typ(ctx).is_float(ctx) {
                            zero_const = ConstantValue::float32(ctx, 0.0);
                        }
                        let zero = Value::constant(ctx, zero_const);
                        num_remove += load.replace_all_uses_with(ctx, zero);
                    }
                    load.remove(ctx);
                } else {
                    let def_bbk = store.get_basicblock(ctx).unwrap();
                    let use_bbk = load.get_basicblock(ctx).unwrap();
                    if dom_info
                        .dominators
                        .get(&&use_bbk)
                        .unwrap()
                        .contains(&def_bbk)
                    {
                        num_remove += load.replace_all_uses_with(ctx, store_value);
                        load.remove(ctx);
                        num_remove += 1;
                    }
                }
            }
            // store.remove(ctx);
            // num_remove += 1;
            stores.clear();
            loads.clear();
        } else {
            let def_bbks = stores
                .iter()
                .map(|store| store.get_basicblock(ctx).unwrap())
                .collect::<HashSet<_>>();
            let use_bbks = loads
                .iter()
                .map(|load| load.get_basicblock(ctx).unwrap())
                .collect::<HashSet<_>>();
            let union_bbks = def_bbks.union(&use_bbks);
            if union_bbks.count() == 1 {
                let bbk = def_bbks.iter().next().unwrap();
                let mut cur_store = None;
                let mut cur_inst = bbk.get_head(ctx);
                while cur_inst.is_some() {
                    let inst = cur_inst.unwrap();
                    if inst.is_store(ctx) && alloca == inst.get_operand(ctx, 1).unwrap() {
                        cur_store = Some(inst);
                    }
                    if inst.is_load(ctx) && alloca == inst.get_operand(ctx, 0).unwrap() {
                        if cur_store.is_none() {
                            let mut zero_const = ConstantValue::int32(ctx, 0);
                            if inst.result_typ(ctx).is_float(ctx) {
                                zero_const = ConstantValue::float32(ctx, 0.0);
                            }
                            let zero = Value::constant(ctx, zero_const);
                            num_remove += inst.replace_all_uses_with(ctx, zero);
                        } else {
                            let store_value = cur_store.unwrap().get_operand(ctx, 0).unwrap();
                            num_remove += inst.replace_all_uses_with(ctx, store_value);
                        }
                    }
                    cur_inst = inst.succ(ctx);
                }
                num_remove += loads.len() as i32;
                num_remove += stores.len() as i32;
                // for store in &stores {
                //     store.remove(ctx);
                // }
                for load in &loads {
                    load.remove(ctx);
                }
                stores.clear();
                loads.clear();
            }
        }

        let user_count = alloca.users(ctx).into_iter().count();
        if user_count == 1 {
            let user = alloca.users(ctx).into_iter().next().unwrap();
            let instruction_to_remove = user.get_instruction();
            // println!("Removing single-use alloca: {}", crate::frontend::ir2string::Display::display(instruction_to_remove, ctx));
            instruction_to_remove.remove(ctx);
            num_remove += 1;
            return num_remove;
        }
        num_remove
    }
}

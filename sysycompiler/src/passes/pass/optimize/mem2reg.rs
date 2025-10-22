use crate::frontend::ir::defuse::Useable;
use crate::utils::linked_list::LinkedListNode;
use crate::{
    frontend::ir::{
        basicblock::BasicBlock,
        context::Context,
        function::Function,
        instruction::{Instruction, InstructionKind, MemAccessOp},
        value::Value,
    },
    passes::{
        pass::{structure::ControlFlowAnalysisResult, structure::DominatorInfo},
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::LinkedListContainer,
};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::collections::VecDeque;

pub struct Mem2Reg;

impl Pass for Mem2Reg {
    type Output = bool;
    type Error = PassError;

    fn pass_type(&self) -> PassType {
        PassType::FunctionBasedOptimization
    }

    fn execute(&mut self, ctx: &mut Context, pctx: &mut PassContext) -> Result<bool, Self::Error> {
        let mut num_remove = 0;
        let functions: Vec<_> = ctx.get_functions().collect();

        for function in functions {
            if let Some(cfg_info) = pctx.cfginfo.get(&function.get_id(ctx)) {
                if let Some(dom_info) = pctx.dom_info.get(&function.get_id(ctx)) {
                    num_remove += self.optimize_function(&function, ctx, cfg_info, dom_info)?;
                }
            }
        }

        #[cfg(debug_assertions)]
        println!("···[Mem2Reg]Removed {} instructions", num_remove);
        Ok(num_remove > 0)
    }

    fn name(&self) -> &'static str {
        "Mem2Reg"
    }

    fn dependencies(&self) -> &[&'static str] {
        &["CFAnalysis", "DominatorRelAnalysis"]
    }

    fn effects(&self) -> &[&'static str] {
        &["Reg2Mem"]
    }
}

impl Mem2Reg {
    fn optimize_function(
        &self,
        function: &Function,
        ctx: &mut Context,
        cfg_info: &ControlFlowAnalysisResult,
        dom_info: &DominatorInfo,
    ) -> Result<i32, PassError> {
        let mut num_remove = 0;
        let entry_block = function
            .head(ctx)
            .ok_or(PassError::InvalidInput("No entry block".to_string()))?;

        // 步骤1: 收集所有可提升的alloca指令
        let mut allocas = Vec::new();
        let mut inst = entry_block.get_head(ctx);
        while inst.is_some() {
            if let InstructionKind::MemAccess {
                op: MemAccessOp::Alloca { .. },
            } = inst.unwrap().get_kind(ctx)
            {
                if self.is_promotable(inst.unwrap().get_result(ctx).unwrap(), ctx) {
                    allocas.push(inst.unwrap());
                }
            }
            inst = inst.unwrap().succ(ctx);
        }

        if allocas.is_empty() {
            return Ok(num_remove);
        }

        // 步骤2: 为每个可提升的变量计算支配边界
        let mut var_info = HashMap::default();
        for &alloca in &allocas {
            let stores = self.collect_stores(alloca.get_result(ctx).unwrap(), ctx);
            let def_blocks = stores
                .iter()
                .map(|store| store.get_basicblock(ctx).unwrap())
                .collect();
            let phi_blocks = self.compute_phi_blocks(&def_blocks, dom_info);
            var_info.insert(alloca, phi_blocks);
        }

        let mut alloca_phi = HashMap::default();
        // 步骤3: 插入Phi函数
        for (&alloca, phi_blocks) in &var_info {
            for &block in phi_blocks {
                let phi = self.insert_phi(alloca.get_result(ctx).unwrap(), block, ctx);
                alloca_phi.insert(phi, alloca.get_result(ctx).unwrap());
            }
        }

        // 使用深度优先搜索rename
        let mut worklist: Vec<(BasicBlock, HashMap<Value, Value>)> =
            vec![(entry_block, HashMap::default())];
        let mut visited = HashSet::default();

        let mut wait_to_remove = Vec::new();
        for inst in allocas {
            let ty = inst.result_typ(ctx);
            worklist
                .get_mut(0)
                .unwrap()
                .1
                .insert(inst.get_result(ctx).unwrap(), Value::undef(ctx, ty));
            num_remove += 1;
            wait_to_remove.push(inst);
            // inst.remove_only(ctx);
        }

        while let Some((cur_bbk, mut incoming_vals)) = worklist.pop() {
            if visited.contains(&cur_bbk) {
                continue;
            }
            visited.insert(cur_bbk);
            let insts: Vec<Instruction> = cur_bbk.iter(ctx).collect();
            for inst in insts {
                let op0 = inst.get_operand(ctx, 0);
                let op1 = inst.get_operand(ctx, 1);
                if inst.is_store(ctx) && incoming_vals.contains_key(&op1.unwrap()) {
                    incoming_vals.insert(op1.unwrap(), op0.unwrap());
                    // inst.remove_only(ctx);
                    wait_to_remove.push(inst);
                    num_remove += 1;
                } else if inst.is_load(ctx) && incoming_vals.contains_key(&op0.unwrap()) {
                    inst.replace_all_uses_with(ctx, incoming_vals[&op0.unwrap()].clone());
                    inst.remove(ctx);
                    num_remove += 1;
                } else if inst.is_phi(ctx) {
                    let alloca = alloca_phi.get(&inst).unwrap();
                    incoming_vals.insert(*alloca, inst.get_result(ctx).unwrap());
                }
            }

            let successors = cfg_info.cfg.get_successors(&cur_bbk).unwrap();
            for successor in successors {
                worklist.push((*successor, incoming_vals.clone()));
                let insts = successor.iter(ctx).collect::<Vec<_>>();
                for inst in insts {
                    if inst.is_phi(ctx) {
                        let alloca = alloca_phi.get(&inst).unwrap();
                        inst.insert_phi_operand(ctx, cur_bbk, incoming_vals[alloca].clone());
                    } else {
                        // phi指令只会位于基本块开头
                        break;
                    }
                }
            }
        }
        // 步骤4: 删除指令
        for inst in wait_to_remove {
            inst.remove(ctx);
        }
        Ok(num_remove)
    }

    /// 检查alloca是否可提升
    /// 未被用于 volatile 指令
    /// 直接被用于 LoadInst 或 StoreInst
    fn is_promotable(&self, alloca: Value, ctx: &Context) -> bool {
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
    /// 理论上经过pre_mem2reg后，可以移除的load和store指令已经移除，剩余可提升的allloca，要么只有alloca，要么需要对应phi指令才能消除
    fn collect_stores(&self, alloca: Value, ctx: &mut Context) -> Vec<Instruction> {
        let mut stores = Vec::new();

        // 收集所有store指令和load指令
        for user in alloca.users(ctx) {
            let user = user.get_instruction();
            if user.is_store(ctx) {
                stores.push(user);
            } else if user.is_gep(ctx) {
                panic!(
                    "[Mem2Reg]GEP instruction [{}] is used by a promotable alloca",
                    crate::frontend::ir2string::Display::display(user, ctx)
                );
            }
        }

        stores
    }

    /// 计算需要插入Phi函数的基本块
    fn compute_phi_blocks(
        &self,
        def_blocks: &HashSet<BasicBlock>,
        dom_info: &DominatorInfo,
    ) -> HashSet<BasicBlock> {
        let mut phi_blocks = HashSet::default();
        let mut worklist: VecDeque<_> = def_blocks.iter().copied().collect();
        let mut visited = HashSet::default();

        while let Some(block) = worklist.pop_front() {
            if !visited.insert(block) {
                continue;
            }

            // 获取当前块的支配边界
            if let Some(df) = dom_info.dominance_frontier.get(&block) {
                for &df_block in df {
                    if phi_blocks.insert(df_block) {
                        worklist.push_back(df_block);
                    }
                }
            }
        }

        phi_blocks
    }

    /// 在指定基本块插入Phi函数
    fn insert_phi(&self, alloca: Value, block: BasicBlock, ctx: &mut Context) -> Instruction {
        let ty = alloca.kind(ctx);

        // 创建Phi指令
        let phi = Instruction::phi(ctx, ty);

        // 插入到基本块开头
        let res = block.push_front(ctx, phi);
        if let Err(e) = res {
            panic!("Failed to insert phi function: {:?}", e);
        }
        return phi;
    }
}

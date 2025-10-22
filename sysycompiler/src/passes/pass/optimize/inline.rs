use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::{
    frontend::ir::{
        basicblock::BasicBlock,
        context::Context,
        defuse::{Useable, User},
        function::Function,
        instruction::{Instruction, InstructionKind},
        typ::Typ,
        value::Value,
    },
    passes::{
        pass::{self, structure::ControlFlowGraph},
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::{LinkedListContainer, LinkedListNode},
};

pub struct Inline;

impl Pass for Inline {
    type Output = bool;

    type Error = PassError;

    fn pass_type(&self) -> crate::passes::pass_manager::PassType {
        PassType::GlobalBasedOptimization
    }

    fn execute(
        &mut self,
        ctx: &mut crate::frontend::ir::context::Context,
        pctx: &mut crate::passes::pass_context::PassContext,
    ) -> Result<Self::Output, Self::Error> {
        let funcs = pctx.callgraph.get_post_order();
        let conservative_inline_size = 15; // 最大展开函数大小
        let max_inline_size = 30; // 最大展开函数大小
        let mut visited = HashSet::default();
        let mut funs_to_inline = HashMap::default();

        for func in funcs {
            if visited.contains(&func.get_id(ctx)) {
                break;
            }
            visited.insert(func.get_id(ctx));
            if func.get_id(ctx) == "main" {
                continue;
            } else if pctx.callgraph.call_self(&func) {
                continue; // 跳过递归调用的函数，也许可以按照一定限制进行展开
            } else if pctx.callgraph.function_call_num[&func] == u32::MAX {
                let num = Inline::inline_function(ctx, func, pctx);
                funs_to_inline.insert(func.get_id(ctx), num);
            } else {
                let size = Inline::get_func_size(&func, ctx);
                if size <= conservative_inline_size {
                    let num = Inline::inline_function(ctx, func, pctx);
                    funs_to_inline.insert(func.get_id(ctx), num);
                } else if size <= max_inline_size && pctx.callgraph.function_call_num[&func] <= 5 {
                    let num = Inline::inline_function(ctx, func, pctx);
                    funs_to_inline.insert(func.get_id(ctx), num);
                } else {
                    continue;
                }
            }
        }

        let inline_num = funs_to_inline.len();
        #[cfg(debug_assertions)]
        {
            println!("···[Inline]Inlined {} functions", inline_num);
            for (func, num) in funs_to_inline {
                println!("······Inlined function {} with {} call sites", func, num);
            }
        }
        Ok(inline_num > 0)
    }

    fn name(&self) -> &'static str {
        "Inline"
    }

    fn dependencies(&self) -> &[&'static str] {
        &["FunctionCallAnalysis"]
    }

    fn effects(&self) -> &[&'static str] {
        &["FunctionCallAnalysis", "CFAnalysis"]
    }
}

impl Inline {
    fn get_func_size(func: &crate::frontend::ir::function::Function, ctx: &Context) -> u32 {
        let mut size = 0;
        let mut cur_bbk = func.head(ctx);
        while let Some(bbk) = cur_bbk {
            size += bbk.iter(ctx).count() as u32;
            cur_bbk = bbk.succ(ctx);
        }
        size
    }
}

impl Inline {
    fn inline_function(ctx: &mut Context, callee: Function, pctx: &mut PassContext) -> usize {
        // 收集所有调用点
        let call_sites = Self::find_call_sites(ctx, &callee, pctx);
        if call_sites.is_empty() {
            return 0;
        }
        let inline_num = call_sites.len();

        // println!("Inlining function:\n {}", callee.display(ctx));
        for (caller, caller_bbk, call_inst) in call_sites {
            // println!("Caller: {}", caller.display(ctx));
            // 拆分call指令所在基本块为caller_bb+cont_bbk
            let cont_bbk =
                Self::create_continuation_block(ctx, &caller, caller_bbk, call_inst, pctx);
            // 复制函数体到caller
            let (br_tar, ret_val) = Self::clone_function(
                ctx,
                &mut caller.head(ctx).unwrap(),
                &callee,
                pctx,
                &call_inst.get_operands(ctx)[1..],
                &cont_bbk,
            );
            // println!("Caller BBK: {}", caller_bbk.display(ctx));
            // call指令替换为br指令
            let br = Instruction::br(ctx, br_tar);
            caller_bbk.push_back(ctx, br).unwrap();
            // println!("Caller BBK after inlining:\n {}", caller_bbk.display(ctx));
            // 替换对call指令结果的使用
            if ret_val.is_some() {
                let old_res = call_inst.get_result(ctx).unwrap();
                call_inst.remove_only(ctx);
                if !old_res.is_removed(ctx) {
                    old_res.replace_all_uses_with(ctx, ret_val.unwrap());
                }
            } else {
                call_inst.remove(ctx);
            }
            let res = pass::analysis::cfa::CFAnalysis::execute_function(&caller, ctx);
            let new_cfg = res.unwrap().1;
            pctx.cfginfo.insert(caller.get_id(ctx), new_cfg);
            // println!("Caller after inlining:\n {}", caller.display(ctx));
        }
        return inline_num;
    }

    //=== 辅助函数实现 ===//

    /// 查找所有调用点
    fn find_call_sites(
        ctx: &Context,
        callee: &Function,
        pctx: &PassContext,
    ) -> Vec<(Function, BasicBlock, Instruction)> {
        let mut sites = Vec::new();

        for caller in pctx.callgraph.reverse_calls.get(callee).unwrap() {
            let mut cur_bbk = caller.head(ctx);
            while let Some(bbk) = cur_bbk {
                for inst in bbk.iter(ctx).rev() {
                    if let InstructionKind::Call { .. } = &inst.get_kind(ctx) {
                        if inst
                            .get_operand(ctx, 0)
                            .unwrap()
                            .get_func_name(ctx)
                            .unwrap()
                            == callee.get_id(ctx)
                        {
                            sites.push((*caller, bbk, inst));
                        }
                    }
                }
                cur_bbk = bbk.succ(ctx);
            }
        }

        sites
    }

    /// 创建后续基本块
    fn create_continuation_block(
        ctx: &mut Context,
        caller: &Function,
        caller_bbk: BasicBlock,
        call_inst: Instruction,
        pctx: &PassContext,
    ) -> BasicBlock {
        // 拆分基本块：调用点之前(call指令仍保留在在这) & 调用点之后(cont_bbk)
        let cont_bbk = BasicBlock::new(ctx);
        let mut insts_to_move = Vec::new(); // 调用点之前的指令
        let mut cur_inst = call_inst.succ(ctx);
        while let Some(inst) = cur_inst {
            insts_to_move.push(inst);
            cur_inst = inst.succ(ctx);
        }
        for inst in insts_to_move.iter() {
            inst.unlink(ctx);
            cont_bbk.push_back(ctx, inst.clone()).unwrap();
        }
        for succ_bbk in pctx
            .cfginfo
            .get(&caller.get_id(ctx))
            .unwrap()
            .cfg
            .get_successors(&caller_bbk)
            .unwrap()
        {
            let mut cur_inst = succ_bbk.head(ctx);
            while let Some(inst) = cur_inst {
                if inst.is_phi(ctx) {
                    inst.replace_phi_bbk(ctx, caller_bbk, cont_bbk);
                } else {
                    break;
                }
                cur_inst = inst.succ(ctx);
            }
        }
        caller_bbk.insert_after(ctx, cont_bbk).unwrap();
        cont_bbk
    }

    fn clone_function(
        ctx: &mut Context,
        caller_entry: &mut BasicBlock,
        callee: &Function,
        pctx: &PassContext,
        params: &[Value],
        cont_bbk: &BasicBlock,
    ) -> (BasicBlock, Option<Value>) {
        let mut ret_val = None;
        let mut params_map = HashMap::default();
        let mut bbks_map = HashMap::default();
        for (i, old_param) in callee.get_parameters(ctx).iter().enumerate() {
            params_map.insert(*old_param, params[i]);
        }

        // 复制基本块，并建立映射关系
        let mut cur_bbk = callee.head(ctx);
        while let Some(bbk) = cur_bbk {
            let cloned_bbk = BasicBlock::new(ctx);
            bbks_map.insert(bbk, cloned_bbk);
            cur_bbk = bbk.succ(ctx);
        }

        let mut visited = HashSet::default();
        let mut postorder_list = Vec::new();
        Self::postorder(
            ctx,
            &pctx.cfginfo.get(&callee.get_id(ctx)).unwrap().cfg,
            &callee.head(ctx).unwrap(),
            &mut visited,
            &mut postorder_list,
        );
        let rev_postorder_list = postorder_list.iter().rev().cloned().collect::<Vec<_>>();

        let mut br_tar = None;
        for bbk in &rev_postorder_list {
            let cloned_bbk = bbks_map[bbk];
            cont_bbk.insert_before(ctx, cloned_bbk).unwrap();
            let mut cur_inst = bbk.head(ctx);
            while let Some(inst) = cur_inst {
                let cloned_inst =
                    Self::clone_inst(ctx, inst, &mut params_map, &mut bbks_map, cont_bbk);
                if inst.is_ret(ctx) && inst.get_operand(ctx, 0).is_some() {
                    ret_val = params_map.get(&inst.get_operand(ctx, 0).unwrap()).cloned();
                    if ret_val.is_none() && inst.get_operand(ctx, 0).is_some() {
                        ret_val = inst.get_operand(ctx, 0);
                    }
                }
                if inst.is_alloca(ctx) {
                    caller_entry.push_front(ctx, cloned_inst).unwrap();
                } else {
                    cloned_bbk.push_back(ctx, cloned_inst).unwrap();
                }
                cur_inst = inst.succ(ctx);
            }
            if br_tar.is_none() {
                br_tar = Some(cloned_bbk)
            }
        }

        (br_tar.unwrap(), ret_val)
    }

    fn clone_inst(
        ctx: &mut Context,
        inst: Instruction,
        params_map: &mut HashMap<Value, Value>,
        bbks_map: &mut HashMap<BasicBlock, BasicBlock>,
        cont_bbk: &BasicBlock,
    ) -> Instruction {
        let kind = inst.get_kind(ctx).clone();
        let opds = inst
            .get_operands(ctx)
            .iter()
            .map(|opd| params_map.get(opd).copied().unwrap_or(*opd))
            .collect::<Vec<_>>();
        let old_res = inst.get_result(ctx);
        let cloned_inst;
        if old_res.is_some() {
            let result_typ = inst.result_typ(ctx);
            if inst.is_phi(ctx) {
                cloned_inst = Instruction::phi(ctx, result_typ);
                let old_phi_ops = inst.get_phi(ctx);
                for (bbk, val) in old_phi_ops {
                    let cloned_bbk = bbks_map[&bbk];
                    cloned_inst.insert_phi_operand(
                        ctx,
                        cloned_bbk,
                        *params_map.get(&val).unwrap_or(&val),
                    );
                }
            } else {
                cloned_inst = Instruction::new_with_operands(ctx, kind, result_typ, opds.clone());
                // 维护def-use信息
                for (i, opd) in opds.iter().enumerate() {
                    let user = User::new(cloned_inst, i + 1);
                    opd.insert(ctx, user);
                }
                let res = cloned_inst.get_result(ctx).unwrap();
                let user = User::new(cloned_inst, 0);
                res.insert(ctx, user);
            }
            let new_res = cloned_inst.get_result(ctx).unwrap();
            params_map.insert(old_res.unwrap(), new_res);
        } else {
            if inst.is_br(ctx) {
                let cloned_bbk = bbks_map[&inst.get_operand_bbk(ctx, 0).unwrap()];
                cloned_inst = Instruction::br(ctx, cloned_bbk);
                let user = User::new(cloned_inst, 1);
                cloned_bbk.insert(ctx, user);
            } else if inst.is_cbr(ctx) {
                let cloned_bbk_true = bbks_map[&inst.get_operand_bbk(ctx, 0).unwrap()];
                let cloned_bbk_false = bbks_map[&inst.get_operand_bbk(ctx, 1).unwrap()];
                cloned_inst = Instruction::icbr(ctx, opds[0], cloned_bbk_true, cloned_bbk_false);
                let user = User::new(cloned_inst, 1);
                opds[0].insert(ctx, user);
                let user = User::new(cloned_inst, 1);
                cloned_bbk_true.insert(ctx, user);
                let user = User::new(cloned_inst, 2);
                cloned_bbk_false.insert(ctx, user);
            } else if inst.is_ret(ctx) {
                cloned_inst = Instruction::br(ctx, *cont_bbk);
                let user = User::new(cloned_inst, 1);
                cont_bbk.insert(ctx, user);
            } else {
                let void = Typ::void(ctx);
                cloned_inst = Instruction::new_with_operands(ctx, kind, void, opds.clone());
                for (i, opd) in opds.iter().enumerate() {
                    let user = User::new(cloned_inst, i + 1);
                    opd.insert(ctx, user);
                }
            }
        }
        cloned_inst
    }

    fn postorder(
        ctx: &Context,
        cfg: &ControlFlowGraph,
        bbk: &BasicBlock,
        visited: &mut HashSet<BasicBlock>,
        postorder_list: &mut Vec<BasicBlock>,
    ) {
        visited.insert(*bbk);
        for bbk in cfg.succ_set[bbk].iter() {
            if !visited.contains(bbk) {
                Self::postorder(ctx, cfg, bbk, visited, postorder_list);
            }
        }
        postorder_list.push(*bbk);
    }
}

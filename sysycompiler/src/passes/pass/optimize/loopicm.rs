/// Loop Invariant Code Motion (LICM) Pass
/// 循环不变量外提
/// 入口块最好不要外提指令，否则处理很麻烦
/// 不考虑store和call等可能有副作用的指令，其他指令外提不需要限制条件
/// 考虑限制条件的话要考虑 支配关系+不执行情况
use crate::{
    frontend::{
        ir::{
            basicblock::BasicBlock,
            context::Context,
            defuse::{Useable, User},
            function::Function,
            instruction::Instruction,
            typ::Typ,
            value::Value,
        },
        ir2string::Display,
    },
    passes::{
        pass::{self, analysis::loopa::LoopAnalysis, structure::Loop},
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::{LinkedListContainer, LinkedListNode},
};
use rustc_hash::FxHashMap as HashMap;

pub struct LICM;

impl Pass for LICM {
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
            changed |= Self::execute_function(&function, input, pctx)?;
        }
        Ok(changed)
    }

    fn name(&self) -> &'static str {
        "LICM"
    }

    fn dependencies(&self) -> &[&'static str] {
        &[
            "LoopSimplify",
            "LoopAnalysis",
            "CFAnalysis",
            "LoopElimination",
        ]
    }

    fn effects(&self) -> &[&'static str] {
        &["CFAnalysis", "DominatorRelAnalysis", "LoopAnalysis"]
    }
}

impl LICM {
    fn execute_function(
        function: &Function,
        ctx: &mut Context,
        pctx: &mut PassContext,
    ) -> Result<bool, PassError> {
        let mut changed = false;
        let loop_info = pctx.loop_info.get(&function.get_id(ctx)).unwrap();
        // 从内层到外层处理循环
        let mut loops = loop_info.loops.clone();
        loops.sort_by(|a, b| b.depth.cmp(&a.depth)); // 按深度降序（先内层）

        for loop_ in loops {
            // 获取循环的preheader（由LoopSimplify生成）
            let Some(preheader) = loop_.preheader else {
                continue;
            };

            // 执行当前循环的LICM
            changed |= Self::hoist_invariants_in_loop(ctx, function, &loop_, preheader, pctx)?;
        }

        Ok(changed)
    }

    /// 在单个循环中执行不变量外提
    fn hoist_invariants_in_loop(
        ctx: &mut Context,
        function: &Function,
        loop_: &Loop<BasicBlock>,
        preheader: BasicBlock,
        pctx: &mut PassContext,
    ) -> Result<bool, PassError> {
        let mut changed = false;
        let mut worklist_changed = true;
        let need_switch = false;
        // 多次迭代直到无新指令可外提
        while worklist_changed {
            worklist_changed = false;
            let mut invariants = Vec::new();

            // 收集循环体中所有非终结指令
            for &bbk in &loop_.body {
                if bbk == loop_.header {
                    continue;
                }
                let mut cur_inst = bbk.head(ctx);
                while let Some(inst) = cur_inst {
                    // 跳过终结指令
                    if inst.is_terminater(ctx) || inst.is_phi(ctx) {
                        cur_inst = inst.succ(ctx);
                        continue;
                    }

                    // 检查是否可安全外提
                    if LoopAnalysis::is_invariant(function, inst, ctx, loop_, pctx)
                        && !LoopAnalysis::has_side_effects(inst, ctx)
                    {
                        invariants.push(inst);
                    } else if LoopAnalysis::is_invariant(function, inst, ctx, loop_, pctx)
                        && LoopAnalysis::has_side_effects(inst, ctx)
                        && LoopAnalysis::is_dom_use(
                            &inst,
                            ctx,
                            loop_,
                            pctx.dom_info.get(&function.get_id(ctx)).unwrap(),
                        )
                        && LoopAnalysis::is_dom_exits(
                            &inst,
                            ctx,
                            loop_,
                            pctx.dom_info.get(&function.get_id(ctx)).unwrap(),
                        )
                        && !inst.is_call(ctx)
                    {
                        // invariants.push(inst);
                        // need_switch = true;
                    }
                    cur_inst = inst.succ(ctx);
                }
            }

            for inst in invariants {
                // 将指令移动到preheader末尾（跳转前）
                let terminator = preheader.tail(ctx).unwrap();
                inst.unlink(ctx);
                terminator.insert_before(ctx, inst).unwrap();
                // println!(
                //     "LICM: hoisted invariant: {}",
                //     crate::frontend::ir2string::Display::display(inst, ctx)
                // );
                changed = true;
                worklist_changed = true;
            }
        }

        if changed && need_switch {
            let mut need_new_bbk = true;
            if let Some((start_val, end_val)) =
                LoopAnalysis::get_loop_bounds(loop_, function, ctx, pctx)
            {
                if start_val.is_constant(ctx) && end_val.is_constant(ctx) {
                    need_new_bbk = false;
                }
            }

            // println!("{}", function.display(ctx));
            if need_new_bbk && loop_.header.tail(ctx).unwrap().is_cbr(ctx) {
                Self::insert_switch(loop_, function, preheader, pctx, ctx);
            }
            // println!("{}", function.display(ctx));
        }

        // 外提所有识别到的不变量
        // for inst in to_move {
        //     // 将指令移动到preheader末尾（跳转前）
        //     let terminator = preheader.tail(ctx).unwrap();
        //     inst.unlink(ctx);
        //     terminator.insert_before(ctx, inst).unwrap();
        //     // println!(
        //     //     "LICM: hoisted invariant: {}",
        //     //     crate::frontend::ir2string::Display::display(inst, ctx)
        //     // );
        //     changed = true;
        // }
        Ok(changed)
    }

    /// 在preheader之前插入开关基本块，防止某未知执行状态的循环外提代码执行导致副作用
    fn insert_switch(
        loop_: &Loop<BasicBlock>,
        function: &Function,
        preheader: BasicBlock,
        pctx: &mut PassContext,
        ctx: &mut Context,
    ) {
        // println!("LOOP HEADER: \n");
        // for inst in header_insts {
        //     println!("{}", inst.display(ctx));
        // }
        let cfg = pctx.cfginfo.get(&function.get_id(ctx)).unwrap();
        let switch_bbk = BasicBlock::new(ctx);
        for pre_bbk in cfg.cfg.get_predecessors(&preheader).unwrap() {
            let inst = pre_bbk.tail(ctx).unwrap();
            if inst.is_br(ctx) {
                inst.replace_operand_bbk(ctx, 0, switch_bbk);
            } else if inst.is_cbr(ctx) {
                let true_bbk = inst.get_operand_bbk(ctx, 0).unwrap();
                let false_bbk = inst.get_operand_bbk(ctx, 1).unwrap();
                if true_bbk == preheader {
                    inst.replace_operand_bbk(ctx, 0, switch_bbk);
                } else if false_bbk == preheader {
                    inst.replace_operand_bbk(ctx, 1, switch_bbk);
                }
            } else {
                panic!("Unexpected terminator instruction in predecessor block")
            }
        }
        let mut val_map: HashMap<Value, Value> = HashMap::default();

        let mut cur_inst = loop_.header.head(ctx);
        while let Some(inst) = cur_inst {
            if inst.is_phi(ctx) {
                let res = inst.get_result(ctx).unwrap();
                let init = inst.get_phi_operand(ctx, preheader).unwrap();
                val_map.insert(res, init);
                cur_inst = inst.succ(ctx);
                continue;
            }

            let inst_kind = inst.get_kind(ctx);
            let opds = inst
                .get_operands(ctx)
                .iter()
                .map(|opd| val_map.get(opd).copied().unwrap_or(*opd))
                .collect::<Vec<_>>();
            let old_res = inst.get_result(ctx);
            let cloned_inst;
            if old_res.is_some() {
                let result_typ = inst.result_typ(ctx);
                cloned_inst =
                    Instruction::new_with_operands(ctx, inst_kind, result_typ, opds.clone());

                // 维护def-use信息
                for (i, opd) in opds.iter().enumerate() {
                    let user = User::new(cloned_inst, i + 1);
                    opd.insert(ctx, user);
                }
                let res = cloned_inst.get_result(ctx).unwrap();
                let user = User::new(cloned_inst, 0);
                res.insert(ctx, user);

                let new_res = cloned_inst.get_result(ctx).unwrap();
                val_map.insert(old_res.unwrap(), new_res);
            } else {
                if inst.is_br(ctx) {
                    println!("Function: \n{}", function.display(ctx));
                    panic!(
                        "Unexpected branch instruction {} in loop header",
                        inst.display(ctx)
                    );
                } else if inst.is_cbr(ctx) {
                    let true_bbk = inst.get_operand_bbk(ctx, 0).unwrap();
                    let false_bbk = inst.get_operand_bbk(ctx, 1).unwrap();
                    let (cloned_bbk_true, cloned_bbk_false, exit_bbk) =
                        if loop_.body.contains(&true_bbk) {
                            (preheader, loop_.header, loop_.header)
                        } else if loop_.body.contains(&false_bbk) {
                            (loop_.header, preheader, loop_.header)
                        } else {
                            println!("Function: \n{}", function.display(ctx));
                            panic!(
                                "Unexpected branch {} target in loop header",
                                inst.display(ctx)
                            );
                        };
                    cloned_inst =
                        Instruction::icbr(ctx, opds[0], cloned_bbk_true, cloned_bbk_false);
                    let user = User::new(cloned_inst, 1);
                    opds[0].insert(ctx, user);
                    let user = User::new(cloned_inst, 1);
                    cloned_bbk_true.insert(ctx, user);
                    let user = User::new(cloned_inst, 2);
                    cloned_bbk_false.insert(ctx, user);

                    let mut phi = exit_bbk.head(ctx);
                    while phi.is_some() && phi.unwrap().is_phi(ctx) {
                        let old_val = phi.unwrap().get_phi_operand(ctx, preheader).unwrap();
                        phi.unwrap().insert_phi_operand(ctx, switch_bbk, old_val);
                        phi = phi.unwrap().succ(ctx);
                    }
                } else if inst.is_ret(ctx) {
                    panic!("Unexpected return instruction in loop header")
                } else {
                    let void = Typ::void(ctx);
                    cloned_inst =
                        Instruction::new_with_operands(ctx, inst_kind, void, opds.clone());
                    for (i, opd) in opds.iter().enumerate() {
                        let user = User::new(cloned_inst, i + 1);
                        opd.insert(ctx, user);
                    }
                }
            }
            switch_bbk.push_back(ctx, cloned_inst).unwrap();
            cur_inst = inst.succ(ctx);
        }

        preheader.insert_before(ctx, switch_bbk).unwrap();
        // println!("Switch block: \n{}", switch_bbk.display(ctx));
        // println!("Function: \n{}", function.display(ctx));
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
    }
}

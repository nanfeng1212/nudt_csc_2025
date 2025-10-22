/// Loop Unrolling Pass
/// This pass unrolls loops that have fitted and konwn conditions for unrolling.
///
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
    pass::analysis::loopa::LoopAnalysis as LoopAnalysisPass,
    passes::{
        pass::structure::{GetName, Loop, LoopInfo},
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::{LinkedListContainer, LinkedListNode},
};
use rustc_hash::FxHashMap as HashMap;

pub struct LoopUnroll;

impl Pass for LoopUnroll {
    type Output = bool;
    type Error = PassError;

    fn pass_type(&self) -> PassType {
        PassType::FunctionBasedOptimization
    }

    fn execute(&mut self, ctx: &mut Context, pctx: &mut PassContext) -> Result<bool, Self::Error> {
        let mut changed = false;
        let functions: Vec<_> = ctx.get_functions().collect();

        for function in functions {
            if let Some(loop_info) = pctx.loop_info.get(&function.get_id(ctx)) {
                changed |= self.execute_function(&function, ctx, loop_info, pctx)?;
            }
        }
        Ok(changed)
    }

    fn name(&self) -> &'static str {
        "LoopUnroll"
    }

    fn dependencies(&self) -> &[&'static str] {
        &["LoopSimplify", "LoopAnalysis", "CFAnalysis", "Mem2Reg"]
    }

    fn effects(&self) -> &[&'static str] {
        &["CFAnalysis", "DominatorAnalysis", "LoopAnalysis"]
    }
}

impl LoopUnroll {
    const MAX_UNROLLED_SIZE: u32 = 30; // 最大展开的指令数
    const MAX_BLOAT_SIZE: u32 = 60; // 允许展开的最大膨胀体积
    const MIN_UNROLL_ITER: u32 = 1000; // 最小迭次数

    const MINI_LOOP_SIZE: u32 = 5; // 最小循环体大小
    const MINI_LOOP_ITER: u32 = 10; // 最小循环迭代次数

    fn execute_function(
        &self,
        function: &Function,
        ctx: &mut Context,
        loop_info: &LoopInfo,
        pctx: &PassContext,
    ) -> Result<bool, PassError> {
        let mut changed = false;
        // 从内层到外层处理循环
        let mut loops = loop_info.loops.clone();
        loops.sort_by(|a, b| b.depth.cmp(&a.depth)); // 按深度降序（先内层）

        for loop_ in loops {
            // 检查循环是否适合展开
            // 1.上下界必须是常量
            // 2.步长必须为常量
            // 3.循环体中不能包含复杂控制流： 循环体内不能再有分支条件分支指令
            // 4.展开后指令数不能超过限制
            if Self::is_suitable_for_unrolling(&loop_, function, ctx, pctx) {
                // 计算最佳展开因子
                let unroll_factor =
                    Self::calculate_optimal_unroll_factor(&loop_, ctx, function, pctx);
                println!("Find suitable loop for unrolling");
                println!("Unroll factor: {}", unroll_factor);

                if unroll_factor > 1 {
                    changed |= self.unroll_loop(function, ctx, &loop_, unroll_factor, pctx)?;
                }
            }
        }

        Ok(changed)
    }

    /// 检查循环是否适合展开
    fn is_suitable_for_unrolling(
        loop_: &Loop<BasicBlock>,
        function: &Function,
        ctx: &Context,
        pctx: &PassContext,
    ) -> bool {
        // 检查循环体是否包含复杂控制流
        if Self::contains_complex_control_flow(loop_, ctx) {
            return false;
        }

        // 获取循环边界信息
        let (_induction_var, start_val, end_val, step_val, _latch_val) =
            match LoopAnalysisPass::find_induction_variable(loop_, function, ctx, pctx) {
                Some(info) => info,
                None => return false,
            };
        println!(
            "Loop bounds: {} -> {} (step: {})",
            start_val.display(ctx),
            end_val.display(ctx),
            step_val.display(ctx)
        );
        let cmp = loop_.header.tail(ctx).unwrap().pre(ctx).unwrap();
        // 计算迭代次数
        let trip_count =
            match LoopAnalysisPass::calculate_trip_count(start_val, end_val, step_val, &cmp, ctx) {
                Some(count) if count > Self::MIN_UNROLL_ITER || count <= Self::MINI_LOOP_ITER => {
                    count
                }
                _ => return false,
            };

        // 检查循环体大小是否合适
        let body_size = Self::calculate_loop_body_size(loop_, ctx);

        if body_size > Self::MAX_UNROLLED_SIZE as usize
            || (trip_count <= Self::MINI_LOOP_ITER && body_size > Self::MINI_LOOP_SIZE as usize)
        {
            return false;
        }

        true
    }

    /// 计算循环体大小
    fn calculate_loop_body_size(loop_: &Loop<BasicBlock>, ctx: &Context) -> usize {
        let mut size = 0;

        for &block in &loop_.body {
            // 跳过循环头（包含PHI节点）
            if block == loop_.header {
                continue;
            }

            let mut cur_inst = block.head(ctx);
            while let Some(inst) = cur_inst {
                size += 1;
                cur_inst = inst.succ(ctx);
            }
        }

        size
    }

    /// 检查循环体是否包含复杂控制流
    fn contains_complex_control_flow(loop_: &Loop<BasicBlock>, ctx: &Context) -> bool {
        for &block in &loop_.body {
            if block == loop_.header {
                continue;
            }

            let terminator = block.tail(ctx).unwrap();
            if terminator.is_cbr(ctx) {
                // 循环体内有分支，可能不适合展开
                return true;
            }
        }

        false
    }

    /// 计算最佳展开因子
    /// 只要可以整除上下界的，不能整除就不进行展开
    fn calculate_optimal_unroll_factor(
        loop_: &Loop<BasicBlock>,
        ctx: &Context,
        function: &Function,
        pctx: &PassContext,
    ) -> u32 {
        let body_size = Self::calculate_loop_body_size(loop_, ctx);
        let (_, start, end, step, _) =
            match LoopAnalysisPass::find_induction_variable(loop_, function, ctx, pctx) {
                Some(info) => info,
                None => panic!("Loop bounds not found"),
            };
        let cmp = loop_.header.tail(ctx).unwrap().pre(ctx).unwrap();
        let trip_count =
            LoopAnalysisPass::calculate_trip_count(start, end, step, &cmp, ctx).unwrap() as u32;
        if trip_count <= Self::MINI_LOOP_ITER && body_size <= Self::MINI_LOOP_SIZE as usize {
            return trip_count;
        }

        let mut factor = (Self::MAX_BLOAT_SIZE / body_size as u32).max(1).min(8) as u32;
        println!("Trip count: {}", trip_count);
        println!("Body size: {}", body_size);
        if trip_count % factor == 0 {
            return factor;
        }
        while factor >= 1 {
            factor -= 1;
            if trip_count % factor == 0 {
                return factor;
            }
        }
        1
    }

    /// 执行循环展开
    fn unroll_loop(
        &self,
        function: &Function,
        ctx: &mut Context,
        loop_: &Loop<BasicBlock>,
        unroll_factor: u32,
        pctx: &PassContext,
    ) -> Result<bool, PassError> {
        let latch_bbk = LoopAnalysisPass::find_loop_latch(
            loop_,
            pctx.cfginfo.get(&function.get_id(ctx)).unwrap(),
        )
        .unwrap();

        // 创建展开后的循环体
        self.create_unrolled_body(function, ctx, pctx, loop_, unroll_factor, latch_bbk);

        Ok(true)
    }

    /// 创建展开后的循环体
    fn create_unrolled_body(
        &self,
        function: &Function,
        ctx: &mut Context,
        pctx: &PassContext,
        loop_: &Loop<BasicBlock>,
        unroll_factor: u32,
        latch_bbk: BasicBlock,
    ) {
        let cfg = pctx.cfginfo.get(&function.get_id(ctx)).unwrap();
        let mut val_map = HashMap::default();
        let mut bbk_map = HashMap::default();
        let mut bodys = Vec::new();
        let mut cur_bbk = Some(loop_.header);
        while let Some(bbk) = cur_bbk {
            if bbk != loop_.header {
                bodys.push(bbk);
            }
            bbk_map.insert(bbk, bbk);
            let mut cur_inst = bbk.head(ctx);
            while let Some(inst) = cur_inst {
                if let Some(val) = inst.get_result(ctx) {
                    val_map.insert(val, val);
                }
                cur_inst = inst.succ(ctx);
            }
            let succs = cfg.cfg.get_successors(&bbk).unwrap();
            if bbk != loop_.header && succs.len() != 1 {
                panic!("Complicated control flow in loop body")
            } else if bbk == loop_.header {
                for succ in succs {
                    if loop_.body.contains(&succ) {
                        cur_bbk = Some(*succ);
                    }
                }
            } else {
                let succ = succs.iter().next().unwrap();
                if *succ == loop_.header {
                    cur_bbk = None;
                } else {
                    cur_bbk = succs.iter().next().cloned();
                }
            }
        }

        let mut insert_pos = bodys[bodys.len() - 1];
        for i in 1..unroll_factor {
            for (j, bbk) in bodys.iter().enumerate() {
                let new_bbk = BasicBlock::new(ctx);
                insert_pos.insert_after(ctx, new_bbk).unwrap();
                if j == 0 {
                    let br = insert_pos.tail(ctx).unwrap();
                    br.replace_operand_bbk(ctx, 0, new_bbk);
                }
                insert_pos = new_bbk;
                bbk_map.insert(*bbk, new_bbk);
            }
            for bbk in bodys.iter() {
                let new_bbk = bbk_map[bbk];
                let mut cur_inst = bbk.head(ctx);
                while let Some(inst) = cur_inst {
                    let cloned_inst = Self::clone_inst(ctx, inst, &mut val_map, &bbk_map);
                    new_bbk.push_back(ctx, cloned_inst).unwrap();
                    cur_inst = inst.succ(ctx);
                }
            }
            if i == unroll_factor - 1 {
                let br = insert_pos.tail(ctx).unwrap();
                br.replace_operand_bbk(ctx, 0, loop_.header);
            }
            println!("Function: {}", function.display(ctx));
        }

        let mut cur_inst = loop_.header.head(ctx);
        while let Some(inst) = cur_inst {
            if inst.is_phi(ctx) {
                let old_value = inst.get_phi_operand(ctx, latch_bbk).unwrap();
                inst.replace_phi_operand(ctx, latch_bbk, bbk_map[&latch_bbk], val_map[&old_value]);
            }
            cur_inst = inst.succ(ctx);
        }
    }

    fn clone_inst(
        ctx: &mut Context,
        inst: Instruction,
        params_map: &mut HashMap<Value, Value>,
        bbks_map: &HashMap<BasicBlock, BasicBlock>,
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
                println!(
                    "Find a br instruction in loop body, bbk {}",
                    inst.get_operand_bbk(ctx, 0).unwrap().get_name(ctx)
                );
                let old_bbk = inst.get_operand_bbk(ctx, 0).unwrap();
                let cloned_bbk = bbks_map.get(&old_bbk).unwrap_or(&old_bbk);
                cloned_inst = Instruction::br(ctx, *cloned_bbk);
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
                // cloned_inst = Instruction::br(ctx, *cont_bbk);
                // let user = User::new(cloned_inst, 1);
                // cont_bbk.insert(ctx, user);
                panic!("Finad a ret instruction in loop body");
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
}

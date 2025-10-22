/// Loop Elimination
/// 循环消除，检查循环的上下界消除不会进入的循环体
use crate::{
    frontend::{
        ir::{
            basicblock::BasicBlock,
            context::Context,
            function::Function,
            instruction::{ICompCond, InstructionKind},
        },
        ir2string::Display,
    },
    passes::{
        pass::{
            analysis::loopa::LoopAnalysis,
            structure::{Loop, LoopInfo},
        },
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::LinkedListContainer,
};

pub struct LoopElimination;

impl Pass for LoopElimination {
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
            if let Some(loop_info) = pctx.loop_info.get(&function.get_id(input)) {
                changed |= Self::execute_function(&function, input, loop_info, pctx)?;
            }
        }
        Ok(changed)
    }

    fn name(&self) -> &'static str {
        "LoopElimination"
    }

    fn dependencies(&self) -> &[&'static str] {
        &["LoopSimplify", "LoopAnalysis", "CFAnalysis"]
    }

    fn effects(&self) -> &[&'static str] {
        &["CFAnalysis", "DominatorRelAnalysis", "LoopAnalysis"]
    }
}

impl LoopElimination {
    fn execute_function(
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
            if Self::is_dead_loop(ctx, function, &loop_, pctx) {
                // 执行当前循环的循环消除
                Self::eliminate_dead_loop(ctx, &loop_);
                changed = true;
            }
        }
        Ok(changed)
    }

    fn is_dead_loop(
        ctx: &mut Context,
        function: &Function,
        loop_: &Loop<BasicBlock>,
        pctx: &PassContext,
    ) -> bool {
        let mut is_dead = false; // 是否循环体中所有指令都已被外提，循环不变量已被消除
        if let Some((start_val, end_val)) =
            LoopAnalysis::get_loop_bounds(loop_, function, ctx, pctx)
        {
            // println!(
            //     "Loop Header [{}] has start value [{}] and end value [{}]",
            //     loop_.header.display(ctx),
            //     start_val.display(ctx),
            //     end_val.display(ctx)
            // );
            if start_val.get_int_const_value(ctx).is_some()
                && end_val.get_int_const_value(ctx).is_some()
            {
                let cbr = loop_.header.tail(ctx).unwrap();
                let true_exit = loop_.body.contains(&cbr.get_operand_bbk(ctx, 0).unwrap());

                let cmp = cbr
                    .get_operand(ctx, 0)
                    .unwrap()
                    .get_instruction(ctx)
                    .unwrap();
                let (l_val, r_val) = if end_val == cmp.get_operand(ctx, 0).unwrap() {
                    (
                        end_val.get_int_const_value(ctx).unwrap(),
                        start_val.get_int_const_value(ctx).unwrap(),
                    )
                } else if end_val == cmp.get_operand(ctx, 1).unwrap() {
                    (
                        start_val.get_int_const_value(ctx).unwrap(),
                        end_val.get_int_const_value(ctx).unwrap(),
                    )
                } else {
                    panic!(
                        "Loop Header [{}] has wrong cmp [{}] and end value [{}]",
                        loop_.header.display(ctx),
                        cmp.display(ctx),
                        end_val.display(ctx),
                    );
                };

                if !cbr.is_cbr(ctx) {
                    panic!("Loop header should have a terminator instruction");
                } else {
                    if let InstructionKind::IComp { cond } = cmp.get_kind(ctx) {
                        let cmp_res = match cond {
                            ICompCond::Eq => l_val == r_val,
                            ICompCond::Ne => l_val != r_val,
                            ICompCond::Ugt => l_val > r_val,
                            ICompCond::Uge => l_val >= r_val,
                            ICompCond::Ult => l_val < r_val,
                            ICompCond::Ule => l_val <= r_val,
                            ICompCond::Sgt => l_val > r_val,
                            ICompCond::Sge => l_val >= r_val,
                            ICompCond::Slt => l_val < r_val,
                            ICompCond::Sle => l_val <= r_val,
                        };
                        is_dead = (true_exit && cmp_res) || (!true_exit && !cmp_res);
                    }
                }
            }

            if start_val.get_float_const_value(ctx).is_some()
                && end_val.get_float_const_value(ctx).is_some()
            {
                let cbr = loop_.header.tail(ctx).unwrap();
                let true_exit = loop_.body.contains(&cbr.get_operand_bbk(ctx, 1).unwrap());

                let cmp = cbr
                    .get_operand(ctx, 0)
                    .unwrap()
                    .get_instruction(ctx)
                    .unwrap();
                let (l_val, r_val) = if end_val == cmp.get_operand(ctx, 0).unwrap() {
                    (
                        end_val.get_float_const_value(ctx).unwrap(),
                        start_val.get_float_const_value(ctx).unwrap(),
                    )
                } else if end_val == cmp.get_operand(ctx, 1).unwrap() {
                    (
                        start_val.get_float_const_value(ctx).unwrap(),
                        end_val.get_float_const_value(ctx).unwrap(),
                    )
                } else {
                    panic!(
                        "Loop Header [{}] has wrong cmp [{}] and end value [{}]",
                        loop_.header.display(ctx),
                        cmp.display(ctx),
                        end_val.display(ctx),
                    );
                };

                if !cbr.is_cbr(ctx) {
                    panic!("Loop header should have a terminator instruction");
                } else {
                    if let InstructionKind::FComp { cond } = cmp.get_kind(ctx) {
                        let cmp_res = match cond {
                            crate::frontend::ir::instruction::FCompCond::False => false,
                            crate::frontend::ir::instruction::FCompCond::True => true,
                            crate::frontend::ir::instruction::FCompCond::Oeq => l_val == r_val,
                            crate::frontend::ir::instruction::FCompCond::One => l_val != r_val,
                            crate::frontend::ir::instruction::FCompCond::Ogt => l_val > r_val,
                            crate::frontend::ir::instruction::FCompCond::Oge => l_val >= r_val,
                            crate::frontend::ir::instruction::FCompCond::Olt => l_val < r_val,
                            crate::frontend::ir::instruction::FCompCond::Ole => l_val <= r_val,
                            crate::frontend::ir::instruction::FCompCond::Ord => {
                                panic!("Ordered comparison is not supported")
                            }
                            crate::frontend::ir::instruction::FCompCond::Ueq => l_val == r_val,
                            crate::frontend::ir::instruction::FCompCond::Une => l_val != r_val,
                            crate::frontend::ir::instruction::FCompCond::Ugt => l_val > r_val,
                            crate::frontend::ir::instruction::FCompCond::Uge => l_val >= r_val,
                            crate::frontend::ir::instruction::FCompCond::Ult => l_val < r_val,
                            crate::frontend::ir::instruction::FCompCond::Ule => l_val <= r_val,
                            crate::frontend::ir::instruction::FCompCond::Uno => {
                                panic!("Unordered comparison is not supported")
                            }
                        };
                        is_dead = (true_exit && cmp_res) || (!true_exit && !cmp_res);
                    }
                }
            }
        }
        is_dead
    }

    fn eliminate_dead_loop(ctx: &mut Context, loop_: &Loop<BasicBlock>) {
        let cbr = loop_.header.tail(ctx).unwrap();
        let true_exit = loop_.body.contains(&cbr.get_operand_bbk(ctx, 0).unwrap());
        let bbk_remain = if true_exit {
            cbr.get_operand_bbk(ctx, 1).unwrap()
        } else {
            cbr.get_operand_bbk(ctx, 0).unwrap()
        };
        cbr.remove(ctx);
        let br = crate::frontend::ir::instruction::Instruction::br(ctx, bbk_remain);
        loop_.header.push_back(ctx, br).unwrap();
    }
}

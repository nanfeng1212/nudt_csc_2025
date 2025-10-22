use rustc_hash::FxHashSet as HashSet;

use crate::{
    frontend::ir::{
        context::Context, defuse::Useable, function::Function, instruction::Instruction,
        value::Value,
    },
    passes::{
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::LinkedListContainer,
};

pub struct LiveInstAnalysis;

impl Pass for LiveInstAnalysis {
    type Output = bool;
    type Error = PassError;

    fn pass_type(&self) -> PassType {
        PassType::FunctionBasedAnalysis
    }

    /// 执行遍处理
    fn execute(
        &mut self,
        input: &mut Context,
        pctx: &mut PassContext,
    ) -> Result<bool, Self::Error> {
        pctx.live_insts.clear();
        let mut changed = false;
        let functions: Vec<_> = input.get_functions().collect(); // 收集所有函数
        for function in functions {
            let res = self.execute_function(&function, input, pctx);
            match res {
                Ok((tmp, live_vars)) => {
                    changed |= tmp;
                    pctx.live_insts.extend(live_vars);
                }
                Err(e) => return Err(e),
            }
        }
        Ok(changed)
    }

    /// 获取遍名称（用于依赖管理）
    fn name(&self) -> &'static str {
        "LiveInstAnalysis"
    }

    /// 声明依赖的遍
    fn dependencies(&self) -> &[&'static str] {
        &["CFAnalysis"] // 依赖于控制流分析
    }

    fn effects(&self) -> &[&'static str] {
        &[]
    }
}

impl LiveInstAnalysis {
    fn execute_function(
        &mut self,
        function: &Function,
        ctx: &mut Context,
        pctx: &mut PassContext,
    ) -> Result<(bool, HashSet<Instruction>), PassError> {
        let mut live_insts = HashSet::default();

        // 获取控制流分析结果
        let cfa_result = pctx
            .cfginfo
            .get(&function.get_id(ctx))
            .ok_or(PassError::MissingCFG)?;
        let cfg = &cfa_result.cfg;

        for bbks in cfg.blocks.iter() {
            for inst in bbks.iter(ctx).rev() {
                // println!(
                //     "Instruction: {}",
                //     crate::frontend::ir2string::Display::display(inst, ctx)
                // );
                if live_insts.contains(&inst) {
                    continue;
                }
                if inst.is_terminater(ctx) {
                    // println!(
                    //     "Terminator: {}",
                    //     crate::frontend::ir2string::Display::display(inst, ctx)
                    // );
                    if live_insts.insert(inst.clone()) {
                        for opd in inst.get_operands(ctx) {
                            self.backtrace(&opd, ctx, &mut live_insts);
                        }
                    }
                } else if inst.is_store(ctx) {
                    let opd2 = inst.get_operands(ctx)[1];
                    if opd2.is_global_ptr(ctx) {
                        // use crate::frontend::ir2string::Display;
                        // println!("Store: {}", inst.display(ctx));
                        if live_insts.insert(inst.clone()) {
                            // changed = true;
                            self.backtrace(&opd2, ctx, &mut live_insts);
                            let opd1 = inst.get_operands(ctx)[0];
                            self.backtrace(&opd1, ctx, &mut live_insts);
                        }
                    } else {
                        for user in opd2.users(ctx) {
                            if user.get_instruction().is_gep(ctx) {
                                if live_insts.insert(inst.clone()) {
                                    // changed = true;
                                    for opd in inst.get_operands(ctx) {
                                        self.backtrace(&opd, ctx, &mut live_insts);
                                    }
                                    if let Some(res) = inst.get_result(ctx) {
                                        self.backtrace(&res, ctx, &mut live_insts);
                                    }
                                }
                            }
                        }
                    }
                } else if inst.is_call(ctx) {
                    // println!("Call: {}", crate::frontend::ir2string::Display::display(inst, ctx));
                    if live_insts.insert(inst.clone()) {
                        for opd in inst.get_operands(ctx) {
                            // println!("Operand: {}", crate::frontend::ir2string::Display::display(opd, ctx));
                            self.backtrace(&opd, ctx, &mut live_insts);
                        }
                    }
                }
            }
        }
        Ok((false, live_insts))
    }

    fn _backtrace(&self, value: &Value, ctx: &Context, live_insts: &mut HashSet<Instruction>) {
        for user in value.users(ctx) {
            let inst = user.get_instruction();
            // println!("Value: {} -> User: {}", crate::frontend::ir2string::Display::display(*value, ctx), crate::frontend::ir2string::Display::display(inst, ctx));
            if live_insts.insert(inst) {
                for opd in inst.get_operands(ctx) {
                    let _ = self.backtrace(&opd, ctx, live_insts);
                }
                if let Some(res) = inst.get_result(ctx) {
                    let _ = self.backtrace(&res, ctx, live_insts);
                }
            }
        }
    }

    fn backtrace(&self, value: &Value, ctx: &Context, live_insts: &mut HashSet<Instruction>) {
        use std::collections::VecDeque;

        // 使用队列存储待处理的Value
        let mut queue = VecDeque::new();
        queue.push_back(value.clone());

        // 使用HashSet跟踪已处理的指令，避免重复处理
        let mut processed_insts = HashSet::default();

        while let Some(current_value) = queue.pop_front() {
            for user in current_value.users(ctx) {
                let inst = user.get_instruction();
                if inst.is_gep(ctx) {
                    // gep指令需要保守处理，保证对数组的定值不会被删除
                    // 如果指令尚未处理且成功添加到活跃指令集
                    if !processed_insts.contains(&inst) && live_insts.insert(inst) {
                        processed_insts.insert(inst);

                        // 添加操作数到处理队列
                        for opd in inst.get_operands(ctx) {
                            queue.push_back(opd.clone());
                        }

                        // 添加结果值到处理队列
                        if let Some(res) = inst.get_result(ctx) {
                            queue.push_back(res.clone());
                        }
                    }
                } else if user.get_index() == 0 || (user.get_index() == 2 && inst.is_store(ctx)) {
                    // 如果指令尚未处理且成功添加到活跃指令集
                    if !processed_insts.contains(&inst) && live_insts.insert(inst) {
                        processed_insts.insert(inst);

                        // 添加操作数到处理队列
                        for opd in inst.get_operands(ctx) {
                            queue.push_back(opd.clone());
                        }
                    }
                }
            }
        }
    }
}

use crate::{
    frontend::ir::{
        basicblock::BasicBlock, context::Context, function::Function, instruction::Instruction,
    },
    passes::{
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::{LinkedListContainer, LinkedListNode},
};
use core::panic;
use rustc_hash::FxHashMap as HashMap;

pub struct TailCallOptimization;

impl Pass for TailCallOptimization {
    type Output = bool;
    type Error = PassError;

    fn pass_type(&self) -> PassType {
        PassType::FunctionBasedOptimization
    }

    fn execute(
        &mut self,
        ctx: &mut Context,
        pctx: &mut PassContext,
    ) -> Result<Self::Output, Self::Error> {
        let mut modified = false;

        // 获取函数列表（克隆以避免借用问题）
        let functions: Vec<Function> = ctx.get_functions().collect();

        for func in functions {
            let f_modified = Self::try_optimize(ctx, pctx, func);
            modified |= f_modified.unwrap();
        }

        Ok(modified)
    }

    fn name(&self) -> &'static str {
        "TailCallOptimization"
    }

    fn dependencies(&self) -> &[&'static str] {
        &["FunctionCallAnalysis", "LoopAnalysis"]
    }

    fn effects(&self) -> &[&'static str] {
        &["CFAnalysis", "DominatorRelAnalysis", "FunctionCallAnalysis"]
    }
}

impl TailCallOptimization {
    /// 尝试优化函数
    fn try_optimize(
        ctx: &mut Context,
        pctx: &mut PassContext,
        func: Function,
    ) -> Result<bool, PassError> {
        let mut modified = false;
        let cfg = &pctx.cfginfo.get(&func.get_id(ctx)).unwrap().cfg;
        let exit_bbk = cfg.get_exit_bbks()[0];
        let pre_ret_bbks = cfg.get_predecessors(&exit_bbk).unwrap().clone();
        let mut tail_recursion = Vec::new();
        let mut tail_calls = Vec::new();
        for bbk in pre_ret_bbks {
            let inst = bbk.get_tail(ctx).unwrap().pre(ctx);
            if inst.is_some() && inst.unwrap().is_call(ctx) {
                let inst = inst.unwrap();
                if Self::is_call_self(ctx, &func, &inst) && Self::is_tail_position(ctx, &inst) {
                    tail_recursion.push(inst);
                } else if Self::is_tail_call(ctx, &inst) && Self::is_tail_position(ctx, &inst) {
                    tail_calls.push(inst);
                }
            } else {
                continue;
            }
        }

        #[cfg(debug_assertions)]
        if !tail_recursion.is_empty() {
            println!(
                "···[TCO] Function `{}` has tail recursion",
                func.get_id(ctx)
            );
        }
        #[cfg(debug_assertions)]
        if !tail_calls.is_empty() {
            println!("···[TCO] Function `{}` has tail calls", func.get_id(ctx));
        }

        modified |= Self::optimize_tail_recursion(ctx, &func, &tail_recursion);
        modified |= Self::optimize_tail_calls(ctx, &mut tail_calls);
        Ok(modified)
    }

    /// 判断call指令是否是调用自己
    fn is_call_self(ctx: &Context, func: &Function, call: &Instruction) -> bool {
        if call.is_call(ctx) {
            let func_name = func.get_id(ctx);
            let call_name = call
                .get_operand(ctx, 0)
                .unwrap()
                .get_func_name(ctx)
                .unwrap();
            if call_name == func_name {
                return true;
            }
            false
        } else {
            panic!("[TCO] Function `is_tail_recursive` must use with call instruction")
        }
    }

    /// 判断指令是否是尾调用
    fn is_tail_call(ctx: &Context, inst: &Instruction) -> bool {
        return inst.is_call(ctx) && Self::is_tail_position(ctx, inst);
    }

    /// 检查指令是否位于尾部位置（函数最后的返回点）
    fn is_tail_position(ctx: &Context, inst: &Instruction) -> bool {
        let succ = inst.succ(ctx).unwrap();
        let tar_bbk = succ.get_operand_bbk(ctx, 0).unwrap();
        if succ.is_br(ctx) && tar_bbk.is_ret(ctx) {
            return true;
        } else if succ.is_cbr(ctx) {
            panic!(
                "Call is follwed by conditional branch: \n{}",
                crate::frontend::ir2string::Display::display(
                    succ.get_basicblock(ctx).unwrap(),
                    ctx
                )
            );
        } else {
            false
        }
    }

    /// 优化尾递归：将尾递归转换为循环
    fn optimize_tail_recursion(
        ctx: &mut Context,
        func: &Function,
        tail_recursion: &Vec<Instruction>,
    ) -> bool {
        if tail_recursion.is_empty() {
            return false;
        }

        let loop_header = func.head(ctx).unwrap();
        // 创建新的入口基本块
        let entry_bbk = BasicBlock::new(ctx);
        // 将新的基本块插入到函数中
        loop_header.insert_before(ctx, entry_bbk).unwrap();

        let mut cur_inst = loop_header.head(ctx);
        while let Some(inst) = cur_inst {
            if inst.is_alloca(ctx) {
                // 将alloca指令移动到循环头
                inst.unlink(ctx);
                entry_bbk.push_back(ctx, inst).unwrap();
            } else {
                break;
            }
            cur_inst = inst.succ(ctx);
        }

        // 从新入口块跳到循环头
        let br = Instruction::br(ctx, loop_header);
        entry_bbk.push_back(ctx, br).unwrap();

        // 在循环头中添加PHI节点
        let mut new_phis = HashMap::default();
        for (idx, param) in func.get_parameters(ctx).iter().enumerate() {
            let typ = param.kind(ctx);
            let phi = Instruction::phi(ctx, typ);
            loop_header.push_front(ctx, phi).unwrap();
            new_phis.insert(idx, phi);
            let res = phi.get_result(ctx).unwrap();
            param.replace_all_uses_with(ctx, res);
            phi.insert_phi_operand(ctx, entry_bbk, *param);
        }

        let ret_phi = func.tail(ctx).unwrap().head(ctx).unwrap();
        // 处理每个基本块中的尾递归调用
        for call_inst in tail_recursion {
            let bbk = call_inst.get_basicblock(ctx).unwrap();
            // 获取调用参数
            let args: Vec<_> = (1..call_inst.operand_count(ctx))
                .filter_map(|i| call_inst.get_operand(ctx, i))
                .collect();

            // 更新PHI节点的输入
            for (idx, arg) in args.iter().enumerate() {
                if let Some(phi) = new_phis.get(&idx) {
                    phi.insert_phi_operand(ctx, bbk, *arg);
                }
            }

            // 将尾调用替换为跳转到循环头
            let br = call_inst.succ(ctx).unwrap();
            br.replace_operand_bbk(ctx, 0, loop_header);
            if ret_phi.is_phi(ctx) {
                ret_phi.remove_phi_operand(ctx, bbk);
            }
            call_inst.remove(ctx);
        }
        true
    }

    /// 优化普通尾调用（非递归）
    fn optimize_tail_calls(ctx: &mut Context, tail_calls: &mut Vec<Instruction>) -> bool {
        if tail_calls.is_empty() {
            return false;
        }

        for call_inst in tail_calls.iter_mut() {
            call_inst.set_tail_call(ctx, true);
        }

        true
    }
}

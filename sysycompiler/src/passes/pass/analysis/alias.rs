/// Alias Analysis Pass
/// 别名分析遍
/// 分析每个变量的别名关系
use crate::{
    frontend::ir::{context::Context, value::Value},
    passes::{
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
};

pub struct AliasAnalysis;

impl Pass for AliasAnalysis {
    type Output = bool;
    type Error = PassError;

    fn pass_type(&self) -> PassType {
        PassType::FunctionBasedAnalysis
    }

    /// 执行遍处理
    fn execute(
        &mut self,
        _input: &mut Context,
        _pctx: &mut PassContext,
    ) -> Result<bool, Self::Error> {
        Ok(false)
    }

    /// 获取遍名称（用于依赖管理）
    fn name(&self) -> &'static str {
        "AliasAnalysis"
    }

    /// 声明依赖的遍
    fn dependencies(&self) -> &[&'static str] {
        &[]
    }

    /// 声明产生的影响
    fn effects(&self) -> &[&'static str] {
        &[]
    }
}

impl AliasAnalysis {
    /// 判断两个变量(要是地址)是否是别名关系
    pub fn is_alias(ctx: &Context, var1: Value, var2: Value) -> bool {
        if var1 == var2 {
            return true;
        }
        if var1.is_constant(ctx) && var2.is_constant(ctx) {
            return ((var1.get_int_const_value(ctx) == var2.get_int_const_value(ctx)
                && !var1.get_int_const_value(ctx).is_none())
                || (var1.get_float_const_value(ctx) == var2.get_float_const_value(ctx)
                    && !var1.get_float_const_value(ctx).is_none())
                || (var1.get_bool_const_value(ctx) == var2.get_bool_const_value(ctx)
                    && !var1.get_bool_const_value(ctx).is_none()))
                && var1.kind(ctx) == var2.kind(ctx);
        } else if var1.is_parameter(ctx) && var2.is_parameter(ctx) {
            return var1 == var2;
        } else if var1.is_function(ctx) && var2.is_function(ctx) {
            return var1.get_func_name(ctx) == var2.get_func_name(ctx);
        } else if var1.get_instruction(ctx).is_some() && var2.get_instruction(ctx).is_some() {
            let def1 = var1.get_instruction(ctx).unwrap();
            let def2 = var2.get_instruction(ctx).unwrap();
            if def1.get_kind(ctx) == def2.get_kind(ctx) && !def1.is_phi(ctx) {
                let ops1 = def1.get_operands(ctx);
                let ops2 = def2.get_operands(ctx);
                let mut alias = !ops1.is_empty();
                for (op1, op2) in ops1.iter().zip(ops2.iter()) {
                    alias &= Self::is_alias(ctx, *op1, *op2);
                }
                return alias;
            } else if def1.is_phi(ctx) && def2.is_phi(ctx) {
            }
        }
        false
    }
}

use super::context::Context;
use super::typ::Typ;
use super::value::ConstantValue;
use crate::utils::storage::{Arena, ArenaPtr, GenericPtr};

pub struct GlobalData {
    pub self_ptr: Global,
    name: String,
    value: ConstantValue,
}

impl GlobalData {
    pub fn get_self_ptr(&self) -> Global {
        self.self_ptr
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Copy)]
pub struct Global(pub GenericPtr<GlobalData>);

impl ArenaPtr for Global {
    type Arena = Context;
    type Data = GlobalData;
}

impl Global {
    /// 创建一个全局变量
    pub fn new(ctx: &mut Context, name: String, value: ConstantValue) -> Self {
        ctx.alloc_with(|self_ptr| GlobalData {
            self_ptr,
            name,
            value,
        })
    }

    /// 获取全局变量的名称
    pub fn name(self, ctx: &Context) -> &str {
        &self
            .deref(ctx)
            .expect("Failed to deref `name` of struct Global")
            .name
    }

    /// 获取全局变量的值
    pub fn value(self, ctx: &Context) -> &ConstantValue {
        &self
            .deref(ctx)
            .expect("Failed to deref `value` of struct Global")
            .value
    }

    /// 获取全局变量的类型
    pub fn typ(self, ctx: &Context) -> Typ {
        self.value(ctx).typ()
    }
}

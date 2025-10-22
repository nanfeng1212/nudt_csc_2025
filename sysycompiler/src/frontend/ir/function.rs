use super::basicblock::BasicBlock;
use super::value::Value;
use super::{context::Context, typ::Typ};
use crate::utils::linked_list::LinkedListContainer;
use crate::utils::storage::{Arena, ArenaPtr, GenericPtr};

pub struct FunctionData {
    self_ptr: Function,
    id: String,
    parameters: Vec<Value>,
    return_type: Typ,
    head: Option<BasicBlock>,
    tail: Option<BasicBlock>,
}

impl FunctionData {
    pub fn get_self_ptr(&self) -> Function {
        *(&self.self_ptr)
    }
}

#[derive(Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct Function(pub GenericPtr<FunctionData>);

impl crate::passes::pass::structure::GetName for Function {
    fn get_name(&self, ctx: &Context) -> String {
        format!("{}", self.get_id(ctx))
    }
}

impl Function {
    /// 创建一个新的函数
    pub fn new(ctx: &mut Context, id: String, return_type: Typ) -> Self {
        ctx.alloc_with(|self_ptr| FunctionData {
            self_ptr,
            id,
            parameters: vec![],
            return_type,
            head: None,
            tail: None,
        })
    }

    /// 获取函数名
    pub fn get_id(self, ctx: &Context) -> String {
        self.deref(ctx)
            .expect("Failed to deref `functions` in struct Context")
            .id
            .clone()
    }

    /// 获取函数的返回类型
    pub fn get_return_type(self, ctx: &Context) -> Typ {
        self.deref(ctx)
            .expect("Failed to deref `functions` in struct Context")
            .return_type
    }

    /// 获取函数的参数列表
    pub fn get_parameters(self, ctx: &Context) -> Vec<Value> {
        self.deref(ctx)
            .expect("Failed to deref `functions` in struct Context")
            .parameters
            .clone()
    }

    /// 添加一个参数到函数参数列表中
    pub fn add_parameter(self, ctx: &mut Context, typ: Typ) -> Value {
        let parameter = Value::parameter(
            ctx,
            self,
            self.deref(ctx)
                .expect("Failed to deref `functions` in struct Context")
                .parameters
                .len() as u32,
            typ,
        );
        let function = self
            .deref_mut(ctx)
            .expect("Failed to deref `functions` in struct Context");
        function.parameters.push(parameter);
        parameter
    }
}

impl ArenaPtr for Function {
    type Data = FunctionData;
    type Arena = Context;
}

impl LinkedListContainer<BasicBlock> for Function {
    type Ctx = Context;

    fn head(self, ctx: &Self::Ctx) -> Option<BasicBlock> {
        self.deref(ctx)
            .expect("Failed to deref `functions` in struct Context")
            .head
    }

    fn tail(self, ctx: &Self::Ctx) -> Option<BasicBlock> {
        self.deref(ctx)
            .expect("Failed to deref `functions` in struct Context")
            .tail
    }

    fn set_head(self, ctx: &mut Self::Ctx, head: Option<BasicBlock>) {
        self.deref_mut(ctx)
            .expect("Failed to deref `functions` in struct Context")
            .head = head;
    }

    fn set_tail(self, ctx: &mut Self::Ctx, tail: Option<BasicBlock>) {
        self.deref_mut(ctx)
            .expect("Failed to deref `functions` in struct Context")
            .tail = tail;
    }
}

use super::context::Context;
use super::defuse::{Useable, User};
use super::function::Function;
use super::instruction::Instruction;
use crate::utils::linked_list::{LinkedListContainer, LinkedListNode};
use crate::utils::storage::{Arena, ArenaPtr, GenericPtr, Idx};
use rustc_hash::FxHashSet as HashSet;

pub struct BasicBlockData {
    self_ptr: BasicBlock,

    // 基本块的首支令和尾指令
    head: Option<Instruction>,
    tail: Option<Instruction>,
    users: HashSet<User<BasicBlock>>,
    function: Option<Function>,

    // 用于LinkedListNode实现，是基本块放入Function的顺序，并非实际的前驱和后继关系
    pre: Option<BasicBlock>,
    succ: Option<BasicBlock>,
}

impl BasicBlockData {
    pub fn get_self_ptr(&self) -> BasicBlock {
        self.self_ptr
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialOrd, Ord, PartialEq, Eq)]
pub struct BasicBlock(pub GenericPtr<BasicBlockData>);

impl crate::passes::pass::structure::GetName for BasicBlock {
    fn get_name(&self, _ctx: &Context) -> String {
        format!("bb_{}", self.0.index())
    }
}

impl BasicBlock {
    pub fn new(ctx: &mut Context) -> Self {
        ctx.alloc_with(|self_ptr| BasicBlockData {
            self_ptr: self_ptr,
            head: None,
            tail: None,
            pre: None,
            succ: None,
            users: HashSet::default(),
            function: None,
        })
    }

    pub fn get_head(self, ctx: &Context) -> Option<Instruction> {
        self.deref(ctx)
            .expect("Failed to deref `basicblocks` in struct Context")
            .head
    }

    pub fn get_tail(self, ctx: &Context) -> Option<Instruction> {
        self.deref(ctx)
            .expect("Failed to deref `basicblocks` in struct Context")
            .tail
    }

    pub fn is_entry(self, ctx: &Context) -> bool {
        self.deref(ctx)
            .expect("Failed to deref `basicblocks` in struct Context")
            .pre
            .is_none()
    }

    pub fn is_terminated(self, ctx: &Context) -> bool {
        self.deref(ctx)
            .expect("Failed to deref `basicblocks` in struct Context")
            .tail
            .map_or(false, |tail| tail.is_terminater(ctx))
    }

    pub fn is_ret(self, ctx: &Context) -> bool {
        self.deref(ctx)
            .expect("Failed to deref `basicblocks` in struct Context")
            .tail
            .map_or(false, |tail| tail.is_ret(ctx))
    }

    pub fn get_function(self, ctx: &Context) -> Option<Function> {
        self.deref(ctx)
            .expect("Failed to deref `basicblocks` in struct Context")
            .function
    }

    /// 移除基本块，移除前会检查使用者，存在则拒绝删除
    pub fn remove(self, ctx: &mut Context) -> bool {
        let user_count = self.users(ctx).into_iter().count();
        if user_count == 0 {
            let wait_to_remove: Vec<Instruction> = self.iter(ctx).collect();
            for inst in wait_to_remove {
                inst.remove(ctx);
            }
            self.unlink(ctx);
            ctx.dealloc(self);
            true
        } else {
            false
        }
    }

    /// 目前专用于不可达基本块删除，防止因循环依赖导致无法删除的问题
    pub fn remove_without_check(self, ctx: &mut Context) {
        let wait_to_remove: Vec<Instruction> = self.iter(ctx).collect();
        for inst in wait_to_remove {
            inst.remove(ctx);
        }
        self.unlink(ctx);
        ctx.dealloc(self);
    }

    ///只移除基本块，保留指令，用于基本块合并
    pub fn remove_only(self, ctx: &mut Context) -> bool {
        let user_count = self.users(ctx).into_iter().count();
        if user_count == 0 {
            self.unlink(ctx);
            ctx.dealloc(self);
            true
        } else {
            false
        }
    }

    pub fn is_removed(self, ctx: &Context) -> bool {
        self.deref(ctx).is_none()
    }
}

impl ArenaPtr for BasicBlock {
    type Arena = Context;
    type Data = BasicBlockData;
}

impl Useable for BasicBlock {
    fn users(self, arena: &Self::Arena) -> impl IntoIterator<Item = User<Self>> {
        self.deref(arena)
            .expect("Failed to deref `basicblocks` in struct")
            .users
            .iter()
            .copied()
    }

    fn insert(self, arena: &mut Self::Arena, user: User<Self>) {
        self.deref_mut(arena)
            .expect("Failed to deref `basicblocks` in struct")
            .users
            .insert(user);
    }

    fn remove(self, arena: &mut Self::Arena, user: User<Self>) {
        self.deref_mut(arena)
            .expect("Failed to deref `basicblocks` in struct Context")
            .users
            .remove(&user);
    }
}

impl LinkedListContainer<Instruction> for BasicBlock {
    type Ctx = Context;

    fn head(self, ctx: &Self::Ctx) -> Option<Instruction> {
        self.deref(ctx)
            .expect("Failed to deref `basicblocks` in struct Context")
            .head
    }

    fn tail(self, ctx: &Self::Ctx) -> Option<Instruction> {
        self.deref(ctx)
            .expect("Failed to deref `basicblocks` in struct Context")
            .tail
    }

    fn set_head(self, ctx: &mut Self::Ctx, head: Option<Instruction>) {
        self.deref_mut(ctx)
            .expect("Failed to deref `basicblocks` in struct Context")
            .head = head;
    }

    fn set_tail(self, ctx: &mut Self::Ctx, tail: Option<Instruction>) {
        self.deref_mut(ctx)
            .expect("Failed to deref `basicblocks` in struct Context")
            .tail = tail;
    }
}

impl LinkedListNode for BasicBlock {
    type Container = Function;
    type Ctx = Context;

    fn succ(self, ctx: &Self::Ctx) -> Option<Self> {
        self.deref(ctx)
            .expect("Failed to deref `basicblocks` in struct Context")
            .succ
    }

    fn pre(self, ctx: &Self::Ctx) -> Option<Self> {
        self.deref(ctx)
            .expect("Failed to deref `basicblocks` in struct Context")
            .pre
    }

    fn container(self, ctx: &Self::Ctx) -> Option<Self::Container> {
        self.deref(ctx)
            .expect("Failed to deref `basicblocks` in struct Context")
            .function
    }

    fn set_succ(self, ctx: &mut Self::Ctx, succ: Option<Self>) {
        self.deref_mut(ctx)
            .expect("Failed to deref `basicblocks` in struct Context")
            .succ = succ;
    }

    fn set_pre(self, ctx: &mut Self::Ctx, pre: Option<Self>) {
        self.deref_mut(ctx)
            .expect("Failed to deref `basicblocks` in struct Context")
            .pre = pre;
    }

    fn set_container(self, ctx: &mut Self::Ctx, container: Option<Self::Container>) {
        self.deref_mut(ctx)
            .expect("Failed to deref `basicblocks` in struct Context")
            .function = container;
    }

    fn unlink(self, ctx: &mut Self::Ctx) {
        let pre = self.pre(ctx);
        let succ = self.succ(ctx);

        if let Some(pre) = pre {
            pre.set_succ(ctx, succ);
        }

        if let Some(succ) = succ {
            succ.set_pre(ctx, pre);
        }

        if let Some(container) = self.container(ctx) {
            if container.head(ctx) == Some(self) {
                container.set_head(ctx, succ);
            }

            if container.tail(ctx) == Some(self) {
                container.set_tail(ctx, pre);
            }
        }

        self.set_pre(ctx, None);

        self.set_succ(ctx, None);

        self.set_container(ctx, None);
    }
}

use std::hash::Hash;

use super::{mir_label::MirLabel,
             mir_inst::MirInst,
             mir_function::MirFunction,
             mir_context::MirContext};
use crate::{passes::pass::structure::GetName, utils::{linked_list::{LinkedListContainer, LinkedListNode},
                   storage::{Arena, ArenaPtr, GenericPtr}}};

pub struct MirBlockData {
    label: MirLabel,
    start: Option<MirInst>,
    end: Option<MirInst>,
    succ: Option<MirBlock>,
    pre: Option<MirBlock>,
    parent: Option<MirFunction>,
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MirBlock(pub GenericPtr<MirBlockData>);

impl GetName<MirContext> for MirBlock{
    fn get_name(&self, ctx: &MirContext) -> String {
        self.deref(ctx).expect("Invalid block").label.0.clone()
    }
}

impl MirBlock {
    pub fn new(ctx: &mut MirContext, label: impl Into<MirLabel>) -> Self {
        ctx.alloc(MirBlockData {
            label: label.into(),
            start: None,
            end: None,
            succ: None,
            pre: None,
            parent: None,
        })
    }

    pub fn size(self, arena: &MirContext) -> usize {
        let mut size = 0;
        for _ in self.iter(arena) {
            size += 1;
        }
        size
    }

    pub fn label(self, arena: &MirContext) -> &MirLabel {
        &self.deref(arena).expect("Invalid block").label
    }

    pub fn remove(self, arena: &mut MirContext) {
        self.unlink(arena);
        arena.dealloc(self).unwrap();
    }

    pub fn get_head(self, ctx: &MirContext) -> Option<MirInst> {
        self.deref(ctx).expect("Invalid block").start
    }

    pub fn get_tail(self, ctx: &MirContext) -> Option<MirInst> {
        self.deref(ctx).expect("Invalid block").end
    }

    pub fn is_ret(self, ctx: &MirContext) -> bool {
        if let Some(inst) = self.get_tail(ctx) {
            inst.is_ret(ctx);
        }
        false
    }

}

impl Hash for MirBlock {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl ArenaPtr for MirBlock {
    type Arena = MirContext;
    type Data = MirBlockData;
}

impl LinkedListContainer<MirInst> for MirBlock {
    type Ctx = MirContext;
    fn head(self, ctx: &MirContext) -> Option<MirInst> {
        self.deref(ctx).expect("Invalid Inst").start
    }
    fn tail(self, ctx: &MirContext) -> Option<MirInst> {
        self.deref(ctx).expect("Invalid Inst").end
    }

    fn set_head(self, ctx: &mut MirContext, start: Option<MirInst>) {
        self.deref_mut(ctx).expect("Invalid Inst").start = start;
    }

    fn set_tail(self, ctx: &mut MirContext, end: Option<MirInst>) {
        self.deref_mut(ctx).expect("Invalid Inst").end = end;
    }
}

impl LinkedListNode for MirBlock {
    type Container = MirFunction;
    type Ctx = MirContext;
    fn succ(self, ctx: &MirContext) -> Option<Self> {
        self.deref(ctx).expect("Invaliid Block").succ
    }

    fn pre(self, ctx: &MirContext) -> Option<Self> {
        self.deref(ctx).expect("Invaliid Block").pre
    }

    fn set_succ(self, ctx: &mut MirContext, succ: Option<Self>) {
        self.deref_mut(ctx).expect("Invaliid Block").succ = succ;
    }

    fn set_pre(self, ctx: &mut MirContext, pre: Option<Self>) {
        self.deref_mut(ctx).expect("Invaliid Block").pre = pre;
    }

    fn container(self, ctx: &MirContext) -> Option<MirFunction> {
        self.deref(ctx).expect("Invaliid Block").parent
    }

    fn set_container(self, ctx: &mut MirContext, parent: Option<MirFunction>) {
        self.deref_mut(ctx).expect("Invaliid Block").parent = parent;
    }
}

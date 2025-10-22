use std::collections::BTreeSet;
use std::hash::Hash;

use super::{mir_label::MirLabel,
             mir_block::MirBlock,
             mir_context::MirContext,
             regs::PReg};
use crate::utils::{linked_list::LinkedListContainer,
                  storage::{Arena, ArenaPtr, GenericPtr}};

pub struct MirFunctionData {
    self_ptr: MirFunction,
    label: MirLabel,
    storage_stack_size: i32,
    current_stack_size: i32, 
    calleeargs_stack_size: i32,
    callee_regs: BTreeSet<PReg>,
    is_external: bool,
    start: Option<MirBlock>,
    end: Option<MirBlock>,
}

impl MirFunctionData {
    pub fn self_ptr(&self) -> MirFunction { self.self_ptr }
}

#[derive(Debug, PartialEq, Eq)]
pub struct MirFunction(pub GenericPtr<MirFunctionData>);

impl MirFunction {
    pub fn new(ctx: &mut MirContext, label: impl Into<MirLabel>, is_external: bool) -> Self {
        ctx.alloc_with(|self_ptr| MirFunctionData {
            self_ptr,
            label: label.into(),
            storage_stack_size: 0,
            current_stack_size: 0,
            calleeargs_stack_size: 0,
            callee_regs: BTreeSet::default(),
            is_external,
            start: None,
            end: None,
        })
    }

    pub fn label(self, arena: &MirContext) -> &MirLabel { 
        &self.deref(arena).expect("Invalid Function Pointer").label 
    }

    pub fn get_id(self, ctx: &MirContext) -> String {
        self.label(ctx).0.clone()
    }

    pub fn get_head(self, ctx: &MirContext) ->  Option<MirBlock> {
        self.deref(ctx).expect("Invalid Function Pointer").start
    }

    pub fn get_tail(self, ctx: &MirContext) -> Option<MirBlock> {
        self.deref(ctx).expect("Invalid Function Pointer").end
    }

    pub fn is_external(self, ctx: &MirContext) -> bool { 
        self.deref(ctx).expect("Invalid Function Pointer").is_external 
    }

    pub fn current_stack_size(self, ctx: &MirContext) -> i32 { 
        self.deref(ctx).expect("Invalid Function Pointer").current_stack_size 
    }

    pub fn add_current_stack_size(self, ctx: &mut MirContext, size: i32) {
        self.deref_mut(ctx).expect("Invalid Function Pointer").current_stack_size += size;
    }

    pub fn storage_stack_size(self, ctx: &MirContext) -> i32 { 
        self.deref(ctx).expect("Invalid Function Pointer").storage_stack_size 
    }

    
    pub fn add_storage_stack_size(self, ctx: &mut MirContext, size: i32) {
        self.deref_mut(ctx).expect("Invalid Function Pointer").storage_stack_size += size;
    }

    pub fn add_calleeargs_stack_size(self, ctx: &mut MirContext, size: i32) {
        self.deref_mut(ctx).expect("Invalid Function Pointer").calleeargs_stack_size += size;
    }

    pub fn calleeargs_stack_size(self, ctx: &MirContext) -> i32 {
        self.deref(ctx).expect("Invalid Function Pointer").calleeargs_stack_size
    }

    pub fn callee_regs(self, ctx: &MirContext) -> Vec<PReg> {
        let mut regs: Vec<PReg> = self.deref(ctx).expect("Invalid Function Pointer").callee_regs.iter().copied().collect();
        regs.sort();
        regs
    }

    pub fn add_callee_reg(self, ctx: &mut MirContext, reg: PReg) {
        self.deref_mut(ctx).expect("Invalid Function Pointer").callee_regs.insert(reg);
    }

    pub fn callee_stack_size(self, ctx: &MirContext) -> i32 {
        let mut size = 0;
        for reg in self.callee_regs(ctx) {
            if reg.is_float() {
                size += 4;
            } else {
                size += 8;
            }
        }
        if size % 8 != 0 {
            size += 4;
        }
        size
    }
}

impl ArenaPtr for MirFunction {
    type Arena = MirContext;
    type Data = MirFunctionData;
}

impl LinkedListContainer<MirBlock> for MirFunction {
    type Ctx = MirContext;
    fn head(self, ctx: &Self::Ctx) -> Option<MirBlock> { self.deref(ctx).expect("Invalid Function Pointer").start }

    fn tail(self, ctx: &Self::Ctx) -> Option<MirBlock> { self.deref(ctx).expect("Invalid Function Pointer").end }

    fn set_head(self, ctx: &mut Self::Ctx, start: Option<MirBlock>) {
        self.deref_mut(ctx).expect("Invalid Function Pointer").start = start;
    }

    fn set_tail(self, ctx: &mut Self::Ctx, end: Option<MirBlock>) {
        self.deref_mut(ctx).expect("Invalid Function Pointer").end = end;
    }
}

impl Clone for MirFunction {
    fn clone(&self) -> Self { *self }
}

impl Copy for MirFunction {}

impl Hash for MirFunction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { 
        self.0.hash(state) 
    }
}

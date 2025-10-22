use super::{mir_label::MirLabel,
            mir_block::{MirBlock, MirBlockData},
            mir_function::{MirFunction, MirFunctionData},
            mir_inst::{MirInst, MirInstData},
            regs::{RegKind, VReg},
            };

use crate::utils::{storage::{Arena, ArenaPtr, GenericArena}};

pub enum RawData {
    Bytes(Vec<i32>, usize),
    Bss(usize),
}

#[derive(Default)]
pub struct MirContext {
    pub insts: GenericArena<MirInstData>,
    pub blocks: GenericArena<MirBlockData>,
    pub funcs: GenericArena<MirFunctionData>,

    pub raw_data: Vec<(MirLabel, RawData)>,
    pub vreg_counter: u32,
    pub arch: String,
}

impl MirContext {
    pub fn new() -> Self { Self::default() }
    
    pub fn generate_vreg(&mut self, kind: RegKind) -> VReg {
        let vreg = VReg::new(self.vreg_counter, kind);
        self.vreg_counter += 1;
        vreg
    }

    pub fn add_raw_data(&mut self, label: impl Into<MirLabel>, data: RawData) {
        self.raw_data.push((label.into(), data));
    }

    pub fn get_functions(&self) -> impl Iterator<Item = MirFunction> + '_ {
        self.funcs.iter().map(|data| data.self_ptr() )
    }
}

impl Arena<MirBlock> for MirContext {
    fn alloc_with<F>(&mut self, f: F) -> MirBlock
    where
        F: FnOnce(MirBlock) -> MirBlockData,
    {
        MirBlock(self.blocks.alloc_with(|p| f(MirBlock(p))))
    }

    fn deref(&self, ptr: MirBlock) -> Option<&MirBlockData> {
        self.blocks.deref(ptr.0)
    }

    fn deref_mut(&mut self, ptr: MirBlock) -> Option<&mut MirBlockData> {
        self.blocks.deref_mut(ptr.0)
    }

    fn dealloc(&mut self, ptr: MirBlock) -> Option<MirBlockData> {
        self.blocks.dealloc(ptr.0)
    }
}

impl Arena<MirFunction> for MirContext {
    fn alloc_with<F>(&mut self, f: F) -> MirFunction
    where
        F: FnOnce(MirFunction) -> MirFunctionData,
    {
        MirFunction(self.funcs.alloc_with(|p| f(MirFunction(p))))
    }

    fn deref(&self, ptr: MirFunction) -> Option<&MirFunctionData> { self.funcs.deref(ptr.0) }

    fn deref_mut(&mut self, ptr: MirFunction) -> Option<&mut MirFunctionData> {
        self.funcs.deref_mut(ptr.0)
    }

    fn dealloc(&mut self, ptr: MirFunction) -> Option<<MirFunction as ArenaPtr>::Data> {
        self.funcs.dealloc(ptr.0)
    }
}

impl Arena<MirInst> for MirContext {
    fn alloc_with<F>(&mut self, f: F) -> MirInst
    where
        F: FnOnce(MirInst) -> MirInstData,
    {
        MirInst(self.insts.alloc_with(|p| f(MirInst(p))))
    }

    fn deref(&self, ptr: MirInst) -> Option<&MirInstData> { 
        self.insts.deref(ptr.0) 
    }

    fn deref_mut(&mut self, ptr: MirInst) -> Option<&mut MirInstData> {
        self.insts.deref_mut(ptr.0)
    }

    fn dealloc(&mut self, ptr: MirInst) -> Option<MirInstData> { 
        self.insts.dealloc(ptr.0) 
    }
}
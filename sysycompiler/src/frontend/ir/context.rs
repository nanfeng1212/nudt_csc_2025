use super::basicblock::{BasicBlock, BasicBlockData};
use super::function::{Function, FunctionData};
use super::global::{Global, GlobalData};
use super::instruction::{Instruction, InstructionData};
use super::typ::{Typ, TypeData};
use super::value::{Value, ValueData};
use crate::utils::storage::{Arena, GenericArena, GenericPtr, UniqueArena};
use rustc_hash::FxHashSet as HashSet;

pub struct Context {
    pub target: u32, //bytes
    pub types: UniqueArena<TypeData>,
    pub globals: GenericArena<GlobalData>,
    pub values: GenericArena<ValueData>,
    pub basicblocks: GenericArena<BasicBlockData>,
    pub functions: GenericArena<FunctionData>,
    pub instructions: GenericArena<InstructionData>,
    pub syslibdecls: Vec<FunctionDecl>,
    pub global_vlaues: HashSet<Value>, // 记录使用的全局变量便于后续G2L
}

pub struct FunctionDecl {
    pub name: String,
    pub parameters_typ: Vec<Typ>,
    pub return_type: Typ,
}

impl FunctionDecl {
    pub fn get_name(&self) -> &str {
        &self.name
    }
    pub fn get_parameters_typ(&self) -> &[Typ] {
        &self.parameters_typ
    }
    pub fn get_return_type(&self) -> &Typ {
        &self.return_type
    }
}
impl Default for Context {
    fn default() -> Self {
        Self::new(4)
    }
}

impl Context {
    /// 创建一个新的Context
    pub fn new(target: u32) -> Self {
        Self {
            target,
            types: UniqueArena::default(),
            globals: GenericArena::default(),
            values: GenericArena::default(),
            basicblocks: GenericArena::default(),
            functions: GenericArena::default(),
            instructions: GenericArena::default(),
            syslibdecls: Vec::new(),
            global_vlaues: HashSet::default(),
        }
    }

    /// 设置目标机器字节数
    pub fn set_target(&mut self, target: u32) {
        self.target = target;
    }

    /// 添加系统库函数声明
    pub fn add_funcdecl(&mut self, decl: FunctionDecl) {
        self.syslibdecls.push(decl);
    }

    /// 获取所有函数
    pub fn get_functions(&self) -> impl Iterator<Item = Function> + '_ {
        self.functions
            .iter()
            .map(|functiondata| functiondata.get_self_ptr())
    }

    /// 获取所有的值
    pub fn get_values(&self) -> impl Iterator<Item = Value> + '_ {
        self.values.iter().map(|valuedata| valuedata.get_self_ptr())
    }

    /// 获取所有全局变量Global类型
    pub fn get_globals(&self) -> impl Iterator<Item = Global> + '_ {
        self.globals
            .iter()
            .map(|globaldata| globaldata.get_self_ptr())
    }

    /// 获取所有的指令中用到的全局变量，Vlaue类型
    pub fn get_global_values(&self) -> &HashSet<Value> {
        &self.global_vlaues
    }
}

impl Arena<Typ> for Context {
    fn alloc_with<F>(&mut self, _: F) -> Typ
    where
        F: FnOnce(Typ) -> TypeData,
    {
        panic!("Failed to allocate type in context");
    }

    fn alloc(&mut self, typ: TypeData) -> Typ {
        Typ(self.types.alloc(typ))
    }

    fn dealloc(&mut self, ptr: Typ) -> Option<TypeData> {
        self.types.dealloc(ptr.0)
    }

    fn deref(&self, ptr: Typ) -> Option<&TypeData> {
        self.types.deref(ptr.0)
    }

    fn deref_mut(&mut self, ptr: Typ) -> Option<&mut TypeData> {
        self.types.deref_mut(ptr.0)
    }
}

impl Arena<Global> for Context {
    fn alloc_with<F>(&mut self, f: F) -> Global
    where
        F: FnOnce(Global) -> GlobalData,
    {
        Global(
            self.globals
                .alloc_with(|ptr: GenericPtr<GlobalData>| f(Global(ptr))),
        )
    }

    fn alloc(&mut self, data: GlobalData) -> Global {
        Global(self.globals.alloc_with(|_| data))
    }

    fn dealloc(&mut self, ptr: Global) -> Option<GlobalData> {
        self.globals.dealloc(ptr.0)
    }

    fn deref(&self, ptr: Global) -> Option<&GlobalData> {
        self.globals.deref(ptr.0)
    }

    fn deref_mut(&mut self, ptr: Global) -> Option<&mut GlobalData> {
        self.globals.deref_mut(ptr.0)
    }
}

impl Arena<Value> for Context {
    /// 申请一个新的值，并用f初始化该值，f是一个函数闭包（闭包要求传入一个Value作为参数，并返回一个ValueData）
    fn alloc_with<F>(&mut self, f: F) -> Value
    where
        F: FnOnce(Value) -> ValueData,
    {
        Value(
            self.values
                .alloc_with(|ptr: GenericPtr<ValueData>| f(Value(ptr))),
        )
    }

    /// 只将已经创建好的数据存进Arena中
    fn alloc(&mut self, data: ValueData) -> Value {
        Value(self.values.alloc_with(|_| data))
    }

    fn dealloc(&mut self, ptr: Value) -> Option<ValueData> {
        self.values.dealloc(ptr.0)
    }

    fn deref(&self, ptr: Value) -> Option<&<Value as crate::utils::storage::ArenaPtr>::Data> {
        self.values.deref(ptr.0)
    }

    fn deref_mut(
        &mut self,
        ptr: Value,
    ) -> Option<&mut <Value as crate::utils::storage::ArenaPtr>::Data> {
        self.values.deref_mut(ptr.0)
    }
}

impl Arena<BasicBlock> for Context {
    fn alloc_with<F>(&mut self, f: F) -> BasicBlock
    where
        F: FnOnce(BasicBlock) -> super::basicblock::BasicBlockData,
    {
        BasicBlock(
            self.basicblocks
                .alloc_with(|ptr: GenericPtr<super::basicblock::BasicBlockData>| {
                    f(BasicBlock(ptr))
                }),
        )
    }
    fn alloc(&mut self, data: super::basicblock::BasicBlockData) -> BasicBlock {
        BasicBlock(self.basicblocks.alloc_with(|_| data))
    }
    fn dealloc(&mut self, ptr: BasicBlock) -> Option<super::basicblock::BasicBlockData> {
        self.basicblocks.dealloc(ptr.0)
    }
    fn deref(&self, ptr: BasicBlock) -> Option<&super::basicblock::BasicBlockData> {
        self.basicblocks.deref(ptr.0)
    }
    fn deref_mut(&mut self, ptr: BasicBlock) -> Option<&mut super::basicblock::BasicBlockData> {
        self.basicblocks.deref_mut(ptr.0)
    }
}

impl Arena<Function> for Context {
    fn alloc_with<F>(&mut self, f: F) -> Function
    where
        F: FnOnce(Function) -> FunctionData,
    {
        Function(self.functions.alloc_with(|ptr| f(Function(ptr))))
    }

    fn dealloc(&mut self, ptr: Function) -> Option<FunctionData> {
        self.functions.dealloc(ptr.0)
    }

    fn deref(&self, ptr: Function) -> Option<&FunctionData> {
        self.functions.deref(ptr.0)
    }

    fn deref_mut(&mut self, ptr: Function) -> Option<&mut FunctionData> {
        self.functions.deref_mut(ptr.0)
    }
}

impl Arena<Instruction> for Context {
    fn alloc_with<F>(&mut self, f: F) -> Instruction
    where
        F: FnOnce(Instruction) -> InstructionData,
    {
        Instruction(self.instructions.alloc_with(|ptr| f(Instruction(ptr))))
    }

    fn dealloc(&mut self, ptr: Instruction) -> Option<InstructionData> {
        self.instructions.dealloc(ptr.0)
    }

    fn deref(&self, ptr: Instruction) -> Option<&InstructionData> {
        self.instructions.deref(ptr.0)
    }

    fn deref_mut(&mut self, ptr: Instruction) -> Option<&mut InstructionData> {
        self.instructions.deref_mut(ptr.0)
    }
}

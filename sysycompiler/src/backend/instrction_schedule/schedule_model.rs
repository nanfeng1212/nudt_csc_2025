use std::ops::Add;

use rustc_hash::{FxHashSet as HashSet};

use crate::backend::mir::{mir_context::MirContext, mir_inst::{MirInst, MirInstKind}};

// 依赖关系
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DependenceKind {
    Flow,    // 写后读
    Reverse,    // 读后写
    Output,    // 写后写
    Memory,    // 内存相关
    Control,   // 控制相关
}

// 获取依赖关系
pub fn get_depend(ctx: &MirContext, i1: MirInst, i2: MirInst) -> HashSet<DependenceKind> {
    let mut result = HashSet::default();
    let def_i1 = if i1.is_call(ctx) {
        if i1.is_void(ctx) {
            vec![]
        } else {
            vec![i1.get_ret_reg(ctx)]
        }
    } else {
        i1.def(ctx)
    };
    let def_i2 = if i2.is_call(ctx) {
        if i2.is_void(ctx) {
            vec![]
        } else {
            vec![i2.get_ret_reg(ctx)]
        }
    } else {
        i2.def(ctx)
    };
    // 流依赖
    for reg1 in def_i1.iter() {
        for reg2 in i2.uses(ctx) {
            if *reg1 == reg2 {
                result.insert(DependenceKind::Flow);
            }
        }
    }
    // 反向依赖
    for reg1 in i1.uses(ctx) {
        for reg2 in def_i2.iter() {
            if reg1 == *reg2 {
                result.insert(DependenceKind::Reverse);
            }
        }
    }
    // 输出依赖
    for reg1 in def_i1 {
        for reg2 in def_i2.iter() {
            if reg1 == *reg2 {
                result.insert(DependenceKind::Output);
            }
        }
    }
    // 内存相关依赖
    if (i1.is_store(ctx)||i1.is_load(ctx)) && (i2.is_store(ctx)||i2.is_load(ctx)) {
        result.insert(DependenceKind::Memory);
    }
    
    // 控制相关依赖
    if i1.is_branch(ctx) || i2.is_branch(ctx) {
        result.insert(DependenceKind::Control);
    }
    result
}

// 判断是否可以双发射
// 完善双发射判断
pub fn dual_emit(ctx: &MirContext, i1: MirInst, i2: MirInst) -> bool {
    // 检查资源冲突
    if i1.is_memory(ctx) && i2.is_memory(ctx) {
        return false; // 不能同时发射两个内存操作
    }
    if i1.is_branch(ctx) && i2.is_branch(ctx) {
        return false; // 不能同时发射两个分支
    }
    // if i1.is_fpu(ctx) && i2.is_fpu(ctx) {
    //     return false; // 不能同时发射两个浮点/向量指令
    // }
    if i1.is_multiply(ctx) && i2.is_multiply(ctx) {
        return false;
    }
    // 检查数据依赖
    if get_depend(ctx, i1, i2).contains(&DependenceKind::Flow)
        || get_depend(ctx, i1, i2).contains(&DependenceKind::Output)
    {
        return false;
    }
    true
}

// 获取延迟
pub fn get_latency(ctx: &MirContext, inst: MirInst) -> usize {
    if inst.is_memory(ctx) {
        return 4;
    }
    if inst.is_fpu(ctx) {
        return match inst.kind(ctx) {
            MirInstKind::Fmul{..} => 4,
            MirInstKind::Fdiv{..} => 15,
            _ => 3
        };
    }
    if inst.is_sdiv(ctx) {
        return 10;
    }
    if inst.is_multiply(ctx) {
        return 3;
    }
    1
}

// 更新资源预留表
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Resource {
    pub alu: u8,
    pub mem: u8,
    pub branch: u8,
    pub mul: u8,
    pub div: u8,
    pub fpu: u8,
}

impl Resource {
    pub fn a53() -> Self {
        Self {
            alu: 2,
            mem: 1,
            branch: 1,
            mul: 1,
            div: 1,
            fpu: 2,
        }
    }

    pub fn init() -> Self {
        Self {
            alu: 0,
            mem: 0,
            branch: 0,
            mul: 0,
            div: 0,
            fpu: 0,
        }
    }

    pub fn unsatisfy(&self, other: Self) -> bool {
        self.alu > other.alu || 
            self.mem > other.mem || 
            self.branch > other.branch || 
            self.mul > other.mul ||
            self.div > other.div || 
            self.fpu > other.fpu
    }
    pub fn satisfy(&self, other: Self) -> bool {
        self.alu <= other.alu && 
            self.mem <= other.mem && 
            self.branch <= other.branch && 
            self.mul <= other.mul &&
            self.div <= other.div &&
            self.fpu <= other.fpu
    }
}

impl Add for Resource {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self {
            alu: self.alu + other.alu,
            mem: self.mem + other.mem,
            branch: self.branch + other.branch,
            mul: self.mul + other.mul,
            div: self.div + other.div,
            fpu: self.fpu + other.fpu,
        }
    }
}

// 为每种指令类型创建资源预留
pub fn get_resource(ctx: &mut MirContext, inst: MirInst) -> Vec<Resource> {
    let mut tables = Vec::new();

    if inst.is_memory(ctx) {
        tables.push(Resource { mem: 1, ..Resource::init() });
        tables.push(Resource { mem: 1, ..Resource::init() });
        tables.push(Resource { mem: 1, ..Resource::init() });
        // tables.push(Resource::init());
        // tables.push(Resource::init());
        return tables;
    }

    if inst.is_branch(ctx) {
        tables.push(Resource { branch: 1, ..Resource::init() });
    }
    
    if inst.is_fpu(ctx) {
        // 浮点指令
        if inst.is_sdiv(ctx){
            tables.push(Resource { fpu: 1, ..Resource::init() });
            for _ in 0..14 {
                tables.push(Resource { fpu: 1, ..Resource::init() });
                //tables.push(Resource::init());
            }
            return tables;
        }
        if inst.is_multiply(ctx) {
            tables.push(Resource { fpu: 1, ..Resource::init() });
            for _ in 0..4 {
                tables.push(Resource { fpu: 1, ..Resource::init() });
                //tables.push(Resource::init());
            }
            return tables;
        }
        tables.push(Resource { fpu: 1, ..Resource::init() });
        for _ in 0..3 {
            tables.push(Resource { fpu: 1, ..Resource::init() });
            //tables.push(Resource::init());
        }
        return tables;
    }

    if inst.is_sdiv(ctx) {
        tables.push(Resource { div: 1, ..Resource::init() });
        for _ in 0..9 {
            tables.push(Resource { div: 1, ..Resource::init() });
            //tables.push(Resource::init());
        }
        return tables;
    }

    if inst.is_multiply(ctx) {
        tables.push(Resource { mul: 1, ..Resource::init() });
        tables.push(Resource { mul: 1, ..Resource::init() });
        tables.push(Resource { mul: 1, ..Resource::init() });
        // tables.push(Resource::init());
        // tables.push(Resource::init());
    }

    tables.push(Resource { alu: 1, ..Resource::init() });
    tables
}

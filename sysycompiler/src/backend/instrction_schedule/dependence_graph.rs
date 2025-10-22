use std::cmp::{Ordering, Reverse};
use crate::backend::mir::{mir_inst::MirInst};
use rustc_hash::{FxHashMap as HashMap};

// 处理指令优先级
pub struct InstComparator {
    pub inst: MirInst,
    pub inst_num: usize,
}

impl InstComparator {
    pub fn new(inst: MirInst, inst_num: usize) -> Self {
        Self {
            inst,
            inst_num,
        }
    }
}

impl Eq for InstComparator {}

impl PartialEq for InstComparator {
    fn eq(&self, x: &Self) -> bool { 
        self.inst_num == x.inst_num 
    }
}

impl Ord for InstComparator {
    fn cmp(&self, x: &Self) -> Ordering {
        Reverse(self.inst_num).cmp(&Reverse(x.inst_num))
    }
}

impl PartialOrd for InstComparator {
    fn partial_cmp(&self, x: &Self) -> Option<Ordering> { 
        Some(self.cmp(x)) 
    }
}

// 依赖图结构
#[derive(Debug)]
pub struct DependenceGraph {
    pub pres: HashMap<MirInst, Vec<MirInst>>,
    pub succs: HashMap<MirInst, Vec<MirInst>>,
}

impl DependenceGraph {
    pub fn new() -> Self {
        Self {
            pres: HashMap::default(),
            succs: HashMap::default(),
        }
    }

    pub fn add_edge(&mut self, i1: MirInst, i2: MirInst) {
        self.pres.entry(i2).or_insert_with(Vec::new).push(i1);
        self.succs.entry(i1).or_insert_with(Vec::new).push(i2);
    }

    pub fn get_pres(&self, i: &MirInst) -> &[MirInst] {
        self.pres.get(i).map(|v| v.as_slice()).unwrap_or_default()
    }

    pub fn get_succs(&self, i: &MirInst) -> &[MirInst] {
        self.succs.get(i).map(|v| v.as_slice()).unwrap_or_default()
    }

    pub fn print(&self) {
        for (i, pres) in &self.pres {
            println!("{:?} -> {:?}", i, pres);
        }
        for (i, succs) in &self.succs {
            println!("{:?} <- {:?}", i, succs);
        }
    }
}
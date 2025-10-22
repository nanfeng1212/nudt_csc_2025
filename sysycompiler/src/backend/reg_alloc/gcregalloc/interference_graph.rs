use crate::backend::mir::regs::{Reg, RegKind};
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::cmp::Ordering;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RegNumComparator(pub f64, pub Reg);

impl RegNumComparator {
    pub fn new(weight: f64, reg: Reg) -> Self {
        Self(weight, reg)
    }
}

impl Eq for RegNumComparator {}

impl Ord for RegNumComparator {
    fn cmp(&self, other: &Self) -> Ordering {
        other.0.partial_cmp(&self.0).unwrap_or(Ordering::Equal)
    }
}

impl PartialOrd for RegNumComparator {
    #[allow(clippy::non_canonical_partial_ord_impl)]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        other.0.partial_cmp(&self.0)
    }
}

#[derive(Debug, Clone)]
pub struct RegWeightMap {
    pub weights: HashMap<Reg, f64>,
}

impl RegWeightMap {
    pub fn new() -> Self {
        Self {
            weights: HashMap::default(),
        }
    }

    pub fn get(&self, reg: Reg) -> f64 {
        *self.weights.get(&reg).unwrap()
    }

    pub fn insert(&mut self, reg: Reg, weight: f64) {
        self.weights.insert(reg, weight);
    }
}

#[derive(Debug, Clone)]
pub struct InterferenceGraph {
    pub madj: HashMap<Reg, HashSet<Reg>>,
    pub kind: RegKind,
}

impl InterferenceGraph {
    pub fn new(kind: RegKind) -> Self {
        Self {
            madj: HashMap::default(),
            kind,
        }
    }

    pub fn adj(&self, reg: Reg) -> Option<&HashSet<Reg>> {
        self.madj.get(&reg)
    }

    pub fn add_edge(&mut self, u: Reg, v: Reg) {
        self.madj.entry(u).or_default().insert(v);
        self.madj.entry(v).or_default().insert(u);
    }
    pub fn is_empty(&self) -> bool {
        for reg in self.madj.keys() {
            if reg.is_vreg() {
                return false;
            }
        }
        true
    }

    pub fn delete(&mut self, reg: Reg) {
        if let Some(neighbors) = self.madj.remove(&reg) {
            for neighbor in neighbors {
                self.madj.entry(neighbor).and_modify(|i| {
                    i.remove(&reg);
                });
            }
        }
    }

    pub fn degree(&self, reg: Reg) -> usize {
        self.madj.get(&reg).map(|i| i.len()).unwrap_or(0)
    }

    pub fn print(&self) {
        for (reg, neighbors) in &self.madj {
            print!("{:?}: [", reg);
            for neighbor in neighbors {
                print!("{:?}, ", neighbor);
            }
            println!("]");
        }
    }
}

use rustc_hash::FxHashMap as HashMap;

use super::{
    irgen::IrGenResult,
    lalrpop::ast::{ComptimeValue, Type},
};

#[derive(Debug)]
pub struct SymbolEntry {
    pub typ: Type,
    pub comptime: Option<ComptimeValue>,
    pub ir_value: Option<IrGenResult>,
}

impl SymbolEntry {
    pub fn from_ty(typ: Type) -> Self {
        Self {
            typ,
            comptime: None,
            ir_value: None,
        }
    }
}

#[derive(Default)]
pub struct SymbolTable {
    stack: Vec<HashMap<String, SymbolEntry>>,
    pub curr_ret_ty: Option<Type>,
}

impl SymbolTable {
    pub fn enter_scope(&mut self) {
        self.stack.push(HashMap::default());
    }

    pub fn leave_scope(&mut self) {
        self.stack.pop();
    }

    pub fn insert(&mut self, name: impl Into<String>, entry: SymbolEntry) {
        self.stack.last_mut().unwrap().insert(name.into(), entry);
    }

    pub fn insert_upper(&mut self, name: impl Into<String>, entry: SymbolEntry, upper: usize) {
        self.stack
            .iter_mut()
            .rev()
            .nth(upper)
            .unwrap()
            .insert(name.into(), entry);
    }

    pub fn lookup(&self, name: &str) -> Option<&SymbolEntry> {
        for scope in self.stack.iter().rev() {
            if let Some(entry) = scope.get(name) {
                return Some(entry);
            }
        }
        None
    }

    pub fn lookup_mut(&mut self, name: &str) -> Option<&mut SymbolEntry> {
        for scope in self.stack.iter_mut().rev() {
            if let Some(entry) = scope.get_mut(name) {
                return Some(entry);
            }
        }
        None
    }

    pub fn register_sysylib(&mut self) {
        assert_eq!(self.stack.len(), 1);

        let getint = SymbolEntry::from_ty(Type::function(vec![], Type::int()));
        let getch = SymbolEntry::from_ty(Type::function(vec![], Type::int()));
        let getfloat = SymbolEntry::from_ty(Type::function(vec![], Type::float()));
        let getarray =
            SymbolEntry::from_ty(Type::function(vec![Type::ptr(Type::int())], Type::int()));
        let getfarray =
            SymbolEntry::from_ty(Type::function(vec![Type::ptr(Type::float())], Type::int()));
        let putint = SymbolEntry::from_ty(Type::function(vec![Type::int()], Type::void()));
        let putch = SymbolEntry::from_ty(Type::function(vec![Type::int()], Type::void()));
        let putfloat = SymbolEntry::from_ty(Type::function(vec![Type::float()], Type::void()));
        let putarray = SymbolEntry::from_ty(Type::function(
            vec![Type::int(), Type::ptr(Type::int())],
            Type::void(),
        ));
        let putfarray = SymbolEntry::from_ty(Type::function(
            vec![Type::int(), Type::ptr(Type::float())],
            Type::void(),
        ));

        let memset_int = SymbolEntry::from_ty(Type::function(
            vec![Type::ptr(Type::int()), Type::int(), Type::int()],
            Type::void(),
        ));
        let memset_float = SymbolEntry::from_ty(Type::function(
            vec![Type::ptr(Type::float()), Type::int(), Type::int()],
            Type::void(),
        ));
        self.insert("getint", getint);
        self.insert("getch", getch);
        self.insert("getfloat", getfloat);
        self.insert("getarray", getarray);
        self.insert("getfarray", getfarray);
        self.insert("putint", putint);
        self.insert("putch", putch);
        self.insert("putfloat", putfloat);
        self.insert("putarray", putarray);
        self.insert("putfarray", putfarray);
        self.insert("nudt_memset_int", memset_int);
        self.insert("nudt_memset_float", memset_float);

        let starttime = SymbolEntry::from_ty(Type::function(vec![Type::int()], Type::void()));
        let stoptime = SymbolEntry::from_ty(Type::function(vec![Type::int()], Type::void()));

        self.insert("_sysy_starttime", starttime);
        self.insert("_sysy_stoptime", stoptime);
    }
}

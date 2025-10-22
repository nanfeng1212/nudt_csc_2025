use core::panic;
use hexponent::FloatLiteral;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::{fmt, hash};

/// Parse x given hexadecimal float string into x f32.
pub fn parse_hexadecimal_float(s: &str) -> f32 {
    let float_literal: FloatLiteral = s.parse().unwrap();
    // because we want it to be precise, so we have to use x temp f64.
    let result = float_literal.convert::<f64>();
    result.inner() as f32
}

// CompUnit -> [CompUnit](Decl|FuncDef)
#[derive(Debug)]
pub struct CompUnit {
    pub items: Vec<GlobalItem>,
}

#[derive(Debug)]
pub enum GlobalItem {
    Decl(Decl),
    FuncDef(FuncDef),
}

// Decl -> ConstDecl|VarDecl
#[derive(Debug)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

// ConstDecl -> "const" BType ConstDef{"," ConstDef}
#[derive(Debug)]
pub struct ConstDecl {
    pub typ: Type,
    pub defs: Vec<ConstDef>,
}

//ConstDef -> Ident{'['ConstExp']'} '=' ConstInitVal
#[derive(Debug)]
pub struct ConstDef {
    pub id: String,
    pub dimensions: Vec<Exp>,
    pub init: ConstInitVal,
}

// ConstInitVal -> ConstExp | '{'[ConstInitVal{','ConstInitVal}]'}'
#[derive(Debug)]
pub enum ConstInitVal {
    Exp(Exp),
    List(Exp),
}

impl ConstInitVal {
    pub fn to_exp(self) -> Exp {
        match self {
            ConstInitVal::Exp(val) => val,
            ConstInitVal::List(inits) => inits,
        }
    }
}

// VarDecl -> Btype VarDef{',' VarDef}';'
#[derive(Debug)]
pub struct VarDecl {
    pub typ: Type,
    pub defs: Vec<VarDef>,
}

// VarDef -> Ident{'['ConstExp']'} | Ident{'['ConstExp']'}' '=' InitVal
#[derive(Debug)]
pub struct VarDef {
    pub id: String,
    pub dimensions: Vec<Exp>,
    pub init: Option<InitVal>,
}

// InitVal -> Exp | '{'[InitVal{','InitVal}]'}'
#[derive(Debug)]
pub enum InitVal {
    Exp(Exp),
    List(Exp),
}
impl InitVal {
    pub fn to_exp(self) -> Exp {
        match self {
            InitVal::Exp(val) => val,
            InitVal::List(inits) => inits,
        }
    }
}
// FuncDef -> FuncType Ident '(' [FuncFParams] ')' Block
#[derive(Debug)]
pub struct FuncDef {
    pub typ: Type,
    pub id: String,
    pub params: Vec<FuncFParam>,
    pub block: Block,
}

#[derive(Debug)]
pub struct FuncFParam {
    pub typ: Type,
    pub id: String,
    pub dimensions: Option<Vec<Exp>>,
}

// Block -> '{'{BlockItem}'}'
#[derive(Debug)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

// BlockItem -> Decl | Stmt
#[derive(Debug)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

/* Stmt ->   LVal '=' Exp';'
           | [Exp]';'
           | Block
           | 'if'(Cond) Stmt ['else' Stmt]
           | 'while'(Cond) Stmt
           | 'break';'
           | 'continue';'
           | 'return' [Exp]';'
*/
#[derive(Debug)]
pub enum Stmt {
    Assign(Assign),
    ExpStmt(ExpStmt),
    Block(Block),
    If(Box<If>),
    While(Box<While>),
    Break(Break),
    Continue(Continue),
    Return(Return),
}

#[derive(Debug)]
pub struct Assign {
    pub lval: LVal,
    pub exp: Exp,
}

#[derive(Debug)]
pub struct ExpStmt {
    pub exp: Option<Exp>,
}

#[derive(Debug)]
pub struct If {
    pub cond: Exp,
    pub then: Box<Stmt>,
    pub else_then: Option<Box<Stmt>>,
}

#[derive(Debug)]
pub struct While {
    pub cond: Exp,
    pub block: Box<Stmt>,
}

#[derive(Debug)]
pub struct Break;

#[derive(Debug)]
pub struct Continue;

#[derive(Debug)]
pub struct Return {
    pub exp: Option<Exp>,
}

#[derive(Debug, Clone)]
pub enum ExpKind {
    Const(ComptimeValue),
    Binary(BinaryOp, Box<Exp>, Box<Exp>),
    Unary(UnaryOp, Box<Exp>),
    FuncCall(FuncCall),
    LVal(LVal),
    InitList(Vec<Exp>),
    Coercion(Box<Exp>),
}

#[derive(Debug, Clone)]
pub struct Exp {
    pub kind: ExpKind,
    pub typ: Option<Type>,
}

impl Exp {
    pub fn typ(&self) -> &Type {
        self.typ.as_ref().unwrap()
    }
    pub fn const_(val: ComptimeValue) -> Self {
        let typ = val.get_type();
        Self {
            kind: ExpKind::Const(val),
            typ: Some(typ),
        }
    }

    pub fn binary(op: BinaryOp, lhs: Exp, rhs: Exp) -> Self {
        Self {
            kind: ExpKind::Binary(op, Box::new(lhs), Box::new(rhs)),
            typ: None,
        }
    }

    pub fn unary(op: UnaryOp, expr: Exp) -> Self {
        Self {
            kind: ExpKind::Unary(op, Box::new(expr)),
            typ: None,
        }
    }

    pub fn func_call(id: String, args: Vec<Exp>) -> Self {
        Self {
            kind: ExpKind::FuncCall(FuncCall { id, args }),
            typ: None,
        }
    }

    pub fn lval(lval: LVal) -> Self {
        Self {
            kind: ExpKind::LVal(lval),
            typ: None,
        }
    }

    pub fn init_list(exps: Vec<Exp>) -> Self {
        Self {
            kind: ExpKind::InitList(exps),
            typ: None,
        }
    }

    pub fn coercion(expr: &Exp, to: Type) -> Self {
        if let Some(ref from) = expr.typ {
            if from == &to {
                return expr.clone();
            }
        }

        Self {
            kind: ExpKind::Coercion(Box::new(expr.clone())),
            typ: Some(to),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LVal {
    pub id: String,
    pub dimensions: Vec<Exp>,
}

#[derive(Debug, Clone)]
pub struct FuncCall {
    pub id: String,
    pub args: Vec<Exp>,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add, //add
    Sub, //substraction
    Mul, //multiplication
    Div, //division
    Mod, //modulus
    Lt,  //less than
    Gt,  //greater than
    Le,  //less than or equal to
    Ge,  //greater than or equal to
    Eq,  //equal to
    Ne,  //not equal to
    And, //logical and
    Or,  //logical or
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Neg, //negation
    Not, //logical not
}

#[derive(Debug, Clone)]
pub enum ComptimeValue {
    Bool(bool),
    Int(i32),
    Float(f32),
    List(Vec<ComptimeValue>),
    Zeros(Type),
    Undef(Type),
}

impl ComptimeValue {
    /**
     * 将当前对象转化为int类型，如果当前对象不是int类型，则抛出异常。
     * 注意：Float类型转换为i32可能导致精度丢失。
     */
    pub fn unwrap2int(&self) -> i32 {
        match self {
            ComptimeValue::Bool(y) => *y as i32,
            ComptimeValue::Int(i) => *i,
            ComptimeValue::Float(f) => *f as i32,
            ComptimeValue::List(_) | ComptimeValue::Zeros(_) | ComptimeValue::Undef(_) => {
                panic!("expected int")
            }
        }
    }

    pub fn bool(y: bool) -> Self {
        ComptimeValue::Bool(y)
    }
    pub fn int(i: i32) -> Self {
        ComptimeValue::Int(i)
    }
    pub fn float(f: f32) -> Self {
        ComptimeValue::Float(f)
    }

    pub fn zeros(t: Type) -> Self {
        assert!(t.is_array());
        ComptimeValue::Zeros(t)
    }

    pub fn undef(t: Type) -> Self {
        ComptimeValue::Undef(t)
    }
    pub fn list(v: Vec<ComptimeValue>) -> Self {
        ComptimeValue::List(v)
    }

    pub fn get_type(&self) -> Type {
        match self {
            ComptimeValue::Bool(_) => Type::bool(),
            ComptimeValue::Int(_) => Type::int(),
            ComptimeValue::Float(_) => Type::float(),
            ComptimeValue::List(v) => {
                let elem_type = match v.first() {
                    Some(first) => first.get_type(),
                    None => panic!("empty list"), // 或者返回一个默认的Type
                };
                Type::array(elem_type, v.len())
            }
            ComptimeValue::Zeros(t) => t.clone(),
            ComptimeValue::Undef(t) => t.clone(),
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            ComptimeValue::Bool(y) => !*y,
            ComptimeValue::Int(i) => *i == 0,
            ComptimeValue::Float(f) => *f == 0.0,
            ComptimeValue::List(v) => v.iter().all(|x| x.is_zero()),
            ComptimeValue::Zeros(_) => true,
            ComptimeValue::Undef(_) => false,
        }
    }

    pub fn logical_or(&self, other: &Self) -> Self {
        let lhs = match self {
            ComptimeValue::Bool(x) => *x,
            ComptimeValue::Int(x) => *x != 0,
            ComptimeValue::Float(x) => *x != 0.0,
            ComptimeValue::List(_) | ComptimeValue::Zeros(_) | ComptimeValue::Undef(_) => {
                panic!("logical_or not supported for this type")
            }
        };

        let rhs = match other {
            ComptimeValue::Bool(y) => *y,
            ComptimeValue::Int(y) => *y != 0,
            ComptimeValue::Float(y) => *y != 0.0,
            ComptimeValue::List(_) | ComptimeValue::Zeros(_) | ComptimeValue::Undef(_) => {
                panic!("logical_or not supported for this type")
            }
        };

        ComptimeValue::Bool(lhs || rhs)
    }

    pub fn logical_and(&self, other: &Self) -> Self {
        let lhs = match self {
            ComptimeValue::Bool(x) => *x,
            ComptimeValue::Int(x) => *x != 0,
            ComptimeValue::Float(x) => *x != 0.0,
            ComptimeValue::List(_) | ComptimeValue::Zeros(_) | ComptimeValue::Undef(_) => {
                panic!("logical_and not supported for this type")
            }
        };

        let rhs = match other {
            ComptimeValue::Bool(y) => *y,
            ComptimeValue::Int(y) => *y != 0,
            ComptimeValue::Float(y) => *y != 0.0,
            ComptimeValue::List(_) | ComptimeValue::Zeros(_) | ComptimeValue::Undef(_) => {
                panic!("logical_and not supported for this type")
            }
        };

        ComptimeValue::Bool(lhs && rhs)
    }
}

/* Start: 实现ComptimeVal的运算符重载 包含：!、+、-、*、/、% */
impl std::ops::Neg for ComptimeValue {
    type Output = Self;

    fn neg(self) -> Self {
        match &self {
            ComptimeValue::Bool(x) => ComptimeValue::Int(-(*x as i32)),
            ComptimeValue::Int(x) => ComptimeValue::Int(-*x),
            ComptimeValue::Float(x) => ComptimeValue::Float(-*x),
            _ => panic!(
                "Type{} is not implemented for unary negation",
                self.get_type()
            ),
        }
    }
}

impl std::ops::Not for ComptimeValue {
    type Output = Self;

    fn not(self) -> Self {
        match &self {
            ComptimeValue::Bool(x) => ComptimeValue::Bool(!*x),
            ComptimeValue::Int(x) => ComptimeValue::Bool(*x != 0),
            ComptimeValue::Float(x) => ComptimeValue::Bool(*x != 0.0),
            _ => panic!("Type{} is not implemented for logical not", self.get_type()),
        }
    }
}

impl std::ops::Add for ComptimeValue {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match (&self, &other) {
            //bool + int
            (ComptimeValue::Bool(x), ComptimeValue::Int(y)) => ComptimeValue::Int(*x as i32 + y),
            (ComptimeValue::Int(x), ComptimeValue::Bool(y)) => ComptimeValue::Int(*x + *y as i32),
            // int + int
            (ComptimeValue::Int(x), ComptimeValue::Int(y)) => ComptimeValue::Int(*x + *y),
            // int + float
            (ComptimeValue::Int(x), ComptimeValue::Float(y)) => ComptimeValue::Float(*x as f32 + y),
            (ComptimeValue::Float(x), ComptimeValue::Int(y)) => {
                ComptimeValue::Float(*x + *y as f32)
            }
            // float + float
            (ComptimeValue::Float(x), ComptimeValue::Float(y)) => ComptimeValue::Float(*x + *y),
            // float + bool
            (ComptimeValue::Bool(x), ComptimeValue::Float(y)) => {
                ComptimeValue::Float(*x as i32 as f32 + *y)
            }
            (ComptimeValue::Float(x), ComptimeValue::Bool(y)) => {
                ComptimeValue::Float(*x + *y as i32 as f32)
            }

            _ => panic!(
                "Type{} + Type{} is not supported",
                self.get_type(),
                other.get_type()
            ),
        }
    }
}

impl std::ops::Sub for ComptimeValue {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        match (&self, &other) {
            //bool - int
            (ComptimeValue::Bool(x), ComptimeValue::Int(y)) => ComptimeValue::Int(*x as i32 - y),
            //bool - float
            (ComptimeValue::Bool(x), ComptimeValue::Float(y)) => {
                ComptimeValue::Float(*x as i32 as f32 - *y)
            }
            //int - bool
            (ComptimeValue::Int(x), ComptimeValue::Bool(y)) => ComptimeValue::Int(x - *y as i32),
            //int - int
            (ComptimeValue::Int(x), ComptimeValue::Int(y)) => ComptimeValue::Int(*x - *y),
            //int - float
            (ComptimeValue::Int(x), ComptimeValue::Float(y)) => ComptimeValue::Float(*x as f32 - y),
            //float - int
            (ComptimeValue::Float(x), ComptimeValue::Int(y)) => ComptimeValue::Float(x - *y as f32),
            //float - float
            (ComptimeValue::Float(x), ComptimeValue::Float(y)) => ComptimeValue::Float(*x - *y),
            //float - bool
            (ComptimeValue::Float(x), ComptimeValue::Bool(y)) => {
                ComptimeValue::Float(*x - *y as i32 as f32)
            }
            _ => panic!(
                "Type{} - Type{} is not supported",
                self.get_type(),
                other.get_type()
            ),
        }
    }
}

impl std::ops::Mul for ComptimeValue {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        match (&self, &other) {
            //bool * int
            (ComptimeValue::Bool(x), ComptimeValue::Int(y)) => ComptimeValue::Int(*x as i32 * y),
            //bool * float
            (ComptimeValue::Bool(x), ComptimeValue::Float(y)) => {
                ComptimeValue::Float(*x as i32 as f32 * *y)
            }
            //int * bool
            (ComptimeValue::Int(x), ComptimeValue::Bool(y)) => ComptimeValue::Int(*x * *y as i32),
            //int * int
            (ComptimeValue::Int(x), ComptimeValue::Int(y)) => ComptimeValue::Int(*x * *y),
            //int * float
            (ComptimeValue::Int(x), ComptimeValue::Float(y)) => ComptimeValue::Float(*x as f32 * y),
            //float * int
            (ComptimeValue::Float(x), ComptimeValue::Int(y)) => ComptimeValue::Float(x * *y as f32),
            //float * float
            (ComptimeValue::Float(x), ComptimeValue::Float(y)) => ComptimeValue::Float(*x * *y),
            //float * bool
            (ComptimeValue::Float(x), ComptimeValue::Bool(y)) => {
                ComptimeValue::Float(*x * (*y as i32) as f32)
            }

            _ => panic!(
                "Type{} x Type{} is not supported",
                self.get_type(),
                other.get_type()
            ),
        }
    }
}

impl std::ops::Div for ComptimeValue {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        match (&self, &other) {
            //bool / int
            (ComptimeValue::Bool(x), ComptimeValue::Int(y)) => ComptimeValue::Int(*x as i32 / y),
            //bool / float
            (ComptimeValue::Bool(x), ComptimeValue::Float(y)) => {
                ComptimeValue::Float(*x as i32 as f32 / *y)
            }
            //int / bool
            (ComptimeValue::Int(x), ComptimeValue::Bool(y)) => ComptimeValue::Int(*x / *y as i32),
            //int / int
            (ComptimeValue::Int(x), ComptimeValue::Int(y)) => ComptimeValue::Int(*x / *y),
            //int / float
            (ComptimeValue::Int(x), ComptimeValue::Float(y)) => ComptimeValue::Float(*x as f32 / y),
            //float / int
            (ComptimeValue::Float(x), ComptimeValue::Int(y)) => ComptimeValue::Float(x / *y as f32),
            //float / float
            (ComptimeValue::Float(x), ComptimeValue::Float(y)) => ComptimeValue::Float(*x / *y),
            //float / bool
            (ComptimeValue::Float(x), ComptimeValue::Bool(y)) => {
                ComptimeValue::Float(*x / (*y as i32) as f32)
            }
            _ => panic!(
                "Type{} / Type{} is not supported",
                self.get_type(),
                other.get_type()
            ),
        }
    }
}

impl std::ops::Rem for ComptimeValue {
    type Output = Self;
    fn rem(self, other: Self) -> Self {
        match (&self, &other) {
            // bool%int
            (ComptimeValue::Bool(x), ComptimeValue::Int(y)) => ComptimeValue::Int(*x as i32 % y),
            // int%bool
            (ComptimeValue::Int(x), ComptimeValue::Bool(y)) => ComptimeValue::Int(x % *y as i32),
            // int%int
            (ComptimeValue::Int(x), ComptimeValue::Int(y)) => ComptimeValue::Int(*x % *y),
            _ => panic!(
                "Type{} % Type{} is not supported",
                self.get_type(),
                other.get_type()
            ),
        }
    }
}

impl PartialEq for ComptimeValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ComptimeValue::Bool(x), ComptimeValue::Bool(y)) => x == y,
            (ComptimeValue::Int(x), ComptimeValue::Int(y)) => x == y,
            (ComptimeValue::Float(x), ComptimeValue::Float(y)) => x == y,

            //不同类型进行比较
            //Bool ->  Int
            (ComptimeValue::Bool(x), ComptimeValue::Int(y)) => (*x as i32) == *y,
            (ComptimeValue::Int(x), ComptimeValue::Bool(y)) => *x == (*y as i32),
            //Bool -> Float
            (ComptimeValue::Bool(x), ComptimeValue::Float(y)) => (*x as i32) as f32 == *y,
            (ComptimeValue::Float(x), ComptimeValue::Bool(y)) => *x == (*y as i32) as f32,
            // Int -> Float
            (ComptimeValue::Int(x), ComptimeValue::Float(y)) => *x as f32 == *y,
            (ComptimeValue::Float(x), ComptimeValue::Int(y)) => *x == (*y as f32),

            (ComptimeValue::List(x), ComptimeValue::List(y)) => {
                for (x, y) in x.iter().zip(y.iter()) {
                    if x != y {
                        return false;
                    }
                }
                true
            }
            (ComptimeValue::Zeros(_x), ComptimeValue::Zeros(_y)) => true,
            (ComptimeValue::Undef(_x), ComptimeValue::Undef(_y)) => false,
            _ => false,
        }
    }
}

impl Eq for ComptimeValue {}

impl PartialOrd for ComptimeValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            // bool, bool
            (ComptimeValue::Bool(x), ComptimeValue::Bool(y)) => x.partial_cmp(y),
            // int, int
            (ComptimeValue::Int(x), ComptimeValue::Int(y)) => x.partial_cmp(y),
            // float, float
            (ComptimeValue::Float(x), ComptimeValue::Float(y)) => x.partial_cmp(y),

            //bool, int
            (ComptimeValue::Bool(x), ComptimeValue::Int(y)) => (*x as i32).partial_cmp(y),
            (ComptimeValue::Int(x), ComptimeValue::Bool(y)) => x.partial_cmp(&(*y as i32)),

            //bool, float
            (ComptimeValue::Bool(x), ComptimeValue::Float(y)) => (*x as i32 as f32).partial_cmp(y),
            (ComptimeValue::Float(x), ComptimeValue::Bool(y)) => x.partial_cmp(&(*y as i32 as f32)),

            //int, float
            (ComptimeValue::Int(x), ComptimeValue::Float(y)) => (*x as f32).partial_cmp(y),
            (ComptimeValue::Float(x), ComptimeValue::Int(y)) => x.partial_cmp(&(*y as f32)),

            _ => panic!(
                "Type{} </>/<=/>= Type{} is not supported",
                self.get_type(),
                other.get_type()
            ),
        }
    }
}

/* End: 实现ComptimeVal的运算符重载 */

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub enum TypeKind {
    Void,
    Bool,
    Int,
    Float,
    Ptr(Box<Type>),
    Array(Box<Type>, usize),
    Function(Vec<Type>, Box<Type>),
}

#[derive(Clone, Eq)]
pub struct Type(Rc<TypeKind>);

/* 此处代码参考https://github.com/JuniMay/orzcc */
impl hash::Hash for Type {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl PartialEq for Type {
    // Just compare the pointers
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind() {
            TypeKind::Void => write!(f, "void"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Int => write!(f, "int"),
            TypeKind::Float => write!(f, "float"),
            TypeKind::Ptr(t) => write!(f, "{}*", t),
            TypeKind::Array(t, len) => write!(f, "[{} x {}]", t, len),
            TypeKind::Function(params, ret) => write!(
                f,
                "{}({})",
                ret,
                params
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl Type {
    thread_local! {
        /// The pool to implement singleton.
        ///
        /// Reference: https://github.com/pku-minic/koopa/blob/master/src/ir/types.rs
        ///
        /// XXX: This is not the only solution. In the implementation of IR, we use
        /// `UniqueArena` to store types.
        static POOL: RefCell<HashMap<TypeKind, Type>> = RefCell::new(HashMap::default());
    }

    /// Create x new type.
    pub fn make(kind: TypeKind) -> Type {
        Self::POOL.with(|pool| {
            let mut pool = pool.borrow_mut();
            if let Some(typ) = pool.get(&kind) {
                typ.clone()
            } else {
                let typ = Type(Rc::new(kind.clone()));
                pool.insert(kind, typ.clone());
                typ
            }
        })
    }

    /// Get the kind of the type.
    pub fn kind(&self) -> &TypeKind {
        &self.0
    }

    /// Create x new void type.
    pub fn void() -> Self {
        Self::make(TypeKind::Void)
    }

    /// Create x new boolean type.
    pub fn bool() -> Self {
        Self::make(TypeKind::Bool)
    }

    /// Create x new integer type.
    pub fn int() -> Self {
        Self::make(TypeKind::Int)
    }

    pub fn float() -> Self {
        Self::make(TypeKind::Float)
    }

    pub fn ptr(typ: Type) -> Self {
        Self::make(TypeKind::Ptr(Box::new(typ)))
    }

    pub fn array(typ: Type, len: usize) -> Self {
        Self::make(TypeKind::Array(Box::new(typ), len))
    }

    /// Get all the dimensions of an array
    pub fn array_dims(&self) -> Vec<usize> {
        let mut dims = Vec::new();
        let mut typ = self;

        while let TypeKind::Array(elem_ty, len) = typ.kind() {
            dims.push(*len);
            typ = elem_ty;
        }

        dims
    }

    pub fn get_array_size(&self) -> usize {
        match self.kind() {
            TypeKind::Array(_, len) => *len,
            _ => panic!("get_array_size: not an array type: {}", self),
        }
    }

    /// Create x new function type.
    pub fn function(params: Vec<Type>, ret: Type) -> Self {
        Self::make(TypeKind::Function(params, Box::new(ret)))
    }

    pub fn is_int(&self) -> bool {
        matches!(self.kind(), TypeKind::Int)
    }

    pub fn is_float(&self) -> bool {
        matches!(self.kind(), TypeKind::Float)
    }

    pub fn is_bool(&self) -> bool {
        matches!(self.kind(), TypeKind::Bool)
    }

    pub fn is_array(&self) -> bool {
        matches!(self.kind(), TypeKind::Array(..))
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self.kind(), TypeKind::Ptr(..))
    }

    pub fn is_void(&self) -> bool {
        matches!(self.kind(), TypeKind::Void)
    }

    pub fn unwrap_func(&self) -> (Vec<Type>, Type) {
        if let TypeKind::Function(params, ret) = self.kind() {
            let params = params.clone();
            let ret = ret.clone();
            (params, *ret)
        } else {
            panic!("unwrap_func: not x function type: {}", self);
        }
    }

    pub fn unwrap_array(&self) -> (&Type, usize) {
        if let TypeKind::Array(typ, len) = self.kind() {
            (typ, *len)
        } else {
            panic!("unwrap_array: not an array type: {}", self);
        }
    }

    pub fn inner_ty(&self) -> Option<&Type> {
        if let TypeKind::Array(typ, _) = self.kind() {
            Some(typ)
        } else if let TypeKind::Ptr(typ) = self.kind() {
            Some(typ)
        } else {
            None
        }
    }

    pub fn array_base(&self) -> &Type {
        let mut typ = self;
        while let TypeKind::Array(elem_ty, _) = typ.kind() {
            typ = elem_ty;
        }
        typ
    }

    pub fn get_array_depth(&self) -> usize {
        let mut depth = 0;
        let mut typ = self;
        while let TypeKind::Array(_, _) = typ.kind() {
            depth += 1;
            typ = typ.inner_ty().unwrap();
        }
        depth
    }

    pub fn bytewidth(&self) -> usize {
        match self.kind() {
            TypeKind::Void => 0,
            TypeKind::Bool => 1,
            TypeKind::Int => 4,
            TypeKind::Float => 4,
            TypeKind::Array(typ, len) => typ.bytewidth() * len,
            TypeKind::Ptr(_) => unreachable!(), // TODO: target dependent
            TypeKind::Function(_, _) => unreachable!(),
        }
    }
}

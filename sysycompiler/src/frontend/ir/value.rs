use super::context::Context;
use super::defuse::{Useable, User};
use super::function::Function;
use super::instruction::Instruction;
use super::typ::Typ;
use crate::frontend::ir2string::Display;
use crate::utils::storage::{Arena, ArenaPtr, GenericPtr};
use rustc_hash::FxHashSet as HashSet;

#[derive(Debug, Clone)]
pub enum ConstantValue {
    Undef {
        typ: Typ,
    },
    Zero {
        typ: Typ,
    },
    Bool {
        typ: Typ,
        value: bool,
    },

    Int32 {
        typ: Typ,
        value: i32,
    },
    Float32 {
        typ: Typ,
        value: f32,
    },
    Array {
        typ: Typ,
        elements: Vec<ConstantValue>,
    },
    GlobalPtr {
        typ: Typ,        // 全局指针类型
        name: String,    // 全局变量的名
        value_type: Typ, // 指针指向数据的类型
    },
}

impl ConstantValue {
    //常量值的类型
    pub fn typ(&self) -> Typ {
        match self {
            ConstantValue::Undef { typ } => *typ,
            ConstantValue::Zero { typ, .. } => *typ,
            ConstantValue::Bool { typ, .. } => *typ,
            ConstantValue::Int32 { typ, .. } => *typ,
            ConstantValue::Float32 { typ, .. } => *typ,
            ConstantValue::Array { typ, .. } => *typ,
            ConstantValue::GlobalPtr { value_type, .. } => *value_type,
        }
    }

    pub fn undef(typ: Typ) -> ConstantValue {
        ConstantValue::Undef { typ }
    }

    pub fn is_undef(&self) -> bool {
        matches!(self, ConstantValue::Undef { .. })
    }

    /// 创建zero类型
    pub fn zero(typ: Typ) -> ConstantValue {
        ConstantValue::Zero { typ }
    }

    /// 判断是不是zero类型
    pub fn is_zero(&self) -> bool {
        matches!(self, ConstantValue::Zero { .. })
    }

    /// 判断Value是不是0包括各种类型
    pub fn is_all_zero(&self) -> bool {
        match self {
            ConstantValue::Int32 { value, .. } => *value == 0,
            ConstantValue::Float32 { value, .. } => *value == 0.0,
            ConstantValue::Array { elements, .. } => {
                for element in elements {
                    if !element.is_all_zero() {
                        return false;
                    }
                }
                true
            }
            ConstantValue::GlobalPtr { .. } => true,
            _ => false,
        }
    }

    pub fn bool(ctx: &mut Context, value: bool) -> ConstantValue {
        let bool = Typ::bool(ctx);
        ConstantValue::Bool { typ: bool, value }
    }

    pub fn int32(ctx: &mut Context, value: i32) -> ConstantValue {
        let int32 = Typ::int32(ctx);
        ConstantValue::Int32 { typ: int32, value }
    }

    pub fn float32(ctx: &mut Context, value: f32) -> ConstantValue {
        let float32 = Typ::float32(ctx);
        ConstantValue::Float32 {
            typ: float32,
            value,
        }
    }

    pub fn global_ptr(ctx: &mut Context, name: String, value_type: Typ) -> ConstantValue {
        let global_ptr = Typ::ptr(ctx, value_type.clone());
        ConstantValue::GlobalPtr {
            typ: global_ptr,
            name,
            value_type,
        }
    }

    pub fn array(_ctx: &mut Context, typ: Typ, elements: Vec<ConstantValue>) -> ConstantValue {
        // let array = Typ::array(ctx, typ.clone(), elements.len());
        ConstantValue::Array {
            typ: typ,
            elements: elements,
        }
    }

    pub fn to_string(&self, ctx: &Context) -> String {
        let mut str = String::new();

        match self {
            ConstantValue::Undef { typ: _ } => str.push_str("undef"),
            ConstantValue::Zero { typ } => {
                if typ.is_float(ctx) {
                    str.push_str("0.0")
                } else if typ.is_int(ctx) {
                    str.push_str("0")
                }
            }
            ConstantValue::Bool { value, .. } => str.push_str(&value.to_string()),
            ConstantValue::Int32 { value, .. } => str.push_str(&value.to_string()),
            ConstantValue::Float32 { value, .. } => {
                fn format_number(value: f32) -> String {
                    if value.abs() >= 0.1 && value.abs() < 10_000_000.0 {
                        format!("{:.17}", value)
                    } else {
                        format!("{:.17e}", value)
                    }
                }
                str.push_str(&format!("{}", format_number(*value)));
            }
            ConstantValue::Array { elements, .. } => {
                str.push_str("[");
                for (i, element) in elements.iter().enumerate() {
                    if i > 0 {
                        str.push_str(", ");
                    }
                    let type_str = element.typ().display(ctx) + " ";
                    // println!("type_str: {}", type_str);
                    str.push_str(&type_str);
                    str.push_str(&element.to_string(ctx));
                }
                str.push_str("]");
            }
            ConstantValue::GlobalPtr { name, .. } => {
                str.push_str("@");
                str.push_str(name);
            }
        }
        str
    }
}

#[derive(Debug)]
pub enum ValueKind {
    // 指令的运算结果
    InstResult {
        instruction: Instruction,
        typ: Typ,
    },
    // 函数参数
    Parameter {
        function: Function,
        index: u32,
        typ: Typ,
    },
    // 常量值
    Constant {
        value: ConstantValue,
    },
    // // 基本块
    // BasicBlock{ basicblock: BasicBlock },
    // 函数
    Function {
        function: String,
        ret_type: Typ,
    },
}

#[derive(Debug)]
pub struct ValueData {
    // 值类型的种类
    pub kind: ValueKind,
    pub users: HashSet<User<Value>>,
    self_ptr: Value,
}

impl ValueData {
    pub fn get_self_ptr(&self) -> Value {
        self.self_ptr
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, Copy, PartialOrd)]
pub struct Value(pub GenericPtr<ValueData>);

impl ArenaPtr for Value {
    type Arena = Context;
    type Data = ValueData;
}

impl Value {
    pub fn new(ctx: &mut Context, kind: ValueKind) -> Self {
        ctx.alloc_with(|self_ptr| ValueData {
            self_ptr: self_ptr,
            kind,
            users: HashSet::default(),
        })
    }

    /// InstResult类型的值获取指令
    pub fn get_instruction(self, ctx: &Context) -> Option<Instruction> {
        match self.deref(ctx).unwrap().kind {
            ValueKind::InstResult { instruction, .. } => Some(instruction),
            _ => None,
        }
    }

    // 获取值类型
    pub fn kind(&self, ctx: &Context) -> Typ {
        match self
            .deref(ctx)
            .expect("Failed to deref `Value` in Value::kind()")
            .kind
        {
            ValueKind::InstResult { typ, .. } => typ,
            ValueKind::Parameter { typ, .. } => typ,
            ValueKind::Constant { ref value, .. } => value.typ(),
            ValueKind::Function {
                function: _,
                ret_type,
            } => ret_type,
        }
    }

    pub fn get_func_name(&self, ctx: &Context) -> Option<String> {
        match &self
            .deref(ctx)
            .expect("Failed to deref `Value` in Value::get_func_name()")
            .kind
        {
            ValueKind::Function { function, .. } => Some(function.clone()),
            _ => None,
        }
    }

    pub fn is_function(&self, ctx: &Context) -> bool {
        matches!(
            self.deref(ctx)
                .expect("Failed to deref `is_function` in Value")
                .kind,
            ValueKind::Function { .. }
        )
    }

    pub fn is_removed(&self, ctx: &Context) -> bool {
        self.deref(ctx).is_none()
    }

    pub fn is_undef(&self, ctx: &Context) -> bool {
        matches!(
            self.deref(ctx)
                .expect("Failed to deref `is_undef` in Value")
                .kind,
            ValueKind::Constant {
                value: ConstantValue::Undef { .. }
            }
        )
    }

    pub fn parameter(ctx: &mut Context, function: Function, index: u32, typ: Typ) -> Self {
        Self::new(
            ctx,
            ValueKind::Parameter {
                function: function,
                index,
                typ,
            },
        )
    }

    pub fn is_zero(&self, ctx: &Context) -> bool {
        match &self
            .deref(ctx)
            .expect("Failed to deref `is_constant_zero` in Value")
            .kind
        {
            ValueKind::Constant { value } => value.is_all_zero(),
            _ => false,
        }
    }

    pub fn is_constant(&self, ctx: &Context) -> bool {
        matches!(
            self.deref(ctx)
                .expect("Failed to deref `is_constant` in Value")
                .kind,
            ValueKind::Constant { .. }
        )
    }

    pub fn is_parameter(&self, ctx: &Context) -> bool {
        matches!(
            self.deref(ctx)
                .expect("Failed to deref `is_parameter` in Value")
                .kind,
            ValueKind::Parameter { .. }
        )
    }

    pub fn get_int_const_value(self, ctx: &Context) -> Option<i32> {
        match &self
            .deref(ctx)
            .expect("Failed to deref `get_int_const_value` in Value")
            .kind
        {
            ValueKind::Constant {
                value: constant_value,
            } => match constant_value {
                ConstantValue::Int32 { value, .. } => Some(*value),
                ConstantValue::Zero { .. } => Some(0),
                ConstantValue::Undef { .. } => Some(0),
                _ => None,
            },
            _ => {
                return None;
            }
        }
    }

    pub fn get_float_const_value(self, ctx: &Context) -> Option<f32> {
        match &self
            .deref(ctx)
            .expect("Failed to deref `get_float_const_value` in Value")
            .kind
        {
            ValueKind::Constant {
                value: constant_value,
            } => match constant_value {
                ConstantValue::Float32 { value, .. } => Some(*value as f32),
                ConstantValue::Zero { .. } => Some(0.0),
                ConstantValue::Undef { .. } => Some(0.0),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn get_bool_const_value(self, ctx: &Context) -> Option<bool> {
        match &self
            .deref(ctx)
            .expect("Failed to deref `get_bool_const_value` in Value")
            .kind
        {
            ValueKind::Constant {
                value: constant_value,
            } => match constant_value {
                ConstantValue::Bool { value, .. } => Some(*value),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn inst_result(ctx: &mut Context, instruction: Instruction, typ: Typ) -> Self {
        let value = Self::new(
            ctx,
            ValueKind::InstResult {
                instruction: instruction,
                typ,
            },
        );
        value
    }

    pub fn constant(ctx: &mut Context, value: ConstantValue) -> Self {
        Self::new(ctx, ValueKind::Constant { value })
    }

    pub fn constzero(ctx: &mut Context, typ: Typ) -> Self {
        if typ.is_float(ctx) {
            let zero = ConstantValue::float32(ctx, 0.0);
            Self::new(ctx, ValueKind::Constant { value: zero })
        } else if typ.is_int(ctx) {
            let zero = ConstantValue::int32(ctx, 0);
            Self::new(ctx, ValueKind::Constant { value: zero })
        } else {
            Self::new(
                ctx,
                ValueKind::Constant {
                    value: ConstantValue::zero(typ),
                },
            )
        }
    }

    pub fn undef(ctx: &mut Context, typ: Typ) -> Self {
        Self::new(
            ctx,
            ValueKind::Constant {
                value: ConstantValue::undef(typ),
            },
        )
    }

    pub fn function(ctx: &mut Context, function: String, ret_type: Typ) -> Self {
        Self::new(ctx, ValueKind::Function { function, ret_type })
    }

    pub fn global_ptr(ctx: &mut Context, name: String, value_type: Typ) -> Self {
        let t = ConstantValue::global_ptr(ctx, name, value_type);
        let t = Self::new(ctx, ValueKind::Constant { value: t });
        ctx.global_vlaues.insert(t.clone());
        t
    }

    pub fn is_global_ptr(&self, ctx: &Context) -> bool {
        matches!(
            self.deref(ctx)
                .expect("Failed to deref `is_global_ptr` in Value")
                .kind,
            ValueKind::Constant {
                value: ConstantValue::GlobalPtr { .. }
            }
        )
    }

    pub fn is_global_array(&self, ctx: &Context) -> bool {
        match self
            .deref(ctx)
            .expect("Failed to deref `is_global_array` in Value")
            .kind
        {
            ValueKind::Constant {
                value: ConstantValue::GlobalPtr { value_type, .. },
            } => value_type.is_array(ctx),
            _ => false,
        }
    }

    pub fn get_global_ptr_name(&self, ctx: &Context) -> Option<String> {
        match &self
            .deref(ctx)
            .expect("Failed to deref `get_global_ptr_name` in Value")
            .kind
        {
            ValueKind::Constant {
                value: ConstantValue::GlobalPtr { name, .. },
            } => Some(name.clone()),
            _ => None,
        }
    }

    pub fn is_ptr(&self, ctx: &Context) -> bool {
        self.kind(ctx).is_ptr(ctx)
    }

    // 替换值的所有使用者为指定值
    pub fn replace_all_uses_with(self, ctx: &mut Context, value: Self) -> i32 {
        let mut num_replaced = 0;
        let users: Vec<User<Value>> = self.users(ctx).into_iter().collect();
        for user in users {
            let instruction = user.get_instruction();
            let index = user.get_index();
            if index == 0 {
                panic!(
                    "Error: Cannot replace operand 0 of instruction {}",
                    instruction.display(ctx)
                );
            }
            if let Some(old_value) = instruction.get_operand(ctx, index - 1) {
                // <Value as Useable>::remove(old_value, ctx, user);
                old_value.remove(ctx, instruction);
                instruction.replace_operand(ctx, index - 1, value);
                <Value as Useable>::insert(value, ctx, User::new(instruction, index));
            } else {
                panic!("Error: Index out of bounds when replacing operand in instruction");
            }
            num_replaced += 1;
        }
        num_replaced
    }

    pub fn remove(self, ctx: &mut Context, inst: Instruction) {
        let mut to_remove = HashSet::default();
        let user_count = self.users(ctx).into_iter().count();
        // 首先以不可变方式借用 ctx 获取需要处理的用户
        for user in self.users(ctx).into_iter() {
            if user.get_instruction() == inst {
                to_remove.insert(user);
            }
        }
        // if self.display(ctx) == "%v19" {
        //     println!(
        //         "[Here]Value {} users count: {} to {}",
        //         self.display(ctx),
        //         user_count,
        //         user_count - to_remove.len()
        //     );
        // }
        // if self.display(ctx) == "%v19" {
        //     for user in self.users(ctx).into_iter() {
        //         println!("User: {}", user.get_instruction().display(ctx));
        //     }
        // }
        let remain_user_count = user_count - to_remove.len();
        // 根据收集到的用户进行移除操作
        for user in to_remove {
            // if self.display(ctx) == "%v19" {
            //     println!(
            //         "[Here]Value {} remove user: {}",
            //         self.display(ctx),
            //         user.get_instruction().display(ctx)
            //     );
            // }
            <Value as Useable>::remove(self, ctx, user);
        }
        if remain_user_count == 0 && !self.is_parameter(ctx) {
            // if self.display(ctx) == "%v19" {
            //     println!("[Here]Value {} is removed", self.display(ctx));
            // }
            ctx.dealloc(self);
        }
    }
}

impl Useable for Value {
    fn users(self, arena: &Self::Arena) -> impl IntoIterator<Item = User<Self>> {
        self.deref(arena)
            .expect("Failed to deref `users` in struct Context")
            .users
            .iter()
            .copied()
    }

    fn insert(self, arena: &mut Self::Arena, user: User<Self>) {
        // if self.display(arena) == "%v19" {
        //     println!(
        //         "[Here]Value {} insert user: {:?}",
        //         self.display(arena),
        //         user.get_instruction().get_kind(arena)
        //     );
        // }
        self.deref_mut(arena)
            .expect("Failed to deref mut `users` in struct Context")
            .users
            .insert(user);
        // if self.display(arena) == "%v19" {
        //     println!(
        //         "[Here]Value {} users count: {}",
        //         self.display(arena),
        //         self.users(arena).into_iter().count()
        //     );
        // }
    }

    fn remove(self, arena: &mut Self::Arena, user: User<Self>) {
        self.deref_mut(arena)
            .expect("Failed to deref mut `users` in struct Context")
            .users
            .remove(&user);
    }
}

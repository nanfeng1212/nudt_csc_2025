use core::panic;

use super::context::Context;
use crate::{
    frontend::ir2string::Display,
    utils::storage::{Arena, ArenaPtr, UniqueArenaPtr},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeData {
    Void,
    Bool, //LLVM IR中实际上不存在Bool类型，只有i1类型，为了便于理解仍然使用bool类型，生成代码时需使用i1类型
    Int32,
    Float32,
    Ptr { typ_of_ptr: Typ },
    Array { element_type: Typ, len: usize },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, Copy, PartialOrd)]
pub struct Typ(pub UniqueArenaPtr<TypeData>);

impl ArenaPtr for Typ {
    type Arena = Context;
    type Data = TypeData;
}

impl Typ {
    pub fn void(ctx: &mut Context) -> Self {
        ctx.alloc(TypeData::Void)
    }

    pub fn bool(ctx: &mut Context) -> Self {
        ctx.alloc(TypeData::Bool)
    }

    pub fn int32(ctx: &mut Context) -> Self {
        ctx.alloc(TypeData::Int32)
    }

    pub fn float32(ctx: &mut Context) -> Self {
        ctx.alloc(TypeData::Float32)
    }

    pub fn ptr(ctx: &mut Context, typ_of_ptr: Typ) -> Self {
        ctx.alloc(TypeData::Ptr { typ_of_ptr })
    }

    pub fn array(ctx: &mut Context, element_type: Typ, len: usize) -> Self {
        ctx.alloc(TypeData::Array { element_type, len })
    }

    pub fn is_void(&self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).unwrap(), TypeData::Void)
    }

    pub fn is_bool(&self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).unwrap(), TypeData::Bool)
    }

    pub fn is_int32(&self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).unwrap(), TypeData::Int32)
    }

    pub fn is_int(&self, ctx: &Context) -> bool {
        self.is_int32(ctx)
    }

    pub fn is_array(&self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).unwrap(), TypeData::Array { .. })
    }

    pub fn is_float32(&self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).unwrap(), TypeData::Float32)
    }

    pub fn is_float(&self, ctx: &Context) -> bool {
        self.is_float32(ctx)
    }

    pub fn is_ptr(&self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).unwrap(), TypeData::Ptr { .. })
    }

    pub fn get_ptr_inner_type(&self, ctx: &Context) -> Option<Typ> {
        match self.deref(ctx).unwrap() {
            TypeData::Ptr { typ_of_ptr } => Some(*typ_of_ptr),
            _ => None,
        }
    }

    pub fn as_array(&self, ctx: &Context) -> Option<(Typ, usize)> {
        match self.deref(ctx).unwrap() {
            TypeData::Array { element_type, len } => Some((*element_type, *len)),
            TypeData::Ptr { typ_of_ptr } => Some((*typ_of_ptr, 0)),
            _ => None,
        }
    }

    /// 获取数组的索引列表
    pub fn get_array_indices(&self, ctx: &Context) -> Option<Vec<usize>> {
        if self.is_array(ctx) {
            let mut indices = Vec::new();
            let mut current_type = self.clone();
            while let Some((element_type, len)) = current_type.as_array(ctx) {
                indices.push(len);
                current_type = element_type;
            }
            Some(indices)
        } else {
            None
        }
    }

    pub fn get_basetype(&self, ctx: &Context) -> Option<Typ> {
        match self.deref(ctx).unwrap() {
            TypeData::Ptr { typ_of_ptr } => typ_of_ptr.get_basetype(ctx),
            TypeData::Array { .. } => self.get_array_basetype(ctx),
            _ => Some(self.clone()),
        }
    }

    // 获取数组的基类型
    pub fn get_array_basetype(&self, ctx: &Context) -> Option<Typ> {
        if self.is_array(ctx) {
            let mut current_type = self.clone();
            while let Some((element_type, _)) = current_type.as_array(ctx) {
                current_type = element_type;
            }
            Some(current_type)
        } else {
            None
        }
    }

    /// 获取数组的指定深度的元素类型所占位数
    pub fn get_indices_bitwidth(&self, ctx: &Context, depth: usize) -> Option<usize> {
        let basetype = self.get_array_basetype(ctx);
        let basetype_bitwidth = basetype.unwrap().bitwidth(ctx);
        let mut indices = self.get_array_indices(ctx).unwrap();
        if depth > indices.len() {
            panic!(
                "Depth out of bounds: {} for type {:?}",
                depth,
                self.display(ctx)
            );
        } else {
            indices.reverse();
            let max_depth = indices.len() - depth;
            let mut partial_res = 1;
            for i in 1..max_depth + 1 {
                partial_res *= indices[i - 1];
            }
            return Some(partial_res * basetype_bitwidth);
        }
    }

    /// 获取指针类型数组指定深度的元素类型所占位数
    /// 主要是针对作为函数参数的数组
    pub fn get_ptr_indices_bitwidth(&self, ctx: &Context, depth: usize) -> Option<usize> {
        match self.deref(ctx).unwrap() {
            TypeData::Ptr { typ_of_ptr } => {
                if typ_of_ptr.is_array(ctx) {
                    typ_of_ptr.get_indices_bitwidth(ctx, depth)
                } else {
                    Some(typ_of_ptr.bitwidth(ctx))
                }
            }
            TypeData::Array {
                element_type: _,
                len: _,
            } => self.get_indices_bitwidth(ctx, depth),
            _ => None,
        }
    }

    /// 类型所占位数
    pub fn bitwidth(&self, ctx: &Context) -> usize {
        match self.deref(ctx).unwrap() {
            TypeData::Void => 0,
            TypeData::Bool => 1,
            TypeData::Int32 => 32,
            TypeData::Float32 => 32,
            TypeData::Ptr { .. } => ctx.target as usize * 8,
            TypeData::Array { element_type, len } => element_type.bitwidth(ctx) * len,
        }
    }

    /// 类型所占字节数
    pub fn bytewidth(&self, ctx: &Context) -> usize {
        (self.bitwidth(ctx) + 7) / 8
    }
}

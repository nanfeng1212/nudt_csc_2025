use std::{fmt, mem};
use std::cmp::Ordering;
use std::marker::PhantomData;
use std::collections::{HashMap, HashSet};
use std::hash::{DefaultHasher, Hash, Hasher};

/// Arena的指针特性
pub trait ArenaPtr: Copy + Eq + Hash {
    type Arena: Arena<Self>;
    /// 内部数据
    type Data;

    /// 通过Arena解引用
    /// # 返回值
    /// - `Some(&Self::Data)`: 如果指针有效，则返回指向数据的引用。
    /// - `None`: 如果指针无效，则返回None。
    fn deref(self, arena: &Self::Arena) -> Option<&Self::Data> {
        arena.deref(self) 
    }

    /// 通过Arena解引用
    /// # 返回值
    /// - `Some(&Self::Data)`: 如果指针有效，则返回指向数据的可变引用。
    /// - `None`: 如果指针无效，则返回None。
    fn deref_mut(self, arena: &mut Self::Arena) -> Option<&mut Self::Data> {
        arena.deref_mut(self)
    }
}

/// Arena的存储数据和分配指针特性
/// # 参数类型
/// - `Ptr`: Arena支持的指针类型
pub trait Arena<Ptr: ArenaPtr<Arena = Self>>{
    /// 为数据分配指针并存储到Arena中
    /// # 参数列表
    /// - `f`: 接收分配的指针返回数据类型的函数
    ///
    /// # 返回值
    /// 存储数据的指针
    fn alloc_with<F>(&mut self, f: F) -> Ptr
    where
        F: FnOnce(Ptr) -> Ptr::Data;

    /// 将数据存储到arena中并返回指针
    fn alloc(&mut self, data: Ptr::Data) -> Ptr { self.alloc_with(|_| data) }

    /// 释放指针指向的Arena中的数据
    /// # 返回值
    /// - `Some(Ptr::Data)`: 返回指针指向的数据。
    /// - `None`: 如果指针无效，则返回None。
    fn dealloc(&mut self, ptr: Ptr) -> Option<Ptr::Data>;

    /// 解引用一个指针
    /// # 返回值
    /// - `Some(&Ptr::Data)`: 返回在内存池中存储的数据的引用。
    /// - `None`: 指针无效，返回None
    fn deref(&self, ptr: Ptr) -> Option<&Ptr::Data>;

    /// 解引用一个指针，返回数据的可变引用
    /// # 返回值
    /// - `Some(&mut Ptr::Data)`: 内存池中数据的可变引用
    /// - `None`: 指针无效，返回None
    fn deref_mut(&mut self, ptr: Ptr) -> Option<&mut Ptr::Data>;
}


/// Arena的泛型指针
/// # 参数类型
/// - `Data`: 与存储的数据的类型一致
pub struct GenericPtr<Data> {
    /// 指针的原始索引
    index: usize,
    _phantom: PhantomData<Data>,
}

impl<Data> GenericPtr<Data> {
    fn from_index(index: usize) -> Self {
        Self {
            index,
            _phantom: PhantomData,
        }
    }
}

/// 代表轻量化索引的特性，与Arena指针无关
pub trait Idx: Copy + Ord + Hash {
    /// 获取原始的索引
    fn index(self) -> usize;
}

impl<Data> Idx for GenericPtr<Data> {
    fn index(self) -> usize { self.index }
}

impl<Data> Clone for GenericPtr<Data> {
    fn clone(&self) -> Self { *self }
}

impl<Data> Copy for GenericPtr<Data> {}

impl<Data> Hash for GenericPtr<Data> {
    fn hash<H: Hasher>(&self, state: &mut H) { self.index.hash(state) }
}

impl<Data> PartialEq for GenericPtr<Data> {
    fn eq(&self, other: &Self) -> bool { self.index == other.index }
}

impl<Data> Eq for GenericPtr<Data> {}

impl<Data> PartialOrd for GenericPtr<Data> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
}

impl<Data> Ord for GenericPtr<Data> {
    fn cmp(&self, other: &Self) -> Ordering { self.index.cmp(&other.index) }
}

impl<Data> fmt::Debug for GenericPtr<Data> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "*{}", self.index) }
}

impl<Data> fmt::Display for GenericPtr<Data> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "*{}", self.index) }
}

impl<Data> fmt::Pointer for GenericPtr<Data> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "*{:x}", self.index) }
}

impl<Data> fmt::LowerHex for GenericPtr<Data> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "*{:x}", self.index) }
}

impl<Data> fmt::UpperHex for GenericPtr<Data> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "*{:X}", self.index) }
}

/// 泛型Arena的条目
pub enum GenericEntry<Data> {
    /// 条目是空的，空闲列表并不是按照索引顺序进行排序的，而是按照内存释放的顺序来排序的，最后一个被释放的条目会成为空闲列表中的第一个条目。
    Vacant {
        /// 下一个空位的索引
        next: Option<usize>,
    },
    /// 条目被占用
    Occupied(Data),
}

/// 一个泛型Arena
/// # 参数类型
/// - `Data`: 存储数据的类型
pub struct GenericArena<Data> {
    /// Arena中的所有条目
    entries: Vec<GenericEntry<Data>>,
    /// 空闲列表的头部
    free_head: Option<usize>,
}

impl<Data> Default for GenericArena<Data> {
    fn default() -> Self {
        Self {
            entries: Vec::new(),
            free_head: None,
        }
    }
}

impl<Data> GenericArena<Data> {
    /// 预留出额外的空间
    pub fn reserve(&mut self, additional: usize) { self.entries.reserve(additional) }

    /// 创建一个容量为capacity的内存池
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            entries: Vec::with_capacity(capacity),
            free_head: None,
        }
    }

    /// 迭代存储的数据的不可变引用
    pub fn iter(&self) -> impl Iterator<Item = &Data> {
        self.entries.iter().filter_map(|entry| match entry {
            GenericEntry::Occupied(value) => Some(value),
            GenericEntry::Vacant { .. } => None,
        })
    }

    /// 迭代存储的数据的可变引用
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Data> {
        self.entries.iter_mut().filter_map(|entry| match entry {
            GenericEntry::Occupied(value) => Some(value),
            GenericEntry::Vacant { .. } => None,
        })
    }
}

impl<Data> ArenaPtr for GenericPtr<Data> {
    type Arena = GenericArena<Data>;
    type Data = Data;
}

impl<Data> Arena<GenericPtr<Data>> for GenericArena<Data> {
    fn alloc_with<F>(&mut self, f: F) -> GenericPtr<Data>
    where
        F: FnOnce(GenericPtr<Data>) -> Data,
    {
        match self.free_head.take() {
            Some(index) => {
                let entry = &mut self.entries[index];
                self.free_head = match entry {
                    // 有空块，返回值空块位置
                    GenericEntry::Vacant { next } => *next,
                    // 有空块，但被占用是不允许的
                    GenericEntry::Occupied(_) => unreachable!(),
                };
                let ptr = GenericPtr::from_index(index);
                *entry = GenericEntry::Occupied(f(ptr)); // 将内存块设置为占用
                ptr
            }
            None => {
                // 没有空块，需要创建空块
                let index = self.entries.len();
                let ptr = GenericPtr::from_index(index);
                self.entries.push(GenericEntry::Occupied(f(ptr)));
                ptr
            }
        }
    }

    // 释放内存块
    fn dealloc(&mut self, ptr: GenericPtr<Data>) -> Option<Data> {
        let idx = ptr.index();

        // 超出项目范围，返回 None
        if idx >= self.entries.len() {
            return None;
        }

        // 将当前索引位置的内存条目替换为 Vacant，并保存原有条目
        let delete_entry = mem::replace(
            &mut self.entries[idx],
            GenericEntry::Vacant {
                next: self.free_head,  // 将 free_head 指向原来空闲链表的下一个
                },
            );

        // 更新 free_head 为当前索引
        self.free_head = Some(idx);

        // 根据原条目的类型返回数据（如果是占用状态的话）
        match delete_entry {
            GenericEntry::Vacant { .. } => None,  // 如果原条目已经是空闲的，返回 None
            GenericEntry::Occupied(data) => Some(data),  // 如果原条目是占用的，返回数据
        }
    }

    // 内存池中解引用一个指针,返回数据的不可变引用
    fn deref(&self, ptr: GenericPtr<Data>) -> Option<&Data> {
        match self.entries.get(ptr.index())? {
            GenericEntry::Occupied(value) => Some(value),
            GenericEntry::Vacant { .. } => None,
        }
    }

    // 内存池中解引用一个指针,返回数据的可变引用
    fn deref_mut(&mut self, ptr: GenericPtr<Data>) -> Option<&mut Data> {
        match self.entries.get_mut(ptr.index())? {
            GenericEntry::Occupied(value) => Some(value),
            GenericEntry::Vacant { .. } => None,
        }
    }
}

/// 表示 "Arena" 的唯一哈希值的结构体
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UniqueArenaHash(u64);

impl UniqueArenaHash {
    pub fn new<T: Hash + 'static + ?Sized>(val: &T) -> Self {
        let mut hasher = DefaultHasher::new();
        val.hash(&mut hasher);
        std::any::TypeId::of::<T>().hash(&mut hasher);
        UniqueArenaHash(hasher.finish())
    }
}

pub trait GetUniqueArenaHash {
    fn unique_arena_hash(&self) -> UniqueArenaHash;
}

impl<T> GetUniqueArenaHash for T
where
    T: Hash + 'static + ?Sized,
{
    fn unique_arena_hash(&self) -> UniqueArenaHash { UniqueArenaHash::new(self) }
}

/// 唯一的内存此，每个值在内存池中都是唯一的
pub struct UniqueArena<T>
where
    T: GetUniqueArenaHash + Eq,
{
    arena: GenericArena<T>,
    unique_map: HashMap<UniqueArenaHash, HashSet<GenericPtr<T>>>,
}

pub struct UniqueArenaPtr<T>(GenericPtr<T>);
impl<T> fmt::Debug for UniqueArenaPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "UniqueArenaPtr({})", self.0.index())
    }
}

impl<T> Clone for UniqueArenaPtr<T> {
    fn clone(&self) -> Self { *self }
}

impl<T> Copy for UniqueArenaPtr<T> {}

impl<T> Hash for UniqueArenaPtr<T> {
    fn hash<H: Hasher>(&self, state: &mut H) { self.0.hash(state); }
}

impl<T> PartialEq for UniqueArenaPtr<T> {
    fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}

impl<T> Eq for UniqueArenaPtr<T> {}

impl<T> Ord for UniqueArenaPtr<T> {
    fn cmp(&self, other: &Self) -> Ordering { self.0.cmp(&other.0) }
}

impl<T> PartialOrd for UniqueArenaPtr<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
}

impl<T> Idx for UniqueArenaPtr<T> {
    fn index(self) -> usize { self.0.index() }
}

impl<T> Default for UniqueArena<T>
where
    T: GetUniqueArenaHash + Eq,
{
    fn default() -> Self {
        Self {
            arena: GenericArena::default(),
            unique_map: HashMap::default(),
        }
    }
}

impl<T> ArenaPtr for UniqueArenaPtr<T>
where
    T: GetUniqueArenaHash + Eq,
{
    type Arena = UniqueArena<T>;
    type Data = T;
}

impl<T> Arena<UniqueArenaPtr<T>> for UniqueArena<T>
where
    T: GetUniqueArenaHash + Eq,
{
    fn alloc_with<F>(&mut self, _: F) -> UniqueArenaPtr<T>
    where
        F: FnOnce(UniqueArenaPtr<T>) -> T,
    {
        panic!("UniqueArena does not support alloc_with");
    }

    fn alloc(&mut self, data: T) -> UniqueArenaPtr<T> {
        let cur = data.unique_arena_hash();
        if let Some(ptrs) = self.unique_map.get(&cur) {
            for &ptr in ptrs {
                if &data == self.arena.deref(ptr).expect("invalid pointer present in unique map")
                {
                    return UniqueArenaPtr(ptr);
                }
            }
        }
        let ptr = self.arena.alloc(data);
        self.unique_map.entry(cur).or_default().insert(ptr);
        UniqueArenaPtr(ptr)
    }

    fn dealloc(&mut self, ptr: UniqueArenaPtr<T>) -> Option<T> {
        let data = self.arena.deref(ptr.0)?;
        let cur = data.unique_arena_hash();
        if !self.unique_map.entry(cur).or_default().remove(&ptr.0)
        {
            unreachable!("value present in arena but not in unique map");
        }
        Some(self.arena.dealloc(ptr.0).unwrap_or_else(|| unreachable!("pointer dereferenced but cannot be deallocated")),)
    }

    fn deref(&self, ptr: UniqueArenaPtr<T>) -> Option<&T> { self.arena.deref(ptr.0) }

    fn deref_mut(&mut self, ptr: UniqueArenaPtr<T>) -> Option<&mut T> {
        self.arena.deref_mut(ptr.0)
    }
}
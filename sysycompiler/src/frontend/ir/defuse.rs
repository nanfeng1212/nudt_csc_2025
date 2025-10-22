use super::instruction::Instruction;
use crate::utils::storage::ArenaPtr;

pub trait Useable: ArenaPtr {
    /* 获取值的使用者 */
    fn users(self, arena: &Self::Arena) -> impl IntoIterator<Item = User<Self>>;

    /* 为一个值添加一个使用者 */
    fn insert(self, arena: &mut Self::Arena, user: User<Self>);

    /* 删除一个值的使用者 */
    fn remove(self, arena: &mut Self::Arena, user: User<Self>);
}

/// def-use链中的使用者，泛型结构体可与任何实现了Useable特性的结构体关联
/// E.G:
///
/// add %1 %2
/// User<Instruction> { instruction: instructions, index: 1,}
/// User<Instruction> { instruction: instructions, index: 2,}
/// index 0 留给指令的运算结果
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub struct User<T: Useable> {
    instruction: Instruction,
    index: usize,
    _marker: std::marker::PhantomData<T>, //User结构体只在逻辑上和T关联，不实际存储T类型的值
}

impl<T: Useable> User<T> {
    pub fn new(instruction: Instruction, index: usize) -> Self {
        Self {
            instruction,
            index,
            _marker: std::marker::PhantomData,
        }
    }

    pub fn get_instruction(&self) -> Instruction {
        self.instruction
    }

    pub fn get_index(&self) -> usize {
        self.index
    }
}

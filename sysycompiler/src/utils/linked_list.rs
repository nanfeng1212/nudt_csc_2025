/// 链表错误操作方法
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LinkedListError<N> {
    /// 节点已经在链表中
    NodeAlreadyInContainer(N),
    /// 不允许删除不在链表中的节点
    PositionNodeNotInContainer(N),
    /// 当前节点没有链接
    CurrentNodeNotLinked(N),
}

impl<N> From<LinkedListError<N>> for String
where
    N: std::fmt::Debug,
{
    fn from(err: LinkedListError<N>) -> String {
        format!("LinkedListError: {:?}", err)
    }
}

/// 链表操作方法
/// # 参数列表
/// - `Node`: 链表节点类型
pub trait LinkedListContainer<Node>: Copy + Eq
where
    Node: LinkedListNode<Ctx = Self::Ctx, Container = Self>,
{
    /// 节点的上下文环境与容器的上下文环境一致，来自同一个内存池。
    type Ctx;

    /// 获取链表头节点
    fn head(self, ctx: &Self::Ctx) -> Option<Node>;
    /// 设置链表头节点
    fn set_head(self, ctx: &mut Self::Ctx, head: Option<Node>);
    /// 获取链表尾节点
    fn tail(self, ctx: &Self::Ctx) -> Option<Node>;
    /// 设置链表尾节点
    fn set_tail(self, ctx: &mut Self::Ctx, tail: Option<Node>);

    /// 在链表前面添加节点
    /// # 参数列表
    /// - `ctx`: 获取数据的环境，如内存池（Arena）
    /// - `node`: 添加的节点
    ///
    /// # 返回值
    /// - [`Ok`]: 操作成功
    /// - [`LinkedListError::NodeAlreadyInContainer`]: 节点已存在链表中
    fn push_front(self, ctx: &mut Self::Ctx, node: Node) -> Result<(), LinkedListError<Node>> {
        if node.container(ctx).is_some() {
            return Err(LinkedListError::NodeAlreadyInContainer(node));
        }

        if let Some(head) = self.head(ctx) {
            head.insert_before(ctx, node)
                .unwrap_or_else(|_| unreachable!());
        } else {
            self.set_head(ctx, Some(node));
            self.set_tail(ctx, Some(node));
            node.set_container(ctx, Some(self));
        }

        Ok(())
    }

    /// 在链表后面添加节点
    /// # 参数列表
    /// - `ctx`: 获取数据的环境，如内存池（Arena）
    /// - `node`: 添加的节点
    ///
    /// # 返回值
    /// - [`Ok`]: 操作成功
    /// - [`LinkedListError::NodeAlreadyInContainer`]: 节点已存在链表中
    fn push_back(self, ctx: &mut Self::Ctx, node: Node) -> Result<(), LinkedListError<Node>> {
        if node.container(ctx).is_some() {
            return Err(LinkedListError::NodeAlreadyInContainer(node));
        }

        if let Some(tail) = self.tail(ctx) {
            tail.insert_after(ctx, node)
                .unwrap_or_else(|_| unreachable!());
        } else {
            self.set_head(ctx, Some(node));
            self.set_tail(ctx, Some(node));
            node.set_container(ctx, Some(self));
        }

        Ok(())
    }

    /// 将另外一个链表中的所有节点移动到当前链表
    /// # 参数列表
    /// - `ctx`: 获取数据的环境，如内存池（Arena）
    /// - `other`: 要排空的容器
    fn append(self, ctx: &mut Self::Ctx, other: Self) {
        let mut cur = other.head(ctx);
        while let Some(node) = cur {
            let succ = node.succ(ctx);
            node.unlink(ctx);
            self.push_back(ctx, node).unwrap_or_else(|_| unreachable!());
            cur = succ;
        }

        debug_assert!(other.head(ctx).is_none());
        debug_assert!(other.tail(ctx).is_none());
    }

    /// 在指定位置将链表分成两个链表，分开后指定位置节点属于第一个链表
    /// # 参数列表
    /// - `ctx`: 获取数据的环境，如内存池（Arena）
    /// - `other`: 另一个存链表的存储器
    /// - `pos`: 指定分离位置
    ///
    /// # 返回值
    /// - [`Ok`]：操作成功
    /// - [`LinkedListError::PositionNodeNotInContainer`]：节点不在容器中
    fn split(
        self,
        ctx: &mut Self::Ctx,
        other: Self,
        pos: Node,
    ) -> Result<(), LinkedListError<Node>> {
        if pos.container(ctx) != Some(self) {
            return Err(LinkedListError::PositionNodeNotInContainer(pos));
        }

        let mut curr = self.tail(ctx);
        while let Some(node) = curr {
            if node == pos {
                break;
            }
            let pre = node.pre(ctx);
            node.unlink(ctx);
            other
                .push_front(ctx, node)
                .unwrap_or_else(|_| unreachable!());
            curr = pre;
        }

        debug_assert!(self.tail(ctx) == Some(pos));
        Ok(())
    }

    /// 创建链表的迭代器
    /// # 参数列表
    /// - `ctx`: 获取数据的环境，如内存池（Arena）
    ///
    /// # Returns
    /// 创建的迭代器
    fn iter(self, ctx: &Self::Ctx) -> LinkedListIterator<Node> {
        LinkedListIterator {
            ctx,
            curr_forward: self.head(ctx),
            curr_backward: self.tail(ctx),
        }
    }

    /// 用迭代器中的节点扩展链表
    /// # 参数列表
    /// - `ctx`: 获取数据的环境，如内存池（Arena）
    /// - `iter`: 用来扩展的节点的迭代器
    ///
    /// # 返回值
    /// - [`Ok`]: 操作成功
    /// - [`LinkedListError`]: 操作错误
    fn extend<I: IntoIterator<Item = Node>>(
        self,
        ctx: &mut Self::Ctx,
        iter: I,
    ) -> Result<(), LinkedListError<Node>> {
        for node in iter {
            self.push_back(ctx, node)?;
        }
        Ok(())
    }

    /// 创建链表游标
    /// # 参数列表
    /// - `ctx`: 获取数据的环境，如内存池（Arena）
    /// - `strategy`: 获取下一个节点的游标策略
    ///
    /// # 返回值
    /// 创建的游标
    fn cursor(self, ctx: &Node::Ctx, strategy: CursorStrategy) -> LinkedListCursor<Node> {
        LinkedListCursor::new(self, ctx, strategy, CursorDirection::Forward)
    }
}

/// 链表节点操作方法
pub trait LinkedListNode: Copy + Eq {
    /// 存储链表的容器
    type Container: LinkedListContainer<Self, Ctx = Self::Ctx>;

    /// 访问数据的环境类型
    type Ctx;

    /// 获取后继节点
    fn succ(self, ctx: &Self::Ctx) -> Option<Self>;

    /// 获取前驱节点
    fn pre(self, ctx: &Self::Ctx) -> Option<Self>;

    /// 获取存储链表的容器
    fn container(self, ctx: &Self::Ctx) -> Option<Self::Container>;

    /// 设置后继节点
    fn set_succ(self, ctx: &mut Self::Ctx, succ: Option<Self>);

    /// 设置前驱节点
    fn set_pre(self, ctx: &mut Self::Ctx, pre: Option<Self>);

    /// 创建一个容器存储链表
    fn set_container(self, ctx: &mut Self::Ctx, container: Option<Self::Container>);

    /// 在这个节点后插入节点
    /// # 参数列表
    /// - `ctx`: 获取数据的环境，如内存池（Arena）
    /// - `node`: 被插入的节点
    ///
    /// # 返回值
    /// - [`Ok`]: 插入成功
    /// - [`LinkedListError::CurrentNodeNotLinked`]: 当前节点不在链表中
    /// - [`LinkedListError::NodeAlreadyInContainer`]：待插入的节点已经在链表中
    fn insert_after(self, ctx: &mut Self::Ctx, node: Self) -> Result<(), LinkedListError<Self>> {
        if self.container(ctx).is_none() {
            return Err(LinkedListError::CurrentNodeNotLinked(self));
        }

        if node.container(ctx).is_some() {
            return Err(LinkedListError::NodeAlreadyInContainer(node));
        }

        if let Some(succ) = self.succ(ctx) {
            succ.set_pre(ctx, Some(node));
            node.set_succ(ctx, Some(succ));
        }

        node.set_pre(ctx, Some(self));
        self.set_succ(ctx, Some(node));

        match self.container(ctx) {
            Some(container) => {
                if container.tail(ctx) == Some(self) {
                    container.set_tail(ctx, Some(node));
                }
            }
            None => unreachable!(),
        }
        node.set_container(ctx, self.container(ctx));

        Ok(())
    }

    /// 在借钱前面插入一个节点
    /// # 参数列表
    /// - `ctx`: 获取数据的环境，如内存池（Arena）
    /// - `node`: 被插入的节点
    ///
    /// # 返回值
    /// - [`Ok`]: 插入成功
    /// - [`LinkedListError::CurrentNodeNotLinked`]: 当前节点不在链表中
    /// - [`LinkedListError::NodeAlreadyInContainer`]：待插入的节点已经在链表中
    fn insert_before(self, ctx: &mut Self::Ctx, node: Self) -> Result<(), LinkedListError<Self>> {
        if self.container(ctx).is_none() {
            return Err(LinkedListError::CurrentNodeNotLinked(self));
        }

        if node.container(ctx).is_some() {
            return Err(LinkedListError::NodeAlreadyInContainer(node));
        }

        if let Some(pre) = self.pre(ctx) {
            pre.set_succ(ctx, Some(node));
            node.set_pre(ctx, Some(pre));
        }

        node.set_succ(ctx, Some(self));
        self.set_pre(ctx, Some(node));

        match self.container(ctx) {
            Some(container) => {
                if container.head(ctx) == Some(self) {
                    container.set_head(ctx, Some(node));
                }
            }
            None => unreachable!(),
        }

        node.set_container(ctx, self.container(ctx));

        Ok(())
    }

    /// 取消该节点的链接，但不会冲arena中移除
    /// # Parameters
    /// - `ctx`: 获取数据的环境，如内存池（Arena）
    fn unlink(self, ctx: &mut Self::Ctx) {
        let pre = self.pre(ctx);
        let succ = self.succ(ctx);

        if let Some(pre) = pre {
            pre.set_succ(ctx, succ);
        }

        if let Some(succ) = succ {
            succ.set_pre(ctx, pre);
        }

        if let Some(container) = self.container(ctx) {
            if container.head(ctx) == Some(self) {
                container.set_head(ctx, succ);
            }

            if container.tail(ctx) == Some(self) {
                container.set_tail(ctx, pre);
            }
        }

        self.set_pre(ctx, None);
        self.set_succ(ctx, None);

        self.set_container(ctx, None);
    }

    /// 在这个节点后扩展链表
    /// # 参数列表
    /// - `ctx`: 获取数据的环境，如内存池（Arena）
    /// - `iter`: 扩展的迭代器
    ///
    /// # 返回值
    /// - [`Ok`]：扩展成功
    /// - [`LinkedListError`]: 扩展失败
    fn extend_after<I>(self, ctx: &mut Self::Ctx, iter: I) -> Result<(), LinkedListError<Self>>
    where
        I: IntoIterator<Item = Self>,
    {
        let mut last = self;
        for node in iter {
            last.insert_after(ctx, node)?;
            last = node;
        }
        Ok(())
    }

    /// 在这个节点前扩展链表，不改变本来的节点顺序
    /// # 参数列表
    /// - `ctx`: 获取数据的环境，如内存池（Arena）
    /// - `iter`: 扩展的迭代器
    ///
    /// # 返回值
    /// - [`Ok`]：扩展成功
    /// - [`LinkedListError`]: 扩展失败
    fn extend_before<I>(self, ctx: &mut Self::Ctx, iter: I) -> Result<(), LinkedListError<Self>>
    where
        I: IntoIterator<Item = Self>,
    {
        for node in iter {
            self.insert_before(ctx, node)?;
        }
        Ok(())
    }
}

///链表的双端迭代器
/// # Lifetimes
/// - `a`: 迭代器生命周期
///
/// # 参数类型
/// - `T`: 链表节点类型
pub struct LinkedListIterator<'a, T: LinkedListNode> {
    ctx: &'a T::Ctx,
    curr_forward: Option<T>,
    curr_backward: Option<T>,
}

impl<'a, T: LinkedListNode> Iterator for LinkedListIterator<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let curr = self.curr_forward;
        self.curr_forward = curr.and_then(|node| node.succ(self.ctx));
        curr
    }
}

impl<'a, T: LinkedListNode> DoubleEndedIterator for LinkedListIterator<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let curr = self.curr_backward;
        self.curr_backward = curr.and_then(|node| node.pre(self.ctx));
        curr
    }
}

/// 游标获取下一个节点的策略
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CursorStrategy {
    /// 先获取下一个节点，再访问当前节点
    Pre,

    /// 先访问当前节点，再获取下一个节点。第一个节点并不会在构建游标时被立即获取，而是在调用 LinkedListCursor::succ 方法时获取。
    Post,
}

/// 链表游标移动的方向
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CursorDirection {
    /// 正向移动
    Forward,
    /// 反向移动
    Backward,
}

/// 链表的游标
/// # 参数类型
/// - `T`: 链表节点的类型
pub struct LinkedListCursor<T: LinkedListNode> {
    /// 链表的容器
    container: T::Container,

    /// 当前的链表节点，对于Pre，这是下一个要访问的节点，对于Post，这是当前节点
    curr: Option<T>,

    /// 当前游标策略
    strategy: CursorStrategy,

    /// 当前游标方向
    direction: CursorDirection,

    ///正向迭代结束的指示器
    done: bool,
}

impl<T> LinkedListCursor<T>
where
    T: LinkedListNode,
{
    fn new(
        container: T::Container,
        ctx: &T::Ctx,
        strategy: CursorStrategy,
        direction: CursorDirection,
    ) -> Self {
        let mut cursor = Self {
            container,
            curr: None,
            strategy,
            direction,
            done: false,
        };

        match cursor.strategy {
            CursorStrategy::Pre => match cursor.direction {
                CursorDirection::Forward => cursor.curr = cursor.container.head(ctx),
                CursorDirection::Backward => cursor.curr = cursor.container.tail(ctx),
            },
            CursorStrategy::Post => {}
        }

        cursor
    }

    /// 反转游标方向
    pub fn rev(mut self, ctx: &T::Ctx) -> Self {
        self.direction = match self.direction {
            CursorDirection::Forward => CursorDirection::Backward,
            CursorDirection::Backward => CursorDirection::Forward,
        };

        self.done = false;

        match self.strategy {
            CursorStrategy::Pre => match self.direction {
                CursorDirection::Forward => self.curr = self.container.head(ctx),
                CursorDirection::Backward => self.curr = self.container.tail(ctx),
            },
            CursorStrategy::Post => self.curr = None,
        }

        self
    }

    /// 迭代结束
    pub fn is_done(&self) -> bool {
        self.done
    }

    /// 移动游标访问下一个节点
    pub fn succ(&mut self, ctx: &T::Ctx) -> Option<T> {
        match self.strategy {
            CursorStrategy::Pre => {
                let curr = self.curr;
                match curr {
                    Some(curr) => match self.direction {
                        CursorDirection::Forward => self.curr = curr.succ(ctx),
                        CursorDirection::Backward => self.curr = curr.pre(ctx),
                    },
                    None => self.done = true,
                }
                curr
            }
            CursorStrategy::Post => {
                match self.curr {
                    Some(curr) => match self.direction {
                        CursorDirection::Forward => self.curr = curr.succ(ctx),
                        CursorDirection::Backward => self.curr = curr.pre(ctx),
                    },
                    None if self.done => return None,

                    // 初始状态
                    None => match self.direction {
                        CursorDirection::Forward => self.curr = self.container.head(ctx),
                        CursorDirection::Backward => self.curr = self.container.tail(ctx),
                    },
                }
                self.done = self.curr.is_none();
                self.curr
            }
        }
    }

    /// 使用闭包迭代链表
    /// # 参数列表
    /// - `ctx`: 获取数据的环境，如内存池（Arena）
    /// - `f`: 每个节点的闭包
    pub fn for_each<F: FnMut(&mut T::Ctx, T)>(self, ctx: &mut T::Ctx, mut f: F) {
        let mut cursor = self;
        while let Some(node) = cursor.succ(ctx) {
            f(ctx, node);
        }
    }
}

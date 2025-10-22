use core::panic;

/// Loop Analysis Pass
///
use rustc_hash::FxHashMap as HashMap;
use rustc_hash::FxHashSet as HashSet;

use crate::frontend::ir::basicblock::BasicBlock;
use crate::frontend::ir::defuse::Useable;
use crate::frontend::ir::instruction::Instruction;
use crate::frontend::ir::value::Value;
use crate::utils::linked_list::LinkedListNode;
use crate::utils::storage::Idx;
use crate::{
    frontend::ir::{context::Context, function::Function},
    passes::{
        pass::structure::{ControlFlowAnalysisResult, DominatorInfo, Loop, LoopInfo},
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::LinkedListContainer,
};

pub struct LoopAnalysis;

impl Pass for LoopAnalysis {
    type Output = bool;
    type Error = PassError;

    fn pass_type(&self) -> PassType {
        PassType::FunctionBasedAnalysis
    }

    /// 执行遍处理
    fn execute(
        &mut self,
        input: &mut Context,
        pctx: &mut PassContext,
    ) -> Result<bool, Self::Error> {
        pctx.loop_info.clear();
        let mut changed = false;
        let functions: Vec<_> = input.get_functions().collect(); // 收集所有函数
        for function in functions {
            let dom_info = pctx.dom_info.get(&function.get_id(input)).unwrap();
            let res = self.execute_function(
                &function,
                input,
                pctx.cfginfo.get(&function.get_id(input)).unwrap(),
                dom_info,
            );
            match res {
                Ok((tmp, floop_info)) => {
                    changed |= tmp;
                    pctx.loop_info.insert(function.get_id(input), floop_info);
                }
                Err(e) => return Err(e),
            }
        }
        Ok(changed)
    }

    /// 获取遍名称（用于依赖管理）
    fn name(&self) -> &'static str {
        "LoopAnalysis"
    }

    /// 声明依赖的遍
    fn dependencies(&self) -> &[&'static str] {
        &["CFAnalysis"]
    }

    fn effects(&self) -> &[&'static str] {
        &[]
    }
}

impl LoopAnalysis {
    pub fn execute_function(
        &mut self,
        input: &Function,
        ctx: &mut Context,
        cfg: &ControlFlowAnalysisResult,
        dom_info: &DominatorInfo,
    ) -> Result<(bool, LoopInfo), PassError> {
        let changed = false;
        if let Some(_entry) = input.head(ctx) {
            // 创建控制流图
            let mut loop_info = LoopInfo {
                loops: Vec::new(),
                id2loop: HashMap::default(),
                roots: Vec::new(),
            };

            // 识别循环
            self.detect_loops(&mut loop_info, dom_info, cfg);

            Ok((changed, loop_info))
        } else {
            let loop_info = LoopInfo {
                loops: Vec::new(),
                id2loop: HashMap::default(),
                roots: Vec::new(),
            };
            // 如果没有入口基本块，直接返回
            return Ok((changed, loop_info));
        }
    }
}

impl LoopAnalysis {
    fn detect_loops(
        &self,
        loop_info: &mut LoopInfo,
        dom_info: &DominatorInfo,
        cfg: &ControlFlowAnalysisResult,
    ) {
        let back_edges = self.find_back_edges(dom_info, cfg);
        let mut loops = self.build_loops_from_back_edges(&back_edges, cfg);
        self.establish_parent_child_relationships(&mut loops);
        self.compute_loop_depths(&mut loops);
        self.populate_loop_info(loop_info, &loops);
    }

    fn find_back_edges(
        &self,
        dom_info: &DominatorInfo,
        cfg: &ControlFlowAnalysisResult,
    ) -> Vec<(BasicBlock, BasicBlock)> {
        let mut back_edges = Vec::new();
        for (&block, successors) in &cfg.cfg.succ_set {
            for &successor in successors {
                if dom_info.dominators[&block].contains(&successor) {
                    back_edges.push((block, successor));
                }
            }
        }
        back_edges
    }

    fn build_loops_from_back_edges(
        &self,
        back_edges: &[(BasicBlock, BasicBlock)],
        cfg: &ControlFlowAnalysisResult,
    ) -> Vec<Loop<BasicBlock>> {
        let mut next_id = 0;
        let mut header_to_loop: HashMap<BasicBlock, usize> = HashMap::default();
        let mut loops: Vec<Loop> = Vec::new();

        for &(tail, header) in back_edges {
            if let Some(loop_idx) = header_to_loop.get(&header) {
                loops[*loop_idx].back_edges.push((tail, header));
                let loop_body = self.build_loop_body(tail, header, cfg);
                loops[*loop_idx].body.extend(loop_body);
                continue;
            }

            let loop_body = self.build_loop_body(tail, header, cfg);
            let new_loop = Loop {
                id: next_id,
                preheader: None,
                header,
                parent: None,
                children: Vec::new(),
                body: loop_body,
                back_edges: vec![(tail, header)],
                depth: 0,
            };

            header_to_loop.insert(header, loops.len());
            loops.push(new_loop);
            next_id += 1;
        }
        for loop_ in &mut loops {
            loop_.preheader = Self::find_preheader(loop_, cfg);
        }
        loops
    }

    fn build_loop_body(
        &self,
        tail: BasicBlock,
        header: BasicBlock,
        cfg: &ControlFlowAnalysisResult,
    ) -> HashSet<BasicBlock> {
        let mut loop_body = HashSet::default();
        let mut worklist = vec![tail];
        loop_body.insert(header);

        while let Some(current) = worklist.pop() {
            if current == header || loop_body.contains(&current) {
                continue;
            }

            loop_body.insert(current);

            if let Some(preds) = cfg.cfg.pre_set.get(&current) {
                for &pred in preds {
                    worklist.push(pred);
                }
            }
        }

        loop_body
    }

    fn establish_parent_child_relationships(&self, loops: &mut Vec<Loop<BasicBlock>>) {
        for i in 0..loops.len() {
            let header = loops[i].header;
            let mut parent_id = None;

            for j in 0..loops.len() {
                if i == j {
                    continue;
                }

                if loops[j].body.contains(&header) {
                    if let Some(pid) = parent_id {
                        let p_loop = loops.iter().find(|l| l.id == pid).unwrap();
                        if loops[j].body.len() < p_loop.body.len() {
                            parent_id = Some(loops[j].id);
                        }
                    } else {
                        parent_id = Some(loops[j].id);
                    }
                }
            }

            loops[i].parent = parent_id;

            let cur_id = loops[i].id;
            if let Some(pid) = parent_id {
                if let Some(p_loop) = loops.iter_mut().find(|l| l.id == pid) {
                    p_loop.children.push(cur_id);
                }
            }
        }
    }

    fn compute_loop_depths(&self, loops: &mut Vec<Loop<BasicBlock>>) {
        // 初始化：将所有根循环深度设为1并加入队列
        let mut queue = std::collections::VecDeque::new();
        for loop_ in loops.iter_mut() {
            if loop_.parent.is_none() {
                loop_.depth = 1; // 根循环深度=1
                queue.push_back(loop_.id);
            }
        }

        // 层级遍历计算深度
        while let Some(loop_id) = queue.pop_front() {
            let depth = loops[loop_id as usize].depth;

            // 处理所有子循环
            for child_id in loops[loop_id as usize].children.clone() {
                let child = &mut loops[child_id as usize];
                child.depth = depth + 1; // 子循环深度=父深度+1
                queue.push_back(child_id);
            }
        }
    }

    fn populate_loop_info(&self, loop_info: &mut LoopInfo, loops: &Vec<Loop<BasicBlock>>) {
        let roots = loops
            .iter()
            .filter(|l| l.parent.is_none())
            .map(|l| l.id)
            .collect();

        *loop_info = LoopInfo {
            loops: loops.to_vec(),
            id2loop: HashMap::default(),
            roots,
        };
        for loop_ in &loop_info.loops {
            loop_info.id2loop.insert(loop_.id, loop_.clone());
        }
    }
}

impl LoopAnalysis {
    /// 查找基本归纳变量
    /// 只找形如：
    ///     induction_variable = phi(preheader_value preheader_bb, latch_value latch_bb)
    ///      ...Other Phis
    ///     cond = icmp/fcmp cmpop induction_variable, bountdary_value
    ///     br cond bod1_bb exit_bb
    /// 的循环的基本归纳变量
    /// 返回值：
    ///     (induction_variable, start_value, end_value, step_value ,latch_value)
    pub fn find_induction_variable(
        loop_: &Loop<BasicBlock>,
        function: &Function,
        ctx: &Context,
        pctx: &PassContext,
    ) -> Option<(
        crate::frontend::ir::value::Value,
        crate::frontend::ir::value::Value,
        crate::frontend::ir::value::Value,
        crate::frontend::ir::value::Value,
        crate::frontend::ir::value::Value,
    )> {
        let header = loop_.header;
        let latch = Self::find_loop_latch(loop_, pctx.cfginfo.get(&function.get_id(ctx)).unwrap())?;

        // 1. 在循环头中查找PHI节点
        let mut phis = Vec::new();
        let mut not_phis = Vec::new();
        let mut cur_bbk = header.head(ctx);
        while let Some(inst) = cur_bbk {
            if inst.is_phi(ctx) {
                phis.push(inst);
            } else {
                not_phis.push(inst);
            }
            cur_bbk = inst.succ(ctx);
        }
        if not_phis.len() != 2 {
            return None;
        }

        let (cmp, cbr) = (not_phis[0], not_phis[1]);
        if !cmp.is_cmp(ctx)
            || !cbr.is_cbr(ctx)
            || cmp.get_result(ctx).unwrap() != cbr.get_operand(ctx, 0).unwrap()
        {
            return None;
        }

        let (indution_var, end_value) = if cmp.get_operand(ctx, 0).unwrap().is_constant(ctx) {
            match (cmp.get_operand(ctx, 1), cmp.get_operand(ctx, 0)) {
                (Some(var), Some(val)) => (var, val),
                _ => return None,
            }
        } else if cmp.get_operand(ctx, 1).unwrap().is_constant(ctx) {
            match (cmp.get_operand(ctx, 0), cmp.get_operand(ctx, 1)) {
                (Some(var), Some(val)) => (var, val),
                _ => return None,
            }
        } else {
            return None;
        };
        if !phis.contains(&indution_var.get_instruction(ctx).unwrap()) {
            return None;
        } else {
            let phi = indution_var.get_instruction(ctx).unwrap();
            let (start_value, latch_value) = (
                phi.get_phi_operand(ctx, loop_.preheader.unwrap()),
                phi.get_phi_operand(ctx, latch),
            );
            if start_value.is_none()
                || latch_value.is_none()
                || !start_value.unwrap().is_constant(ctx)
            {
                return None;
            }
            let latch_inst = latch_value.unwrap().get_instruction(ctx).unwrap();
            let inst_kind = latch_inst.get_kind(ctx);
            match inst_kind {
                crate::frontend::ir::instruction::InstructionKind::Binary {
                    op: crate::frontend::ir::instruction::BinaryOp::Add,
                    ..
                }
                | crate::frontend::ir::instruction::InstructionKind::Binary {
                    op: crate::frontend::ir::instruction::BinaryOp::Sub,
                    ..
                } => {
                    let op0 = latch_inst.get_operand(ctx, 0).unwrap();
                    let op1 = latch_inst.get_operand(ctx, 1).unwrap();

                    if op0 == indution_var {
                        if op1.is_constant(ctx) {
                            return Some((
                                indution_var,
                                start_value.unwrap(),
                                end_value,
                                op1,
                                latch_value.unwrap(),
                            ));
                        }
                    } else if op1 == indution_var {
                        if op0.is_constant(ctx) {
                            return Some((
                                indution_var,
                                start_value.unwrap(),
                                end_value,
                                op0,
                                latch_value.unwrap(),
                            ));
                        }
                    }
                }
                _ => return None,
            };
            None
        }
    }

    /// 查找循环后沿
    pub fn find_loop_latch(
        loop_: &Loop<BasicBlock>,
        cfg: &ControlFlowAnalysisResult,
    ) -> Option<BasicBlock> {
        let header = loop_.header;

        if let Some(predecessors) = cfg.cfg.pre_set.get(&header) {
            for &pred in predecessors {
                if loop_.body.contains(&pred) {
                    return Some(pred);
                }
            }
        }

        None
    }

    /// 查找循环前置头块
    pub fn find_preheader(
        loop_: &Loop<BasicBlock>,
        cfg: &ControlFlowAnalysisResult,
    ) -> Option<BasicBlock> {
        let header = loop_.header;

        cfg.cfg
            .pre_set
            .get(&header)?
            .iter()
            .find(|&&pred| {
                // Preheader的条件：唯一后继是循环头
                let succs = cfg.cfg.succ_set.get(&pred).unwrap();
                succs.len() == 1 && succs.contains(&header) && !loop_.body.contains(&pred)
            })
            .copied()
    }

    /// 查找循环出口块
    pub fn find_loop_exit(loop_: &Loop<BasicBlock>, ctx: &Context) -> Vec<BasicBlock> {
        let mut exit_blocks = Vec::new();
        for bbk in &loop_.body {
            let inst = bbk.tail(ctx).unwrap();
            if inst.is_br(ctx) {
                let target = inst.get_operand_bbk(ctx, 0).unwrap();
                if !loop_.body.contains(&target) {
                    exit_blocks.push(target);
                }
            } else if inst.is_cbr(ctx) {
                let target1 = inst.get_operand_bbk(ctx, 0).unwrap();
                let target2 = inst.get_operand_bbk(ctx, 1).unwrap();
                if !loop_.body.contains(&target1) {
                    exit_blocks.push(target1);
                }
                if !loop_.body.contains(&target2) {
                    exit_blocks.push(target2);
                }
            }
        }
        exit_blocks
    }

    /// 判断指令是否为循环不变量
    pub fn is_invariant(
        function: &Function,
        inst: crate::frontend::ir::instruction::Instruction,
        ctx: &Context,
        loop_: &Loop<BasicBlock>,
        pctx: &PassContext,
    ) -> bool {
        // 检查所有操作数是否循环不变量
        inst.get_operands(ctx)
            .iter()
            .all(|op| Self::is_invariant_operand(function, *op, ctx, loop_, pctx))
    }

    /// 检查操作数是否循环不变量
    pub fn is_invariant_operand(
        _function: &Function,
        operand: crate::frontend::ir::value::Value,
        ctx: &Context,
        loop_: &Loop<BasicBlock>,
        _pctx: &PassContext,
    ) -> bool {
        let value_kind = &crate::utils::storage::ArenaPtr::deref(operand, ctx)
            .unwrap()
            .kind;
        match value_kind {
            crate::frontend::ir::value::ValueKind::InstResult {
                instruction,
                typ: _,
            } => {
                // 检查指令是否在循环中
                instruction.get_basicblock(ctx).is_none()
                    || !loop_
                        .body
                        .contains(&instruction.get_basicblock(ctx).unwrap())
            }
            crate::frontend::ir::value::ValueKind::Parameter {
                function: _,
                index: _,
                typ: _,
            } => true, // 理论上参数不会在函数中进行定值
            crate::frontend::ir::value::ValueKind::Constant { value: _ } => true,
            crate::frontend::ir::value::ValueKind::Function {
                function: _,
                ret_type: _,
            } => true, //call指令的第一个参数，不排除call指令有副作用，副作用在has_side_effects中判断
        }
    }

    /// 检查指令是否有副作用
    pub fn has_side_effects(
        inst: crate::frontend::ir::instruction::Instruction,
        ctx: &Context,
    ) -> bool {
        match inst.get_kind(ctx) {
            crate::frontend::ir::instruction::InstructionKind::MemAccess {
                //别名分析之前只能假定有副作用
                op: crate::frontend::ir::instruction::MemAccessOp::Store,
            } => true,
            crate::frontend::ir::instruction::InstructionKind::MemAccess {
                // 别名分析之前只能假定有副作用
                op: crate::frontend::ir::instruction::MemAccessOp::Load,
            } => true,
            crate::frontend::ir::instruction::InstructionKind::Call { .. } => {
                // for operand in inst.get_operands(ctx) {
                //     if operand.is_ptr(ctx)
                // }
                true
            }
            _ => false,
        }
    }

    pub fn is_dom_use(
        inst: &crate::frontend::ir::instruction::Instruction,
        ctx: &Context,
        loop_: &Loop<BasicBlock>,
        dom_info: &DominatorInfo,
    ) -> bool {
        let res = inst.get_result(ctx);
        let def_bbk = inst.get_basicblock(ctx).unwrap();
        if let Some(res) = res {
            for user in res.users(ctx) {
                if user.get_index() == 0 {
                    continue;
                } else {
                    let user_inst = user.get_instruction();
                    let use_bbk = user_inst.get_basicblock(ctx).unwrap();
                    if loop_.body.contains(&use_bbk) {
                        if !dom_info.dominators[&use_bbk].contains(&def_bbk) {
                            return false;
                        }
                    }
                }
            }
        } else {
            return false; // 暂时不考虑对call和store的外提
            // panic!(
            //     "LICM: inst [{}] has no result: TO DO",
            //     crate::frontend::ir2string::Display::display(*inst, ctx)
            // );
        }
        true
    }

    pub fn is_dom_exits(
        inst: &crate::frontend::ir::instruction::Instruction,
        ctx: &Context,
        loop_: &Loop<BasicBlock>,
        dom_info: &DominatorInfo,
    ) -> bool {
        let def_bbk = inst.get_basicblock(ctx).unwrap();
        let exit_bbks = LoopAnalysis::find_loop_exit(loop_, ctx);
        for exit_bbk in exit_bbks {
            if !dom_info.dominators[&exit_bbk].contains(&def_bbk) {
                return false;
            }
        }
        true
    }

    /// 计算迭代次数
    pub fn calculate_trip_count(
        start: Value,
        end: Value,
        step: Value,
        cmp: &crate::frontend::ir::instruction::Instruction,
        ctx: &Context,
    ) -> Option<u32> {
        let cmp_op = match cmp.get_kind(ctx) {
            crate::frontend::ir::instruction::InstructionKind::IComp { cond } => match cond {
                crate::frontend::ir::instruction::ICompCond::Eq => {
                    panic!("[Unsupport]A While loop should not have a equal condition");
                }
                crate::frontend::ir::instruction::ICompCond::Ne => 0,
                crate::frontend::ir::instruction::ICompCond::Ugt => 0,
                crate::frontend::ir::instruction::ICompCond::Uge => 1,
                crate::frontend::ir::instruction::ICompCond::Ult => 0,
                crate::frontend::ir::instruction::ICompCond::Ule => 1,
                crate::frontend::ir::instruction::ICompCond::Sgt => 0,
                crate::frontend::ir::instruction::ICompCond::Sge => 1,
                crate::frontend::ir::instruction::ICompCond::Slt => 0,
                crate::frontend::ir::instruction::ICompCond::Sle => 1,
            },
            crate::frontend::ir::instruction::InstructionKind::FComp { cond } => match cond {
                crate::frontend::ir::instruction::FCompCond::False => {
                    panic!("[Unsupport]A While loop should not have a false condition")
                }
                crate::frontend::ir::instruction::FCompCond::True => {
                    panic!("[Unsupport]A While loop should not have a true condition")
                }
                crate::frontend::ir::instruction::FCompCond::Oeq => {
                    panic!("[Unsupport]A While loop should not have a ordered equal condition")
                }
                crate::frontend::ir::instruction::FCompCond::One => 0,
                crate::frontend::ir::instruction::FCompCond::Ogt => 0,
                crate::frontend::ir::instruction::FCompCond::Oge => 1,
                crate::frontend::ir::instruction::FCompCond::Olt => 0,
                crate::frontend::ir::instruction::FCompCond::Ole => 1,
                crate::frontend::ir::instruction::FCompCond::Ord => {
                    panic!("[Unsupport]A While loop should not have a ordered condition")
                }
                crate::frontend::ir::instruction::FCompCond::Ueq => {
                    panic!("[Unsupport]A While loop should not have a unordered equal condition")
                }
                crate::frontend::ir::instruction::FCompCond::Une => 0,
                crate::frontend::ir::instruction::FCompCond::Ugt => 0,
                crate::frontend::ir::instruction::FCompCond::Uge => 1,
                crate::frontend::ir::instruction::FCompCond::Ult => 0,
                crate::frontend::ir::instruction::FCompCond::Ule => 1,
                crate::frontend::ir::instruction::FCompCond::Uno => {
                    panic!("[Unsupport]A While loop should not have a unordered condition")
                }
            },
            _ => panic!("Loop condition should be a compare instruction"),
        };

        // 简化实现：仅处理常量边界
        match (
            start.get_int_const_value(ctx),
            end.get_int_const_value(ctx),
            step.get_int_const_value(ctx),
        ) {
            (Some(start_val), Some(end_val), Some(step_val)) => {
                if start_val <= end_val && step_val > 0 {
                    let n = end_val - start_val + cmp_op;
                    let mut count: u32 = (n / step_val).try_into().unwrap();
                    if n % step_val != 0 {
                        count += 1;
                    }
                    return Some(count);
                } else if start_val >= end_val && step_val < 0 {
                    let n = start_val - end_val + cmp_op;
                    let mut count: u32 = (n / (-step_val)).try_into().unwrap();
                    if n % (-step_val) != 0 {
                        count += 1;
                    }
                    return Some(count);
                } else {
                    return None;
                }
            }
            _ => return None,
        }
    }

    /// 获取循环边界信息
    pub fn get_loop_bounds(
        loop_: &Loop<BasicBlock>,
        function: &Function,
        ctx: &Context,
        pctx: &PassContext,
    ) -> Option<(Value, Value)> {
        let header = loop_.header;
        let terminator = header.tail(ctx)?;

        // 检查终止指令是否为条件分支
        if !terminator.is_cbr(ctx) {
            return None;
        }

        // 获取归纳变量
        let (induction_var, _, _, _, latch_val) =
            Self::find_induction_variable(loop_, function, ctx, pctx)?;

        let cbr = loop_.header.tail(ctx).unwrap();
        if !cbr.is_cbr(ctx) {
            panic!(
                "Loop header donot have a condition branch instruction: {}",
                crate::frontend::ir2string::Display::display(cbr, ctx)
            );
        }

        let mut cmp = None;
        for user in induction_var.users(ctx) {
            if user.get_instruction().is_cmp(ctx) {
                let cmp_inst = user.get_instruction();
                let res = cmp_inst.get_operand(ctx, 0).unwrap();
                if res == cbr.get_operand(ctx, 0).unwrap() {
                    cmp = Some(cmp_inst);
                }
            }
        }

        if cmp.is_none() {
            //如果基本归纳变量经过复杂计算才用于比较，处理繁琐，先不进行处理
            return None;
        } else {
            // 获取循环边界
            Self::extract_loop_bounds(
                induction_var,
                latch_val.get_instruction(ctx).unwrap(),
                cmp.unwrap(),
                ctx,
            )
        }
    }

    /// 提取循环边界信息
    /// induction_var = phi typ [start_val, pre_header], [latch_val, latch_bbk]
    /// ??? Other Induction
    /// cond = cmp typ [induction_var, bound_val]
    /// br cond loop_bbk exit_bbk or br cond exit_bbk loop_bbk
    pub fn extract_loop_bounds(
        induction_var: Value,
        latch_inst: Instruction,
        cmp: Instruction,
        ctx: &Context,
    ) -> Option<(Value, Value)> {
        // 获取比较操作数
        let op1 = cmp.get_operand(ctx, 0).unwrap();
        let op2 = cmp.get_operand(ctx, 1).unwrap();
        let latch_val = latch_inst.get_result(ctx).unwrap();

        let mut start_val = None;
        if let crate::frontend::ir::value::ValueKind::InstResult {
            instruction: phi, ..
        } = crate::utils::storage::ArenaPtr::deref(induction_var, ctx)
            .unwrap()
            .kind
        {
            if phi.is_phi(ctx) && phi.get_phi(ctx).len() == 2 {
                let ops_phi = phi.get_phi(ctx);
                let ops = ops_phi.values().collect::<Vec<_>>();
                start_val = if *ops[0] == latch_val {
                    Some(*ops[1])
                } else if *ops[1] == latch_val {
                    Some(*ops[0])
                } else {
                    return None;
                };
            } else {
                panic!(
                    "Loop Induction variable [{}] is not a valid PHI node [{}]",
                    crate::frontend::ir2string::Display::display(latch_val, ctx),
                    crate::frontend::ir2string::Display::display(phi, ctx)
                );
            }
        }
        if start_val.is_none() {
            return None;
        }

        // 确定哪个操作数是归纳变量
        let bound_val = if op1 == induction_var {
            op2
        } else if op2 == induction_var {
            op1
        } else {
            return None;
        };

        Some((start_val.unwrap(), bound_val))
    }
}

impl LoopInfo {
    /// 打印循环信息
    pub fn print_loop_info(&self, _ctx: &Context) {
        println!("···[Loops]:");
        for loop_ in &self.loops {
            println!("······Loop ID: {}", loop_.id);
            if loop_.preheader.is_some() {
                println!("      Preheader: bb_{}", loop_.preheader.unwrap().0.index());
            }
            println!("      Header:  bb_{}", loop_.header.0.index());
            println!(
                "      Body:    {}",
                loop_
                    .body
                    .iter()
                    .map(|bb| format!("bb_{}", bb.0.index()))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            println!("      Depth:   {}", loop_.depth);
            println!(
                "      Back edges:  {}",
                loop_
                    .back_edges
                    .iter()
                    .map(|(src, dst)| format!("bb_{} -> bb_{}", src.0.index(), dst.0.index()))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
            if loop_.parent.is_some() {
                println!("      Parent:  {}", loop_.parent.unwrap());
            }
            print!("      Children:");
            for child in &loop_.children {
                print!("  {}", child);
            }
            println!();
        }
        print!("···[Roots]:");
        for root in &self.roots {
            print!("  {}", root);
        }
        println!();
    }
}

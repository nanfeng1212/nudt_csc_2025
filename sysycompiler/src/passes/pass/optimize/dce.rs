use crate::{
    frontend::ir::context::Context,
    passes::{
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::LinkedListContainer,
};

pub struct DCE;

impl Pass for DCE {
    type Output = bool; // 输出数据类型
    type Error = PassError; // 错误类型

    /// 遍类型
    fn pass_type(&self) -> PassType {
        PassType::GlobalBasedOptimization
    }

    /// 执行遍处理
    fn execute(&mut self, input: &mut Context, pctx: &mut PassContext) -> Result<bool, PassError> {
        let mut to_remove = Vec::new();
        for func in input.get_functions() {
            for bbk in func.iter(input) {
                for inst in bbk.iter(input) {
                    if !pctx.live_insts.contains(&inst) {
                        // println!(
                        //     "To Remove: {}",
                        //     crate::frontend::ir2string::Display::display(inst, input)
                        // );
                        to_remove.push(inst);
                    }
                }
            }
        }
        pctx.live_insts.clear();
        for inst in &to_remove {
            // println!(
            //     "Removing instruction: {}",
            //     crate::frontend::ir2string::Display::display(*inst, input)
            // );
            inst.remove(input);
        }

        #[cfg(debug_assertions)]
        println!("···[DCE]Removed {} instructions", to_remove.len());
        Ok(true)
    }

    /// 获取遍名称（用于依赖管理）
    fn name(&self) -> &'static str {
        "GlobalDeadCodeElimination"
    }

    /// 声明依赖的遍
    fn dependencies(&self) -> &[&'static str] {
        &["CFAnalysis", "LiveInstAnalysis"]
    }

    fn effects(&self) -> &[&'static str] {
        &[]
    }
}

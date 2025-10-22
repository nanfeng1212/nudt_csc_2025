// use crate::passes::pass_manager::{Pass, PassError};

// /// Dead Phi Elimination (DPE) Pass
// ///
// /// This pass eliminates dead phi nodes by replacing them with their incoming values.

// pub struct DPE;

// impl Pass for DPE {
//     type Output = bool;

//     type Error = PassError;

//     fn pass_type(&self) -> crate::passes::pass_manager::PassType {
//         todo!()
//     }

//     fn execute(
//         &mut self,
//         ctx: &mut crate::frontend::ir::context::Context,
//         pctx: &mut crate::passes::pass_context::PassContext,
//     ) -> Result<Self::Output, Self::Error> {
//         let mut changed = false;

//         Ok(changed)
//     }

//     fn name(&self) -> &'static str {
//         "DeadPhiElimination"
//     }

//     fn dependencies(&self) -> &[&'static str] {
//         &[]
//     }

//     fn effects(&self) -> &[&'static str] {
//         &[]
//     }
// }

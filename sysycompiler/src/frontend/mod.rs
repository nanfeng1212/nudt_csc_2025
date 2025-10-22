pub mod ir;
pub mod ir2string;
pub mod irgen;
pub mod lalrpop;
pub mod symboltable;
pub mod typecheck;

pub fn irgen(ast: &mut lalrpop::ast::CompUnit, ptr_width: u8) -> ir::context::Context {
    let mut irgen = irgen::IrGenContext::default();

    irgen.ctx.set_target(ptr_width as u32);

    crate::frontend::lalrpop::ast::CompUnit::type_check(ast);

    <crate::frontend::lalrpop::ast::CompUnit as irgen::IrGen>::irgen(ast, &mut irgen);

    irgen.context()
}

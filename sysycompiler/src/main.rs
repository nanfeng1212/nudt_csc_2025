use crate::passes::pass_manager::PipLine;
use lalrpop_util::lalrpop_mod;
use passes::pass;
use rustc_hash::FxHashMap as HashMap;
use std::io::Result;

pub mod backend;
pub mod frontend;
pub mod passes;
pub mod utils;

//指定生成的代码的位置
lalrpop_mod!(#[allow(clippy::all)] pub sysy, "/frontend/lalrpop/sysy.rs");

/// 解析命令行参数
fn parse_arguments() -> HashMap<String, String> {
    let mut args_map = HashMap::default();
    let args: Vec<String> = std::env::args().collect();
    let mut i = 0;
    while i < args.len() {
        let arg = args[i].as_str();
        if arg == "-ast" {
            args_map.insert(String::from("ast"), args[i + 1].to_string());
            i += 1;
        } else if arg == "-ir" {
            args_map.insert(String::from("ir"), args[i + 1].to_string());
            i += 1;
        } else if arg == "-asm" {
            args_map.insert(String::from("asm"), args[i + 1].to_string());
            i += 1;
        } else if arg == "-S" {
            args_map.insert(String::from("S"), String::from("true"));
        } else if arg == "-o" {
            args_map.insert(String::from("o"), args[i + 1].to_string());
            i += 1;
        } else if arg == "-O1" {
            args_map.insert(String::from("O1"), String::from("true"));
        } else if arg == "-h" {
            println!("[Compiler of Syntax干饭人] Usage:");
            println!("[Compiler of Syntax干饭人]    compiler [options] source_file");
            println!("[Compiler of Syntax干饭人] Options:");
            println!("[Compiler of Syntax干饭人]    -ast <file>  : Output the AST to a file");
            println!("[Compiler of Syntax干饭人]    -ir <file>   : Output the IR to a file");
            println!(
                "[Compiler of Syntax干饭人]    -asm <file>  : Output the ASM to a file( Has higher priority than -S!!! )"
            );
            println!("[Compiler of Syntax干饭人]    -S           : Output the ASM to stdout");
            println!("[Compiler of Syntax干饭人]    -o <file>    : Output the ASM to a file");
            println!("[Compiler of Syntax干饭人]    -O1          : Enable optimization level 1");
            std::process::exit(0);
        } else {
            args_map.insert(String::from("source"), arg.to_string());
        }
        i += 1;
    }
    args_map
}

/// 检查命令行参数
fn check_args(args: &HashMap<String, String>) {
    let emit_s = if args.get("S").is_some() && args.get("S").unwrap() == "true" {
        true
    } else {
        false
    };
    let source = args.get("source");
    let output_file = args.get("o");
    if source.is_none() {
        panic!("Error: Source file `{}` not specified", source.unwrap());
    }
    if output_file.is_none() && emit_s {
        panic!("Error: Output file not specified");
    }
    if !std::path::Path::new(source.unwrap()).exists() {
        panic!("Error: Source file `{}` not found", source.unwrap());
    }
}

/// 加入自定义库：memset
fn insert_my_lib(src: &mut String) {
    *src += crate::frontend::ir::my_lib::MEMSET_SYSY;
}

/// 注册IR相关的pass
fn register_ir_passes(pm: &mut passes::pass_manager::PassManager) {
    // Analysis passes
    pm.register_pass(pass::analysis::cfa::CFAnalysis);
    pm.register_pass(pass::analysis::ltdomra::LTDomRelAnalysis);
    pm.register_pass(pass::analysis::live_inst::LiveInstAnalysis);
    pm.register_pass(pass::analysis::fca::FunctionCallAnalysis);
    pm.register_pass(pass::analysis::loopa::LoopAnalysis);
    pm.register_pass(pass::analysis::alias::AliasAnalysis);

    // Optimization passes
    // scalar optimization passes
    pm.register_pass(pass::optimize::gtl::GlobalToLocal);
    pm.register_pass(pass::optimize::pre_mem2reg::PreMem2Reg);
    pm.register_pass(pass::optimize::mem2reg::Mem2Reg);
    pm.register_pass(pass::optimize::reg2mem::Reg2Mem);
    pm.register_pass(pass::optimize::dce::DCE);
    pm.register_pass(pass::optimize::sccp::SCCP);
    // cfg optimization passes
    pm.register_pass(pass::optimize::ube::UBE);
    pm.register_pass(pass::optimize::cie::CIE);
    pm.register_pass(pass::optimize::lbc::LinearBasicBlockCoalescing);
    // func optimization passes
    pm.register_pass(pass::optimize::tco::TailCallOptimization);
    pm.register_pass(pass::optimize::inline::Inline);
    pm.register_pass(pass::optimize::dfe::DFE);
    // loop optimization passes
    pm.register_pass(pass::optimize::loopsimplify::LoopSimplify);
    pm.register_pass(pass::optimize::loopelimi::LoopElimination);
    pm.register_pass(pass::optimize::loopicm::LICM);
    pm.register_pass(pass::optimize::loopunroll::LoopUnroll);
}

fn execute_passes(
    pm: &mut passes::pass_manager::PassManager,
    ctx: &mut crate::frontend::ir::context::Context,
    pctx: &mut passes::pass_context::PassContext,
) {
    let simplify_cfg = PipLine::new(
        "SimplifyCFG",
        vec![
            Box::new(pass::analysis::cfa::CFAnalysis),
            Box::new(pass::optimize::ube::UBE),
            Box::new(pass::optimize::cie::CIE),
            Box::new(pass::optimize::lbc::LinearBasicBlockCoalescing),
            Box::new(pass::analysis::ltdomra::LTDomRelAnalysis),
            Box::new(pass::analysis::loopa::LoopAnalysis),
            Box::new(pass::analysis::fca::FunctionCallAnalysis),
            Box::new(pass::optimize::dfe::DFE),
            Box::new(pass::analysis::fca::FunctionCallAnalysis),
        ],
    );

    let mem2reg = PipLine::new(
        "Mem2Reg",
        vec![
            Box::new(pass::optimize::pre_mem2reg::PreMem2Reg),
            Box::new(pass::optimize::mem2reg::Mem2Reg),
        ],
    );

    let reg2mem = PipLine::new("Reg2Mem", vec![Box::new(pass::optimize::reg2mem::Reg2Mem)]);

    let scalar_opt = PipLine::new(
        "ScalarOpt",
        vec![
            Box::new(pass::optimize::gtl::GlobalToLocal),
            Box::new(pass::optimize::pre_mem2reg::PreMem2Reg),
            Box::new(pass::optimize::sccp::SCCP),
            Box::new(pass::analysis::live_inst::LiveInstAnalysis),
            Box::new(pass::optimize::dce::DCE),
        ],
    );

    let tail_call_opt = PipLine::new(
        "TailCallOpt",
        vec![Box::new(pass::optimize::tco::TailCallOptimization)],
    );

    let inline = PipLine::new(
        "Inline",
        vec![
            Box::new(pass::optimize::reg2mem::Reg2Mem),
            Box::new(pass::optimize::inline::Inline),
            Box::new(pass::analysis::cfa::CFAnalysis),
            Box::new(pass::optimize::ube::UBE),
            Box::new(pass::optimize::cie::CIE),
            Box::new(pass::optimize::lbc::LinearBasicBlockCoalescing),
            Box::new(pass::analysis::ltdomra::LTDomRelAnalysis),
            Box::new(pass::analysis::loopa::LoopAnalysis),
            Box::new(pass::analysis::fca::FunctionCallAnalysis),
            Box::new(pass::optimize::dfe::DFE),
            Box::new(pass::analysis::fca::FunctionCallAnalysis),
            Box::new(pass::optimize::pre_mem2reg::PreMem2Reg),
            Box::new(pass::optimize::mem2reg::Mem2Reg),
        ],
    );

    let licm = PipLine::new(
        "LICM",
        vec![
            Box::new(pass::optimize::reg2mem::Reg2Mem),
            Box::new(pass::optimize::loopsimplify::LoopSimplify),
            Box::new(pass::analysis::ltdomra::LTDomRelAnalysis),
            Box::new(pass::analysis::loopa::LoopAnalysis),
            Box::new(pass::optimize::pre_mem2reg::PreMem2Reg),
            Box::new(pass::optimize::mem2reg::Mem2Reg),
            Box::new(pass::optimize::loopicm::LICM),
        ],
    );

    let lelimi = PipLine::new(
        "LoopElimination",
        vec![
            Box::new(pass::optimize::reg2mem::Reg2Mem),
            Box::new(pass::optimize::loopsimplify::LoopSimplify),
            Box::new(pass::analysis::ltdomra::LTDomRelAnalysis),
            Box::new(pass::analysis::loopa::LoopAnalysis),
            Box::new(pass::optimize::pre_mem2reg::PreMem2Reg),
            Box::new(pass::optimize::mem2reg::Mem2Reg),
            Box::new(pass::optimize::loopelimi::LoopElimination),
        ],
    );

    let _lunroll = PipLine::new(
        "LoopUnroll",
        vec![
            Box::new(pass::optimize::reg2mem::Reg2Mem),
            Box::new(pass::optimize::loopsimplify::LoopSimplify),
            Box::new(pass::analysis::ltdomra::LTDomRelAnalysis),
            Box::new(pass::analysis::loopa::LoopAnalysis),
            Box::new(pass::optimize::pre_mem2reg::PreMem2Reg),
            Box::new(pass::optimize::mem2reg::Mem2Reg),
            Box::new(pass::optimize::loopunroll::LoopUnroll),
        ],
    );

    let _alias_analysis = PipLine::new(
        "AliasAnalysis",
        vec![Box::new(pass::analysis::alias::AliasAnalysis)],
    );

    pm.add(&simplify_cfg);
    pm.add(&mem2reg);
    pm.add(&scalar_opt);

    pm.add(&tail_call_opt);
    pm.add(&simplify_cfg);
    pm.add(&scalar_opt);
    pm.add(&simplify_cfg);

    pm.add(&inline);
    pm.add(&scalar_opt);
    pm.add(&simplify_cfg);

    pm.add(&lelimi);
    pm.add(&simplify_cfg);
    pm.add(&scalar_opt);
    pm.add(&simplify_cfg);

    pm.add(&licm);
    pm.add(&simplify_cfg);
    pm.add(&scalar_opt);
    pm.add(&simplify_cfg);

    // pm.add(&lunroll);
    // pm.add(&simplify_cfg);
    // pm.add(&scalar_opt);
    // pm.add(&simplify_cfg);

    pm.add(&reg2mem);

    pm.execute(ctx, pctx).unwrap();
}

fn main() -> Result<()> {
    // 参数解析与检查
    let args = parse_arguments();
    check_args(&args);

    // 提取参数
    let emit_ast = args.get("ast"); // 输出AST文件
    let emit_ir = args.get("ir"); // 输出IR文件
    let emit_asm = args.get("asm"); // 输出ASM文件
    let emit_s = matches!(args.get("S"), Some(v) if v == "true"); // 适配比赛编译器选项要求,表示生成汇编代码
    let optimize = matches!(args.get("O1"), Some(v) if v == "true"); // 优化选项
    let source = args.get("source"); // 源文件名
    let output_file = args.get("o"); // 输出文件名

    let mut src = std::fs::read_to_string(source.unwrap())?; // 读取源文件内容
    src = frontend::lalrpop::macro_replace::replace_macros(src.as_str()); // 展开宏定义
    insert_my_lib(&mut src); // 加入自定义库

    let mut ast = sysy::CompUnitParser::new().parse(&src).unwrap();
    if let Some(ast_file) = emit_ast {
        std::fs::write(ast_file, format!("{:#?}", ast)).expect("Failed to write AST to file");
    }

    let mut ir = frontend::irgen(&mut ast, 8); // ARMV8架构

    let mut pass_manager = passes::pass_manager::PassManager::new();
    let mut pctx = passes::pass_context::PassContext::new();
    if optimize {
        register_ir_passes(&mut pass_manager);
        execute_passes(&mut pass_manager, &mut ir, &mut pctx);
    }

    if let Some(ir_file) = emit_ir {
        std::fs::write(ir_file, ir.get_ir_string()).expect("Failed to write IR to file");
    }

    if emit_s || emit_asm.is_some() {
        let mut codegen_context = backend::codegen::CodegenContext::new(&ir);
        codegen_context.codegen();
        let mut mctx = codegen_context.mircontext();

        if optimize {
            backend::peephole::delete_dead_inst(&mut mctx, 1);
            crate::backend::peephole::peephole_optimize(&mut mctx, 1);
            backend::peephole::delete_dead_inst(&mut mctx, 2);
        }
        backend::instrction_schedule::schedule(&mut mctx, 256);
        crate::backend::reg_alloc::gcregalloc::graph_coloring_regalloc(&mut mctx, &mut pctx);
        if optimize {
            crate::backend::peephole::peephole_optimize(&mut mctx, 2);
        }
        backend::instrction_schedule::schedule(&mut mctx, 1024);

        let asm = mctx.get_asm();

        let mut output_file_name = output_file;
        if emit_asm.is_some() {
            output_file_name = emit_asm;
        }
        std::fs::write(output_file_name.unwrap(), asm).expect("Failed to write ASM to file");
    }
    Ok(())
}

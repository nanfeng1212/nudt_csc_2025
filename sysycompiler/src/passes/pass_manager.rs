use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::{marker::PhantomData, str};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PassType {
    FunctionBasedAnalysis,
    GlobalBasedAnalsysis,
    FunctionBasedOptimization,
    GlobalBasedOptimization,
}

#[derive(Debug)]
pub enum PassError {
    DependencyCycle(Vec<String>),
    MissingDependency(String),
    ExecutionFailed { pass: String, cause: String },
    InvalidState(String),
    MissingCFG,
    InvalidInput(String),
}

pub trait Pass<Context = crate::frontend::ir::context::Context> {
    type Output; // 输出数据类型
    type Error; // 错误类型

    /// 遍类型
    fn pass_type(&self) -> PassType;

    /// 执行遍处理
    fn execute(
        &mut self,
        input: &mut Context,
        pctx: &mut super::pass_context::PassContext,
    ) -> Result<Self::Output, Self::Error>;

    /// 获取遍名称（用于依赖管理）
    fn name(&self) -> &'static str;

    /// 声明依赖的遍
    fn dependencies(&self) -> &[&'static str] {
        &[]
    }

    /// 声明会产生副作用的遍
    fn effects(&self) -> &[&'static str] {
        &[]
    }
}

pub struct PassManager<Context = crate::frontend::ir::context::Context> {
    passes: HashMap<String, Box<dyn Pass<Context, Output = bool, Error = PassError>>>,
    piplines: Vec<PipLine<Context>>,
    phantom: PhantomData<Context>,
}

pub struct PipLine<Context = crate::frontend::ir::context::Context> {
    name: String,
    passes: Vec<String>,
    phantom: PhantomData<Context>,
}

impl<Context> PipLine<Context> {
    pub fn new(
        name: &str,
        passes: Vec<Box<dyn Pass<Context, Output = bool, Error = PassError>>>,
    ) -> Self {
        {
            PipLine {
                name: name.to_string(),
                passes: passes.iter().map(|p| p.name().to_string()).collect(),
                phantom: PhantomData,
            }
        }
    }
}

impl<Context> Clone for PipLine<Context> {
    fn clone(&self) -> Self {
        PipLine {
            name: self.name.clone(),
            passes: self.passes.clone(),
            phantom: PhantomData,
        }
    }
}

impl<Context> PassManager<Context> {
    pub fn new() -> Self {
        PassManager {
            passes: HashMap::default(),
            piplines: Vec::default(),
            phantom: PhantomData,
        }
    }

    pub fn register_pass(
        &mut self,
        pass: impl Pass<Context, Error = PassError, Output = bool> + 'static,
    ) {
        let name = pass.name().to_string();
        self.passes.insert(name.clone(), Box::new(pass));
    }

    pub fn execute(
        &mut self,
        ctx: &mut Context,
        pctx: &mut super::pass_context::PassContext,
    ) -> Result<(), PassError> {
        let missing_infos = self.check_dependencies();
        if !missing_infos.is_empty() {
            let mut panic_info = format!("[PASS MANAGER] Invalid Execution of Passes:\n");
            for info in missing_infos {
                panic_info.push_str(format!("···{}\n", info).as_str());
            }
            panic!("{}", panic_info);
        } else {
            #[cfg(debug_assertions)]
            println!("TOTALLY {} PIPLINES", self.piplines.len());
            for (_i, pipline) in self.piplines.iter().enumerate() {
                #[cfg(debug_assertions)]
                println!("PIPLINE [{}][{}]->START", _i, pipline.name);
                for pass_name in pipline.passes.iter() {
                    #[cfg(debug_assertions)]
                    println!(" ∟PASS [{}]->START", pass_name);
                    let pass = self.passes.get_mut(pass_name).ok_or_else(|| {
                        PassError::InvalidState(format!("Pass {} not found", pass_name))
                    })?;
                    let _ = pass
                        .execute(ctx, pctx)
                        .map_err(|e| PassError::ExecutionFailed {
                            pass: pass_name.clone(),
                            cause: format!("{:?}", e),
                        });
                    #[cfg(debug_assertions)]
                    println!(" ∟PASS [{}]->END", pass_name);
                }
                #[cfg(debug_assertions)]
                println!("PIPLINE [{}][{}]->END", _i, pipline.name);
            }
        }
        Ok(())
    }

    pub fn add(&mut self, pip_line: &PipLine<Context>) {
        self.piplines.push(pip_line.clone());
    }

    fn check_dependencies(&self) -> Vec<String> {
        let mut missing_infos = Vec::default();
        let mut finished = HashSet::default();

        for (_i, pip_line) in self.piplines.iter().enumerate() {
            let passes = pip_line
                .passes
                .iter()
                .map(|name| self.passes.get(name).unwrap());
            for pass in passes {
                for dep in pass.dependencies() {
                    if !finished.contains(&dep.to_string()) {
                        missing_infos.push(format!(
                            "PipLine [{}][{}] Missing Dependency: Pass [{}] depends on [{}]",
                            _i,
                            pip_line.name,
                            pass.name(),
                            dep
                        ));
                    }
                }
                finished.insert(pass.name().to_string());
                for effect in pass.effects() {
                    finished.remove(*effect);
                }
            }
        }

        missing_infos
    }
}

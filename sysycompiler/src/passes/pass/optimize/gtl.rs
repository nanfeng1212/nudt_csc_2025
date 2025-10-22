use crate::{
    frontend::ir::{
        context::Context,
        defuse::Useable,
        function::Function,
        instruction::{Instruction, MemAccessOp},
        value::{ConstantValue, Value},
    },
    passes::{
        pass_context::PassContext,
        pass_manager::{Pass, PassError, PassType},
    },
    utils::linked_list::{LinkedListContainer, LinkedListNode},
};
/// Gloabal Vlaue to Local Value Pass
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
pub struct GlobalToLocal;

pub struct GlobalInfo {}

impl Pass for GlobalToLocal {
    type Output = bool;
    type Error = PassError;

    fn pass_type(&self) -> PassType {
        PassType::GlobalBasedOptimization
    }

    fn execute(&mut self, ctx: &mut Context, pctx: &mut PassContext) -> Result<bool, Self::Error> {
        let mut num_remove = 0;
        let mut num_to_local = 0;
        let mut def_infos: HashMap<String, HashSet<Instruction>> = HashMap::default();
        let mut use_infos: HashMap<String, HashSet<Instruction>> = HashMap::default();
        let mut global2values: HashMap<String, ConstantValue> = HashMap::default();

        let mut global_values = HashSet::default();
        for value in ctx.get_values() {
            if value.is_global_ptr(ctx) {
                global_values.insert(value);
            }
        }
        // println!("···[GTL] Found {} global values", global_values.len());

        // 建立所有标量全局变量和初始值的映射关系
        for global in ctx.get_globals() {
            if global.typ(ctx).is_array(ctx) {
                continue;
            } else {
                let val = global.value(ctx);
                global2values.insert(global.name(ctx).to_string(), val.clone());
            }
        }
        // println!("···[GTL] Found {} global values", global2values.len());

        let global_names = Self::collect_info(ctx, &mut def_infos, &mut use_infos, &global_values);

        for global in global_names.iter() {
            // 收集全局变量的定值和使用信息
            let def_info = def_infos.get(global);
            let use_info = use_infos.get(global);
            if def_info.is_none() && use_info.is_none() {
                //没有定值和使用，直接删除
                continue; //理论上不会进这个分支
            } else if def_info.is_none() {
                //没有定值直接用初始值替代所有使用
                let init_val = Self::get_init(ctx, global, &global2values);
                if let Some(init_val) = init_val {
                    for load in use_info.unwrap() {
                        load.replace_all_uses_with(ctx, init_val.clone());
                        load.remove(ctx);
                        num_remove += 1;
                    }
                }
            } else if use_info.is_none() {
                //没有使用直接删除
                for inst in def_info.unwrap() {
                    let _ = inst.remove(ctx);
                    num_remove += 1;
                }
            } else if def_info.unwrap().len() == 1
                && !pctx.callgraph.call_cycle(
                    &def_info
                        .unwrap()
                        .iter()
                        .next()
                        .unwrap()
                        .get_basicblock(ctx)
                        .unwrap()
                        .get_function(ctx)
                        .unwrap(),
                )
            {
                //只有一次定值的情况
                let def_info = def_info.unwrap();
                let use_info = use_info.unwrap();
                let def_inst = def_info.iter().next().unwrap();
                let def_bbk = def_inst.get_basicblock(ctx).unwrap();
                let def_func = def_bbk.get_function(ctx).unwrap();

                let use_in_same_func = use_info
                    .iter()
                    .all(|u| u.get_basicblock(ctx).unwrap().get_function(ctx).unwrap() == def_func);
                if use_in_same_func && pctx.callgraph.get_call_num(&def_func) <= 1 {
                    //使用和定值都在一个函数内
                    // println!("[GTL] Function: {} is called {} times", def_func.get_id(ctx), pctx.callgraph.get_call_num(&def_func));
                    Self::to_local(ctx, global, def_info, use_info, &global2values, &def_func);
                    num_to_local += 1;
                } else { // 使用和定值可能不在一个函数内
                    // 不确定使用会在定值前还是之后，需要考虑函数之间的调用关系
                    // let has_no_cycle = use_info.iter().all(|u| !pctx.callgraph.call_cycle(&u.get_basicblock(ctx).unwrap().get_function(ctx).unwrap()));
                    // if has_no_cycle { // 无回路，可以局部化, 有回环的都不处理
                    //     let use_after_def = HashSet::default();
                    //     let use_before_def = HashSet::default();
                    //     let mut stack = vec![def_func];
                    //     let mut
                    // }
                }
            } else {
                //多次定值情况比较复杂
                let def_info = def_info.unwrap();
                let use_info = use_info.unwrap();

                let def_func = def_info
                    .iter()
                    .next()
                    .unwrap()
                    .get_basicblock(ctx)
                    .unwrap()
                    .get_function(ctx)
                    .unwrap();
                let use_func = use_info
                    .iter()
                    .next()
                    .unwrap()
                    .get_basicblock(ctx)
                    .unwrap()
                    .get_function(ctx)
                    .unwrap();
                // 定值和使用有可能在一个函数内，要求函数不能有回环
                if def_func == use_func
                    && !pctx.callgraph.call_cycle(&def_func)
                    && pctx.callgraph.get_call_num(&def_func) <= 1
                {
                    let def_in_same_func = def_info.iter().all(|d| {
                        d.get_basicblock(ctx).unwrap().get_function(ctx).unwrap() == def_func
                    });
                    let use_in_same_func = use_info.iter().all(|u| {
                        u.get_basicblock(ctx).unwrap().get_function(ctx).unwrap() == use_func
                    });
                    if def_in_same_func && use_in_same_func {
                        // println!("···[GTL] Global [{}] is localizable", global);
                        // for inst in def_info {
                        //     println!("···[GTL] Def: {}", inst.display(ctx));
                        // }
                        // for inst in use_info {
                        //     println!("···[GTL] Use: {}", inst.display(ctx));
                        // }
                        //定值和使用都在一个函数内
                        Self::to_local(ctx, global, def_info, use_info, &global2values, &def_func);
                        num_to_local += 1;
                    } else {
                        // 定值和使用可能不在一个函数内，需要考虑函数之间的调用关系
                        // TODO: handle cross-function global value
                    }
                }
            }
        }

        #[cfg(debug_assertions)]
        println!(
            "···[GTL]Removed {} instructions, localize {} global values",
            num_remove, num_to_local
        );
        Ok(num_to_local > 0 || num_remove > 0)
    }

    fn name(&self) -> &'static str {
        "GloabalValueToLoacal"
    }

    fn dependencies(&self) -> &[&'static str] {
        &["FunctionCallAnalysis"]
    }

    fn effects(&self) -> &[&'static str] {
        &[]
    }
}

impl GlobalToLocal {
    fn collect_info(
        ctx: &mut Context,
        def_infos: &mut HashMap<String, HashSet<Instruction>>,
        use_infos: &mut HashMap<String, HashSet<Instruction>>,
        global_values: &HashSet<Value>,
    ) -> HashSet<String> {
        let mut global_names = HashSet::default();
        for global in global_values.iter() {
            if global.is_global_array(ctx) {
                //数组全局变量不进行优化
                continue;
            }
            if let Some(global_name) = global.get_global_ptr_name(ctx) {
                for user in global.users(ctx) {
                    let inst = user.get_instruction();
                    if inst.is_store(ctx) {
                        def_infos
                            .entry(global_name.clone())
                            .or_insert(HashSet::default())
                            .insert(inst.clone());
                    } else if inst.is_load(ctx) {
                        use_infos
                            .entry(global_name.clone())
                            .or_insert(HashSet::default())
                            .insert(inst.clone());
                    }
                }
                global_names.insert(global_name.clone());
            }
        }
        global_names
    }

    // 获取全局变量初始值
    fn get_init(
        ctx: &mut Context,
        global: &String,
        global2values: &HashMap<String, ConstantValue>,
    ) -> Option<Value> {
        if let Some(value) = global2values.get(global) {
            if value.is_undef() || value.is_zero() {
                if value.typ().is_int(ctx) {
                    let zero = ConstantValue::int32(ctx, 0);
                    return Some(Value::constant(ctx, zero));
                } else if value.typ().is_float(ctx) {
                    let zero = ConstantValue::float32(ctx, 0.0);
                    return Some(Value::constant(ctx, zero));
                }
            }
            return Some(Value::constant(ctx, value.clone()));
        }
        None
    }

    /// 全局变量局部化
    fn to_local(
        ctx: &mut Context,
        global: &String,
        def_info: &HashSet<Instruction>,
        use_info: &HashSet<Instruction>,
        global2values: &HashMap<String, ConstantValue>,
        func: &Function,
    ) {
        if let Some(init_val) = Self::get_init(ctx, global, &global2values) {
            let typ = init_val.kind(ctx);
            let alloca = Instruction::memaccess(ctx, MemAccessOp::Alloca { typ: typ }, typ, vec![]);
            let slot = alloca.get_result(ctx).unwrap();
            let store = Instruction::memaccess(
                ctx,
                MemAccessOp::Store,
                typ,
                vec![init_val.clone(), slot.clone()],
            );
            let entry = func.head(ctx).unwrap();
            let insts = entry.iter(ctx).collect::<Vec<Instruction>>();
            for inst in insts {
                if !inst.is_alloca(ctx) {
                    let _ = inst.insert_before(ctx, alloca);
                    let _ = inst.insert_before(ctx, store);
                    break;
                }
            }
            for inst in def_info {
                inst.replace_operand_with_remove(ctx, 1, slot);
            }
            for inst in use_info {
                inst.replace_operand_with_remove(ctx, 0, slot);
            }
        }
    }
}

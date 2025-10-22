use std::{array, collections::{HashMap, HashSet}};

use crate::{backend::mir::{
    mir_context::MirContext,
    mir_function::MirFunction,
    mir_inst::MirInst,
    mir_operand::MemLoc,
    regs::{self, PReg, Reg},
}, utils::linked_list::{LinkedListContainer, LinkedListNode}};

pub fn allocate_reg(_mctx: &mut MirContext, _vreg: regs::VReg, candidates: &[regs::PReg], hw_reg_used: &mut [bool]) -> Option<regs::PReg> {
    for &preg in candidates {
        let index = preg.hw_index() as usize;
        if !hw_reg_used[index] {
            hw_reg_used[index] = true;
            return Some(preg);
        }
    }
    None

}
    
pub fn regalloc(mctx: &mut MirContext) {
    // 硬件寄存器状态(9-15和19-28可以用来分配)
    let hw_reg_used: [bool; 29] = array::from_fn(|i| {
        !((9..=14).contains(&i) || (19..=28).contains(&i))
    });
    let hw_reg_used_float: [bool; 32] = array::from_fn(|i| {
        !((8..=15).contains(&i) || (18..=31).contains(&i))
    });
    let integer_candidates = vec![
        regs::w19(), regs::w20(), regs::w21(), regs::w22(), regs::w23(), regs::w24(), regs::w25(), regs::w26(), regs::w27(), regs::w28(),
        regs::w9(), regs::w10(), regs::w11(), regs::w12(), regs::w13(), regs::w14(), regs::w15(), regs::w16(), regs::w17(),
    ];
    let address_candidates = vec![
        regs::x19(), regs::x20(), regs::x21(), regs::x22(), regs::x23(), regs::x24(), regs::x25(), regs::x26(), regs::x27(), regs::x28(),
        regs::x9(), regs::x10(), regs::x11(), regs::x12(), regs::x13(), regs::x14(), regs::x15(), regs::x16(), regs::x17(),
    ];
    let float_candidates = vec![
        regs::s8(), regs::s9(), regs::s10(), regs::s11(), regs::s12(), regs::s13(), regs::s14(), regs::s15(), 
        regs::s16(), regs::s17(), regs::s18(), regs::s19(), regs::s20(), regs::s21(), regs::s22(), regs::s23(), regs::s24(), regs::s25(), regs::s26(), regs::s27(), regs::s28(), regs::s29(), regs::s30(), regs::s31()
    ];
    let callee_saved: Vec<u8> = vec![19,20,21,22,23,24,25,26,27,28];
    let callee_saved_float: Vec<u8> = vec![8,9,10,11,12,13,14,15,16,17];
    let caller_saved: Vec<PReg> = vec![
        regs::x9(),
        regs::x10(),
        regs::x11(),
        regs::x12(),
        regs::x13(),
        regs::x14(),
        regs::x15(),
        regs::x16(),
        regs::x17(),
    ];
    let caller_saved_float: Vec<PReg> = vec![
        regs::s16(),
        regs::s17(),
        regs::s18(),
        regs::s19(),
        regs::s20(),
        regs::s21(),
        regs::s22(),
        regs::s23(),
        regs::s24(),
        regs::s25(),
        regs::s26(),
        regs::s27(),
        regs::s28(),
        regs::s29(),
        regs::s30(),
        regs::s31()
    ];
    // 预先收集所有函数的信息
    let mut func_info = Vec::new();
    for func_data in mctx.funcs.iter() {
        let function = func_data.self_ptr();
        if function.is_external(mctx) {
            continue;
        }
        let mut all_vregs = HashSet::new();
        
        // 使用不可变借用收集虚拟寄存器
        {
            for block in function.iter(&mctx) {
                for inst in block.iter(&mctx) {
                    let mut collect_reg = |reg: Reg| {
                        if let Reg::V(vreg) = reg {
                            all_vregs.insert(vreg);
                        }
                    };
                    let cur_regs = inst.all_regs(&mctx);
                    for reg in cur_regs {
                        collect_reg(reg);
                    }
                }
            }
        }
        func_info.push((function, all_vregs));
    }
    // 现在使用可变借用来处理每个函数
    for (function, all_vregs) in func_info {
        let curr_func = Some(function);
        let mut reg_map = HashMap::new();
        let mut spill_map = HashMap::new();
        let mut available_regs = hw_reg_used.clone();
        let mut availabel_float_regs = hw_reg_used_float.clone();
        let mut spilled_registers = HashSet::new();
        
        let mut use_count = HashMap::new();
        for block in function.iter(mctx) {
            for inst in block.iter(mctx) {
                for reg in inst.uses(mctx) {
                    if let Reg::V(vreg) = reg {
                        *use_count.entry(vreg).or_insert(0) += 1;
                    }
                }
            }
        }
        let mut sorted_vregs: Vec<_> = all_vregs.iter().collect();
        // 使用频率降序排除
        sorted_vregs.sort_by(|a,b| use_count.get(b).cmp(&use_count.get(a)));

        // 分配物理寄存器
        for &vreg in &sorted_vregs {
            let preg = match vreg.kind() {
                regs::RegKind::Integer => allocate_reg(mctx, *vreg, &integer_candidates, &mut available_regs),
                regs::RegKind::Address => allocate_reg(mctx, *vreg, &address_candidates, &mut available_regs),
                regs::RegKind::Float => allocate_reg(mctx, *vreg, &float_candidates, &mut availabel_float_regs),
            };
            if let Some(preg) = preg {
                reg_map.insert(vreg, preg);
                if callee_saved.contains(&preg.hw_index()) && !preg.is_float() {
                    curr_func.unwrap().add_callee_reg(mctx, preg);
                }
                if callee_saved_float.contains(&preg.hw_index()) && preg.is_float() {
                    curr_func.unwrap().add_callee_reg(mctx, preg);
                }
            } else {
                spilled_registers.insert(vreg);
            }
        }
        for &reg in &spilled_registers {
            let e = match reg.kind() {
                regs::RegKind::Integer => 4,
                regs::RegKind::Float => 4,
                _ => 8
            };
            let cur_stack = curr_func.unwrap().storage_stack_size(mctx) + curr_func.unwrap().calleeargs_stack_size(mctx);
            spill_map.insert(reg, cur_stack);
            curr_func.unwrap().add_storage_stack_size(mctx, e);
        }
        // 处理每条指令的溢出
        curr_func.unwrap().add_storage_stack_size(mctx, 19*8+24*4);
        let mut curr_block = function.head(mctx);
        while let Some(block) = curr_block {
            let mut curr_inst = block.head(mctx);
            while let Some(inst) = curr_inst {
                let next_inst = inst.succ(mctx);
                // 如果是call指令，需要进行调用者保护
                if inst.is_call(&mctx) {
                    // 调用前保存
                    let mut e = curr_func.unwrap().storage_stack_size(mctx) + curr_func.unwrap().calleeargs_stack_size(mctx);
                    for preg in caller_saved.iter() {
                        e -= 8;
                        let mem_loc = MemLoc::RegOffset { base:regs::sp().into() , offset: e };
                        let str = MirInst::str(mctx, regs::Reg::P(*preg), mem_loc);
                        let _ = inst.insert_before(mctx, str);
                    }
                    for preg in caller_saved_float.iter() {
                        e -= 4;
                        let mem_loc = MemLoc::RegOffset { base:regs::sp().into() , offset: e };
                        let str = MirInst::str(mctx, regs::Reg::P(*preg), mem_loc);
                        let _ = inst.insert_before(mctx, str);
                    }
                    // 调用后恢复
                    e = curr_func.unwrap().storage_stack_size(mctx) + curr_func.unwrap().calleeargs_stack_size(mctx);
                    for preg in caller_saved.iter() {
                        e -= 8;
                        let mem_loc = MemLoc::RegOffset { base:regs::sp().into() , offset: e };
                        let ldr = MirInst::ldr(mctx, regs::Reg::P(*preg), mem_loc);
                        let _ = inst.insert_after(mctx, ldr);
                    }
                    for preg in caller_saved_float.iter() {
                        e -= 4;
                        let mem_loc = MemLoc::RegOffset { base:regs::sp().into() , offset: e };
                        let ldr = MirInst::ldr(mctx, regs::Reg::P(*preg), mem_loc);
                        let _ = inst.insert_after(mctx, ldr);
                    }
                } else {
                    // 处理源操作数(uses)
                    let mut spill_reg_index = 0;
                    for reg in inst.uses(mctx) {
                        //println!("{:?},{:?}",reg, inst);
                        if let Reg::V(vreg) = reg {
                            if let Some(spill_slot) = spill_map.get(&vreg) {
                                let spill_reg = if spill_reg_index == 0 {
                                    match vreg.kind() {
                                        regs::RegKind::Address => regs::x15().into(),
                                        regs::RegKind::Integer => regs::w15().into(),
                                        regs::RegKind::Float => regs::s16().into(),
                                    }
                                } else if spill_reg_index == 1{
                                    match vreg.kind() {
                                        regs::RegKind::Address => regs::x16().into(),
                                        regs::RegKind::Integer => regs::w16().into(),
                                        regs::RegKind::Float => regs::s17().into(),
                                    }
                                } else {
                                    match vreg.kind() {
                                        regs::RegKind::Address => regs::x17().into(),
                                        regs::RegKind::Integer => regs::w17().into(),
                                        _ => todo!(),
                                    }
                                };
                                spill_reg_index += 1;
                                
                                let spill_offset = *spill_slot;
                                generate_spill_load(
                                    mctx,
                                    spill_reg,
                                    MemLoc::RegOffset { base: regs::sp().into(), offset: spill_offset },
                                    inst
                                );
                                // 即使目的操作数也是源操作数
                                let flag = if inst.def(mctx).contains(&reg) {
                                    true
                                } else {
                                    false
                                };
                                inst.replace_regs(mctx, Reg::V(vreg), spill_reg);
                                if flag {
                                    generate_spill_store(
                                        mctx,
                                        spill_reg,
                                        MemLoc::RegOffset { base: regs::sp().into(), offset: spill_offset },
                                        inst
                                    );
                                }
                            }
                        }
                    }
                    // 处理目标操作数(defs)
                    for reg in inst.def(mctx) {
                        if let Reg::V(vreg) = reg {
                            if let Some(spill_slot) = spill_map.get(&vreg) {
                                let spill_reg = match vreg.kind() {
                                    regs::RegKind::Address => regs::x15().into(),
                                    regs::RegKind::Integer => regs::w15().into(),
                                    regs::RegKind::Float => regs::s16().into(),
                                };
                                let spill_offset = *spill_slot;
                                inst.replace_regs(mctx, Reg::V(vreg), spill_reg);
                                generate_spill_store(
                                    mctx,
                                    spill_reg,
                                    MemLoc::RegOffset { base: regs::sp().into(), offset: spill_offset },
                                    inst
                                );
                            }
                        }
                    }
                    // 替换非溢出的虚拟寄存器和调整偏移
                    for reg in inst.all_regs(mctx) {
                        if let Reg::V(vreg) = reg {
                            if let Some(allocated_reg) = reg_map.get(&vreg) {
                                //println!("{:?}",inst.display(&mctx.mctx));
                                inst.replace_regs(mctx,Reg::V(vreg),(*allocated_reg).into());
                                //println!("{:?}", inst.display(&mctx.mctx));
                            }
                        }
                    }
                }
                curr_inst = next_inst;
            }
            curr_block = block.succ(mctx);
        }
        generate_prologue(mctx,curr_func.unwrap());
        generate_epilogue(mctx, curr_func.unwrap());
    }
}

pub fn generate_spill_load(mctx: &mut MirContext, reg: Reg, mem_loc: MemLoc, inst: MirInst) {
    let load = MirInst::ldr(mctx,  reg.into(), mem_loc);
    let _ = inst.insert_before(mctx, load);
}

pub fn generate_spill_store(mctx: &mut MirContext, reg: Reg, mem_loc: MemLoc, inst: MirInst) {
    let store = MirInst::str(mctx,  reg.into(), mem_loc);
    let _ = inst.insert_after(mctx, store);
}
    

pub fn generate_prologue(mctx: &mut MirContext, function: MirFunction) {
    function.add_callee_reg(mctx, regs::fp().into());
    function.add_callee_reg(mctx, regs::x30().into());
    let mut stack_size = function.storage_stack_size(mctx) + function.calleeargs_stack_size(mctx) + function.callee_regs(mctx).len() as i32 * 8;
    stack_size = (stack_size + 15) & !15;
    let sub = MirInst::subi(mctx, regs::sp().into(), regs::sp().into(), stack_size);
    let stp = MirInst::stp(mctx, regs::fp().into(), regs::x30().into(), MemLoc::RegOffset{base: regs::sp().into(), offset: stack_size - 16});
    if stack_size > 4095 {}
    let addi = MirInst::addi(mctx, regs::fp().into(), regs::sp().into(), stack_size);
    // 存储被调用者保护寄存器
    let mut e= 16;
    for (_, reg) in function.callee_regs(mctx).iter().enumerate() {
        if *reg == regs::x30().into() || *reg == regs::fp().into() {
            continue;
        }
        e += 8;
        let real_reg = match reg.kind() {
            regs::RegKind::Float => (*reg).into(),
            _ => regs::x_reg(reg.0).into()
        };
        let str = MirInst::str(mctx, real_reg, MemLoc::RegOffset { base: regs::sp().into(), offset: stack_size - e});
        function.head(mctx).unwrap().push_front(mctx, str).unwrap();
    }
    function.head(mctx).unwrap().push_front(mctx, addi).unwrap();
    function.head(mctx).unwrap().push_front(mctx, stp).unwrap();
    function.head(mctx).unwrap().push_front(mctx, sub).unwrap();
}

pub fn generate_epilogue(mctx: &mut MirContext, function: MirFunction) {
    let mut stack_size = function.storage_stack_size(mctx) + function.calleeargs_stack_size(mctx) + function.callee_regs(mctx).len() as i32 * 8;
    stack_size = (stack_size + 15) & !15;
    let ldp = MirInst::ldp(mctx, regs::fp().into(), regs::x30().into(), MemLoc::RegOffset{base: regs::sp().into(), offset: (stack_size - 16) });
    let addi = MirInst::addi(mctx, regs::sp().into(), regs::sp().into(), stack_size );
    let ret = MirInst::ret(mctx);
    // 恢复被调用者保护寄存器
    let mut e= 16;
    for (_, reg) in function.callee_regs(mctx).iter().enumerate() {
        if *reg == regs::x30().into() || *reg == regs::fp().into() {
            continue;
        }
        e += 8;
        let real_reg = match reg.kind() {
            regs::RegKind::Float => (*reg).into(),
            _ => regs::x_reg(reg.0).into()
        };
        let ldr = MirInst::ldr(mctx, real_reg, MemLoc::RegOffset { base: regs::sp().into(), offset: stack_size - e });
        function.tail(mctx).unwrap().push_back(mctx, ldr).unwrap();
    }
    function.tail(mctx).unwrap().push_back(mctx, ldp).unwrap();
    function.tail(mctx).unwrap().push_back(mctx, addi).unwrap();
    function.tail(mctx).unwrap().push_back(mctx, ret).unwrap();
}
    
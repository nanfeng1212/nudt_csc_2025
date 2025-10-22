pub mod cie; // Continuos br instruction elimination (CIE) pass
pub mod dce; // Dead code elimination (DCE) pass
pub mod dfe; // Dead Function Elimination (DFE) pass
pub mod dpe; // Dead Phi Elimination (DPE) pass ??? todo
pub mod gtl; // Global Value to Local Value (GTL) pass
pub mod inline; // Inline function pass
pub mod lbc; // Linear BasicBlock Coalescing (LBC) pass
pub mod loopelimi; // Loop Elimination pass
pub mod loopicm; // Loop Invariant Code Motion (LICM) pass
pub mod loopsimplify; // Loop Simplify pass循环标准化
pub mod loopunroll; // Loop Unroll pass循环展开
pub mod mem2reg; //Mem2reg insert phi
pub mod pre_mem2reg; // Pre-mem2reg pass remove single store for scalar
pub mod reg2mem; // Reg2mem pass remove phi
pub mod sccp; // Sparse conditional constant propagation (SCCP) pass
pub mod tco; // Tail call optimization (TCO) pass
pub mod ube; // Unused block elimination (UBE) pass

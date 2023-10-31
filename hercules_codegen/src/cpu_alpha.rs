extern crate bitvec;
extern crate hercules_ir;
extern crate inkwell;

use std::iter::zip;

use self::bitvec::prelude::*;

use self::inkwell::builder::Builder;
use self::inkwell::context::Context;
use self::inkwell::module::Module;
use self::inkwell::OptimizationLevel;

use self::hercules_ir::def_use::*;
use self::hercules_ir::ir::*;

/*
 * This CPU backend was written to get some Hercules IR running, and to better
 * understand how writing backends for Hercules IR will work. This backend is
 * not meant to be used in the long term. If you are reading this code in a
 * significant amount of time from when this comment was written, you are
 * probably already doing something wrong - Russel.
 */

/*
 * Top level function to generate code for a module. Writes the result object
 * file to the specified path.
 */
pub fn cpu_alpha_codegen(
    module: &hercules_ir::ir::Module,
    reverse_postorders: &Vec<Vec<NodeID>>,
    bbs: &Vec<Vec<NodeID>>,
    path: &std::path::Path,
) {
    // Step 1: partition reverse postorder into control and data nodes, for each
    // function individually.
    let hercules_ir::ir::Module {
        functions,
        types,
        constants,
        dynamic_constants,
    } = module;

    let mut cfgs = vec![];
    let mut dfgs = vec![];
    for function_idx in 0..functions.len() {
        let function = &functions[function_idx];
        let reverse_postorder = &reverse_postorders[function_idx];
        let (cfg, dfg): (Vec<_>, Vec<_>) = reverse_postorder
            .iter()
            .map(|x| *x)
            .partition(|id| function.is_control(*id));
        cfgs.push(cfg);
        dfgs.push(dfg);
    }

    // Step 2: initialize LLVM objects.
    let context = Context::create();
    let module = context.create_module("");
    let builder = context.create_builder();

    // Step 3: add all the types.

    // Step 4: do codegen for each function.
    for function_idx in 0..functions.len() {
        let function = &functions[function_idx];
        let cfg = &cfgs[function_idx];
        let dfg = &dfgs[function_idx];
    }
}

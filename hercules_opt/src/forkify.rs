extern crate hercules_ir;

use self::hercules_ir::ir::*;
use self::hercules_ir::loops::*;

/*
 * Top level function to convert natural loops with simple induction variables
 * into fork-joins.
 */
pub fn forkify(
    function: &mut Function,
    constants: &Vec<Constant>,
    dynamic_constants: &mut Vec<DynamicConstant>,
    loops: &LoopTree,
) {
}

extern crate hercules_ir;

use self::hercules_ir::ir::*;
use self::hercules_ir::loops::*;

/*
 * Function to attempt to forkify a particular loop. Only forkifies loops with
 * an obvious loop induction variable.
 */
pub fn forkify(
    function: &mut Function,
    loop_header: NodeID,
    constants: &Vec<Constant>,
    dynamic_constants: &mut Vec<DynamicConstant>,
    loops: &LoopTree,
) {
}

extern crate hercules_ir;

use std::fmt::Write;

use self::hercules_ir::*;

/*
 * Top level function to generate code for a partition, targeting the CPU.
 */
pub(crate) fn codegen_cpu<W: Write>(
    function: &Function,
    partition: &Vec<NodeID>,
    top_node: NodeID,
    schedules: &Vec<Vec<Schedule>>,
    llvm_types: &Vec<String>,
    llvm_constants: &Vec<String>,
    llvm_dynamic_constants: &Vec<String>,
    w: &mut W,
) -> std::fmt::Result {
    todo!()
}

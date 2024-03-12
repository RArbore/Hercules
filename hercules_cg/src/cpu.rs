extern crate hercules_ir;

use std::fmt::Write;

use self::hercules_ir::*;

use crate::*;

/*
 * Top level function to generate code for a partition, targeting the CPU.
 */
pub(crate) fn codegen_cpu<W: Write>(
    function: &Function,
    def_use: &ImmutableDefUseMap,
    plan: &Plan,
    top_node: NodeID,
    inverted_partition_map: &Vec<Vec<NodeID>>,
    llvm_types: &Vec<String>,
    llvm_constants: &Vec<String>,
    llvm_dynamic_constants: &Vec<String>,
    w: &mut W,
) -> Result<(), std::fmt::Error> {
    // Step 1: do some analysis to find three things:
    // 1. All of the data inputs to this partition.
    // 2. All of the data outputs of this partition.
    // 3. All of the return locations of this partition.
    let partition_id = plan.partitions[top_node.idx()];
    let data_inputs = partition_data_inputs(function, plan, inverted_partition_map, partition_id);
    let data_outputs = partition_data_outputs(
        function,
        def_use,
        plan,
        inverted_partition_map,
        partition_id,
    );
    let control_returns = partition_control_returns(
        function,
        def_use,
        plan,
        inverted_partition_map,
        partition_id,
    );

    // Step 2: Determine the function signature for this partition. The
    // arguments are the input data nodes, plus dynamic constants, plus array
    // constants. The return struct contains all of the data outputs, plus
    // control information if there are multiple successor partitions. The
    // control information is used by the orchestration code to implement
    // control flow between partitions.
    let return_type = Type::Product(Box::new([]));

    Ok(())
}

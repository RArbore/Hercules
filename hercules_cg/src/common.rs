extern crate hercules_ir;

use std::fmt::Write;

use self::hercules_ir::*;

/*
 * Find data inputs to a partition.
 */
pub(crate) fn partition_data_inputs(
    function: &Function,
    plan: &Plan,
    inverted_partition_map: &Vec<Vec<NodeID>>,
    partition_id: PartitionID,
) -> Vec<NodeID> {
    let partition = &inverted_partition_map[partition_id.idx()];

    partition
        .iter()
        .map(|id| {
            // For each node in the partition, filter out the uses that are data
            // nodes and are in a different partition.
            get_uses(&function.nodes[id.idx()])
                .as_ref()
                .into_iter()
                .filter(|id| {
                    !function.nodes[id.idx()].is_control()
                        && plan.partitions[id.idx()] != partition_id
                })
                .map(|x| *x)
                .collect::<Vec<NodeID>>()
        })
        // Collect all such uses across the whole partition.
        .flatten()
        .collect()
}

/*
 * Find data outputs of a partition.
 */
pub(crate) fn partition_data_outputs(
    function: &Function,
    def_use: &ImmutableDefUseMap,
    plan: &Plan,
    inverted_partition_map: &Vec<Vec<NodeID>>,
    partition_id: PartitionID,
) -> Vec<NodeID> {
    let partition = &inverted_partition_map[partition_id.idx()];

    partition
        .iter()
        .filter(|id| {
            // For each data node in the partition, check if it has any uses
            // outside its partition. Users can be control or data nodes.
            !function.nodes[id.idx()].is_control()
                && def_use
                    .get_users(**id)
                    .as_ref()
                    .into_iter()
                    .filter(|id| plan.partitions[id.idx()] != partition_id)
                    .map(|x| *x)
                    .count()
                    > 0
        })
        .map(|x| *x)
        .collect()
}

/*
 * Find control nodes that might return from a partition.
 */
pub(crate) fn partition_control_returns(
    function: &Function,
    def_use: &ImmutableDefUseMap,
    plan: &Plan,
    inverted_partition_map: &Vec<Vec<NodeID>>,
    partition_id: PartitionID,
) -> Vec<NodeID> {
    let partition = &inverted_partition_map[partition_id.idx()];

    partition
        .iter()
        .filter(|id| {
            // For each control node in the partition, check if it has any users
            // outside its partition. Users can be control nodes - if a user in
            // a different partition is a data node, then the partition is mal-
            // formed.
            function.nodes[id.idx()].is_control()
                && def_use
                    .get_users(**id)
                    .as_ref()
                    .into_iter()
                    .filter(|id| {
                        // Users of control nodes can only be data nodes if they
                        // are in the same partition as the control node. Only
                        // control users may be in a different partition.
                        assert!(
                            function.nodes[id.idx()].is_control()
                                || plan.partitions[id.idx()] == partition_id
                        );
                        plan.partitions[id.idx()] != partition_id
                    })
                    .map(|x| *x)
                    .count()
                    > 0
        })
        .map(|x| *x)
        .collect()
}

/*
 * Emit a function signature in LLVM IR.
 */
pub(crate) fn emit_function_signature<W: Write>(
    llvm_types: &Vec<String>,
    llvm_constants: &Vec<String>,
    llvm_dynamic_constants: &Vec<String>,
    w: &mut W,
) -> std::fmt::Result {
    todo!()
}

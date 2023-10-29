extern crate hercules_ir;

use std::collections::HashMap;

use self::hercules_ir::dataflow::*;
use self::hercules_ir::def_use::*;
use self::hercules_ir::dom::*;
use self::hercules_ir::ir::*;
use self::hercules_ir::loops::*;
use self::hercules_ir::subgraph::*;

/*
 * Top level global code motion function. Assigns each data node to one of its
 * immediate control use / user nodes, forming (unordered) basic blocks. Returns
 * the control node / basic block each node is in.
 */
pub fn gcm(
    function: &Function,
    def_use: &ImmutableDefUseMap,
    reverse_postorder: &Vec<NodeID>,
    control_subgraph: &Subgraph,
    dom: &DomTree,
    fork_join_map: &HashMap<NodeID, NodeID>,
) -> Vec<NodeID> {
    // Step 1: find the immediate control uses and immediate control users of
    // each node.
    let immediate_control_uses =
        forward_dataflow(function, reverse_postorder, |inputs, node_id| {
            immediate_control_flow(inputs, node_id, function)
        });
    let immediate_control_users =
        backward_dataflow(function, def_use, reverse_postorder, |inputs, node_id| {
            immediate_control_flow(inputs, node_id, function)
        });

    // Step 2: calculate loop tree of function.
    let loops = loops(&control_subgraph, NodeID::new(0), &dom, fork_join_map);

    // Step 3: find most control dependent, shallowest loop level node for every
    // node.
    for idx in 0..function.nodes.len() {
        let above =
            dom.lowest_amongst(immediate_control_uses[idx].nodes(function.nodes.len() as u32));
        let below =
            dom.common_ancestor(immediate_control_users[idx].nodes(function.nodes.len() as u32));
    }

    todo!()
}

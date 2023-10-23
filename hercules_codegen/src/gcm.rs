extern crate hercules_ir;

use self::hercules_ir::dataflow::*;
use self::hercules_ir::def_use::*;
use self::hercules_ir::ir::*;

/*
 * Top level global code motion function. Assigns each data node to one of its
 * immediate control use / user nodes, forming (unordered) basic blocks. Returns
 * the control node / basic block each node is in.
 */
pub fn gcm(
    function: &Function,
    def_use: &ImmutableDefUseMap,
    reverse_postorder: &Vec<NodeID>,
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

    for i in 0..function.nodes.len() {
        for j in 0..function.nodes.len() {
            if immediate_control_uses[i].is_set(NodeID::new(j)) {
                println!("Node {} uses node {}", i, j);
            }
        }

        for j in 0..function.nodes.len() {
            if immediate_control_users[i].is_set(NodeID::new(j)) {
                println!("Node {} users node {}", i, j);
            }
        }
        println!("");
    }

    todo!()
}

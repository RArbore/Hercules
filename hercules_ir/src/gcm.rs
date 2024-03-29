use std::collections::HashMap;

use crate::*;

/*
 * Top level global code motion function. Assigns each data node to one of its
 * immediate control use / user nodes, forming (unordered) basic blocks. Returns
 * the control node / basic block each node is in.
 */
pub fn gcm(
    function: &Function,
    def_use: &ImmutableDefUseMap,
    reverse_postorder: &Vec<NodeID>,
    dom: &DomTree,
    antideps: &Vec<(NodeID, NodeID)>,
    loops: &LoopTree,
) -> Vec<NodeID> {
    // Step 1: find the immediate control uses and immediate control users of
    // each node.
    let mut immediate_control_uses =
        forward_dataflow(function, reverse_postorder, |inputs, node_id| {
            immediate_control_flow(inputs, node_id, function)
        });
    let mut immediate_control_users =
        backward_dataflow(function, def_use, reverse_postorder, |inputs, node_id| {
            immediate_control_flow(inputs, node_id, function)
        });

    // Reads and writes forming anti dependencies must be put in the same block.
    for (read, write) in antideps {
        let meet = UnionNodeSet::meet(
            &immediate_control_uses[read.idx()],
            &immediate_control_uses[write.idx()],
        );
        immediate_control_uses[read.idx()] = meet.clone();
        immediate_control_uses[write.idx()] = meet;

        let meet = UnionNodeSet::meet(
            &immediate_control_users[read.idx()],
            &immediate_control_users[write.idx()],
        );
        immediate_control_users[read.idx()] = meet.clone();
        immediate_control_users[write.idx()] = meet;
    }

    // Step 2: find most control dependent, shallowest loop level node for every
    // node.
    let bbs = (0..function.nodes.len())
        .map(|idx| {
            let highest =
                dom.lowest_amongst(immediate_control_uses[idx].nodes(function.nodes.len() as u32));
            let lowest = dom
                .common_ancestor(immediate_control_users[idx].nodes(function.nodes.len() as u32))
                .unwrap_or(highest);

            // If the ancestor of the control users isn't below the lowest
            // control use, then just place in the loewst control use.
            if !dom.does_dom(highest, lowest) {
                highest
            } else {
                // Collect in vector to reverse, since we want to traverse down
                // the dom tree, not up it.
                let mut chain = dom
                    .chain(lowest, highest)
                    .collect::<Vec<_>>()
                    .into_iter()
                    .rev();

                let mut location = chain.next().unwrap();
                while let Some(control_node) = chain.next() {
                    // Traverse down the dom tree until we find a loop.
                    if loops.contains(control_node) {
                        break;
                    } else {
                        location = control_node;
                    }
                }

                location
            }
        })
        .collect();

    bbs
}

/*
 * Find fork/join nests that each control node is inside of. Result is a map
 * from each control node to a list of fork nodes. The fork nodes are listed in
 * ascending order of nesting.
 */
pub fn compute_fork_join_nesting(
    function: &Function,
    dom: &DomTree,
    fork_join_map: &HashMap<NodeID, NodeID>,
) -> HashMap<NodeID, Vec<NodeID>> {
    // For each control node, ascend dominator tree, looking for fork nodes. For
    // each fork node, make sure each control node isn't strictly dominated by
    // the corresponding join node.
    (0..function.nodes.len())
        .map(NodeID::new)
        .filter(|id| function.nodes[id.idx()].is_control())
        .map(|id| {
            (
                id,
                dom.ascend(id)
                    .filter(|id| function.nodes[id.idx()].is_fork())
                    .filter(|fork_id| !dom.does_prop_dom(fork_join_map[&fork_id], id))
                    .collect(),
            )
        })
        .collect()
}

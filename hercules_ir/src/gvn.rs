use std::collections::HashMap;

use crate::*;

/*
 * Top level function to run global value numbering. In the sea of nodes, GVN is
 * fairly simple compared to a normal CFG.
 */
pub fn gvn(function: &mut Function, def_use: &ImmutableDefUseMap) {
    // Step 1: create worklist (starts as all nodes) and value number hashmap.
    let mut worklist: Vec<_> = (0..function.nodes.len()).rev().map(NodeID::new).collect();
    let mut value_numbers: HashMap<Node, NodeID> = HashMap::new();

    // Step 2: do worklist.
    while let Some(work) = worklist.pop() {
        if let Some(leader) = value_numbers.get(&function.nodes[work.idx()]) {
            // If there is a value number (a previously found Node ID) for the
            // current node, then replace all users' uses of the current work
            // node ID with the value number node ID.
            for user in def_use.get_users(work) {
                for u in get_uses_mut(&mut function.nodes[user.idx()]).as_mut() {
                    if **u == work {
                        **u = *leader;
                    }
                }
            }

            // Since all ex-users now use the value number node ID, delete this
            // node.
            function.nodes[work.idx()] = Node::Start;
        } else {
            // If not found, insert node with its own node ID as the value
            // number.
            value_numbers.insert(function.nodes[work.idx()].clone(), work);
        }
    }
}

extern crate hercules_ir;

use std::collections::HashMap;

use self::hercules_ir::def_use::*;
use self::hercules_ir::ir::*;

/*
 * Top level function to run global value numbering. In the sea of nodes, GVN is
 * fairly simple compared to in a normal CFG. Needs access to constants for
 * identity function simplification.
 */
pub fn gvn(function: &mut Function, constants: &Vec<Constant>, def_use: &ImmutableDefUseMap) {
    // Step 1: create worklist (starts as all nodes) and value number hashmap.
    let mut worklist: Vec<_> = (0..function.nodes.len()).rev().map(NodeID::new).collect();
    let mut value_numbers: HashMap<Node, NodeID> = HashMap::new();

    // Step 2: do worklist.
    while let Some(work) = worklist.pop() {
        // First, iteratively simplify the work node by unwrapping identity
        // functions.
        let value = crawl_identities(work, function, constants);

        // Next, check if there is a value number for this simplified value yet.
        if let Some(leader) = value_numbers.get(&function.nodes[value.idx()]) {
            // Also need to check that leader is not the current work ID. The
            // leader should never remove itself.
            if *leader != work {
                // If there is a value number (a previously found Node ID) for the
                // current node, then replace all users' uses of the current work
                // node ID with the value number node ID.
                for user in def_use.get_users(work) {
                    for u in get_uses_mut(&mut function.nodes[user.idx()]).as_mut() {
                        if **u == work {
                            **u = *leader;
                        }
                    }

                    // Since we modified user, it may now be congruent to other
                    // nodes, so add it back into the worklist.
                    worklist.push(*user);
                }

                // Since all ex-users now use the value number node ID, delete this
                // node.
                function.nodes[work.idx()] = Node::Start;

                // Explicitly continue to branch away from adding current work
                // as leader into value_numbers.
                continue;
            }
        }
        // If not found, insert the simplified node with its node ID as the
        // value number.
        value_numbers.insert(function.nodes[value.idx()].clone(), value);
    }
}

/*
 * Helper function for unwrapping identity functions.
 */
fn crawl_identities(mut work: NodeID, function: &Function, constants: &Vec<Constant>) -> NodeID {
    loop {
        // TODO: replace with API for saner pattern matching on IR. Also,
        // actually add the rest of the identity functions.
        if let Node::Binary {
            left,
            right,
            op: BinaryOperator::Add,
        } = function.nodes[work.idx()]
        {
            if let Node::Constant { id } = function.nodes[left.idx()] {
                if constants[id.idx()].is_zero() {
                    work = right;
                    continue;
                }
            }
        }

        if let Node::Binary {
            left,
            right,
            op: BinaryOperator::Add,
        } = function.nodes[work.idx()]
        {
            if let Node::Constant { id } = function.nodes[right.idx()] {
                if constants[id.idx()].is_zero() {
                    work = left;
                    continue;
                }
            }
        }

        return work;
    }
}

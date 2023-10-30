extern crate hercules_ir;

use self::hercules_ir::def_use::*;
use self::hercules_ir::ir::*;

/*
 * Top level function to run dead code elimination. Deletes nodes by setting
 * nodes to gravestones. Works with a function already containing gravestones.
 */
pub fn dce(function: &mut Function) {
    // Step 1: count number of users for each node.
    let mut num_users = vec![0; function.nodes.len()];
    for (idx, node) in function.nodes.iter().enumerate() {
        for u in get_uses(node).as_ref() {
            num_users[u.idx()] += 1;
        }

        // Return nodes shouldn't be considered dead code, so create a "phantom"
        // user.
        if node.is_return() {
            num_users[idx] += 1;
        }
    }

    // Step 2: worklist over zero user nodes.

    // Worklist starts as list of all nodes with 0 users.
    let mut worklist: Vec<_> = num_users
        .iter()
        .enumerate()
        .filter(|(_, num_users)| **num_users == 0)
        .map(|(idx, _)| idx)
        .collect();
    while let Some(work) = worklist.pop() {
        // Use start node as gravestone node value.
        let mut gravestone = Node::Start;
        std::mem::swap(&mut function.nodes[work], &mut gravestone);

        // Now that we set the gravestone, figure out other nodes that need to
        // be added to the worklist.
        for u in get_uses(&gravestone).as_ref() {
            num_users[u.idx()] -= 1;
            if num_users[u.idx()] == 0 {
                worklist.push(u.idx());
            }
        }
    }
}

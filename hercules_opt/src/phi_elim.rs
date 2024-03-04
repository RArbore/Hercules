/* A Hercules IR transformation that 
 * - Eliminates phi nodes where all inputs are the same (here this means the
 *   same node in IR, we are not performing GVM, SCCP, or any similar
 *   optimization)
 * - Eliminate regions with only a single predecessor
 *
 * The first of these optimizations is inspired by the description of the SSA
 * construction algorithm in Braun et al., CC 2013 used by the front-end. This
 * optimization performs the phi removal suggested by that paper, as in our
 * construction algorithm performing it during code generation is difficult.
 */
extern crate hercules_ir;

use std::collections::HashMap;

use self::hercules_ir::ir::*;
use self::hercules_ir::get_uses_mut;

/*
 * Top level function to run phi elimination, as described above.
 * Deletes nodes by setting nodes to gravestones. Works with a function already
 * containing gravestones.
 */
pub fn phi_elim(function : &mut Function) {
    // Keep a map of nodes that we need to replace, and what we need to replace
    // them with
    let mut replace_nodes : HashMap<usize, NodeID> = HashMap::new();

    // Iterate over the nodes of the function until convergence.
    // In this loop, we look for phis and regions that can be eliminated, mark
    // them as gravestones, and add them to the replacement map. For all other
    // nodes, we see if any of their arguments are in the replacement map and
    // if so eliminate them
    let mut changed = true;
    while changed {
        changed = false;

        for (idx, node) in function.nodes.iter_mut().enumerate() {
            // Replace any nodes that this node uses that are in the replacement
            // map
            for u in get_uses_mut(node).as_mut() {
                let old_id = u.idx();
                if let Some(replacement) = replace_nodes.get(&old_id) {
                    **u = *replacement;
                    changed = true;
                }
            }

            // Then, check if this node can be removed
            if let Node::Phi { control : _, data } = node {
                // For a phi, we can remove it if all of its data inputs are
                // the same node or self-cycles
                let mut unique = Some(data[0]);
                for i in 1..data.len() {
                    // Ignore self-loops
                    if data[i].idx() != idx && Some(data[i]) != unique{
                        if unique.unwrap().idx() == idx {
                            unique = Some(data[i]);
                        } else {
                            unique = None; break;
                        }
                    }
                }
                if let Some(value) = unique {
                    changed = true;
                    replace_nodes.insert(idx, value);
                    *node = Node::Start; // mark node as removed
                }
            } else if let Node::Region { preds } = node {
                if preds.len() == 1 {
                    changed = true;
                    replace_nodes.insert(idx, preds[0]);
                    *node = Node::Start; // mark as dead
                }
            }
        }
    }
}

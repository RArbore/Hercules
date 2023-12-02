extern crate hercules_ir;

use std::collections::HashMap;

use self::hercules_ir::ir::*;

/*
 * Top level function to allocate individual arrays for sets of nodes in the IR
 * graph. Doesn't attempt to overlap allocations w/ liveness analysis. Returns
 * a set of nodes per allocation, which are the nodes that use that allocation,
 * along with dimensions specified with dynamic constants.
 */
pub fn logical_array_alloc(
    function: &Function,
    fork_join_map: &HashMap<NodeID, NodeID>,
    bbs: &Vec<Vec<NodeID>>,
    fork_join_nests: &HashMap<NodeID, Vec<NodeID>>,
) -> () {
}

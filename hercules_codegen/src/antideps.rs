extern crate hercules_ir;

use self::hercules_ir::ir::*;

/*
 * Top level function to assemble anti-dependence edges. Returns a list of pairs
 * of nodes. The first item in the pair is the read node, and the second item is
 * the write node.
 */
pub fn antideps(function: &Function) -> Vec<(NodeID, NodeID)> {
    vec![]
}

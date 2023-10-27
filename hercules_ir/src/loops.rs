extern crate bitvec;

use std::collections::HashMap;

use self::bitvec::prelude::*;

use crate::*;

/*
 * Custom type for storing a loop tree. Each node corresponds to a single loop
 * or a fork join pair in the IR graph. Each node in the tree corresponds to
 * some subset of the overall IR graph. The root node corresponds to the entire
 * IR graph. The children of the root correspond to the top-level loops and fork
 * join pairs, and so on. Each node in the loop tree has a representative
 * "header" node. For normal loops, this is the region node branched to by a
 * dominated if node. For fork join pairs, this is the fork node. A loop is an
 * top-level loop if its parent is the root node of the subgraph.
 */
#[derive(Debug, Clone)]
pub struct LoopTree {
    root: NodeID,
    loops: HashMap<NodeID, (BitVec<u8, Lsb0>, NodeID)>,
}

impl LoopTree {}

/*
 * Top level function for calculating loop trees.
 */
pub fn loops(
    subgraph: &Subgraph,
    root: NodeID,
    dom: &DomTree,
    fork_join_map: &HashMap<NodeID, NodeID>,
) -> LoopTree {
    // Step 1: collect loop back edges.
    let mut loop_back_edges = vec![];
    for node in subgraph.iter() {
        // Check successors. Any successor dominating its predecessor is the
        // destination of a loop back edge.
        for succ in subgraph.succs(*node) {
            if dom.does_dom(succ, *node) {
                loop_back_edges.push((*node, succ));
            }
        }
    }

    // Step 2: collect "edges" from joins to forks. Technically, this doesn't
    // correspond to a real edge in the graph. However, our loop tree includes
    // fork join pairs as loops, so create a phantom loop back edge.
    for (fork, join) in fork_join_map {
        loop_back_edges.push((*join, *fork));
    }

    println!("{:?}", loop_back_edges);

    todo!()
}

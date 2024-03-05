extern crate bitvec;

use std::collections::hash_map;
use std::collections::HashMap;
use std::collections::VecDeque;

use self::bitvec::prelude::*;

use crate::*;

/*
 * Custom type for storing a loop tree. Each node corresponds to a single loop
 * or a fork join pair in the IR graph. Each node in the tree corresponds to
 * some subset of the overall IR graph. The root node corresponds to the entire
 * IR graph. The children of the root correspond to the top-level loops and fork
 * join pairs, and so on. Each node in the loop tree has a representative
 * "header" node. For normal loops, this is the region node branched to by a
 * dominated if node. For fork join pairs, this is the fork node. A loop is a
 * top-level loop if its parent is the root node of the subgraph. Each node in
 * the tree is an entry in the loops HashMap - the key is the "header" node for
 * the loop, and the key is a pair of the set of control nodes inside the loop
 * and this loop's parent header.
 */
#[derive(Debug, Clone)]
pub struct LoopTree {
    root: NodeID,
    loops: HashMap<NodeID, (BitVec<u8, Lsb0>, NodeID)>,
}

impl LoopTree {
    pub fn contains(&self, x: NodeID) -> bool {
        x == self.root || self.loops.contains_key(&x)
    }

    pub fn loops(&self) -> hash_map::Iter<'_, NodeID, (BitVec<u8, Lsb0>, NodeID)> {
        self.loops.iter()
    }

    /*
     * Sometimes, we need to iterate the loop tree bottom-up. Just assemble the
     * order upfront.
     */
    pub fn bottom_up_loops(&self) -> Vec<(NodeID, &BitVec<u8, Lsb0>)> {
        let mut bottom_up = vec![];
        let mut children_count: HashMap<NodeID, u32> = self.loops.iter().map(|(k, _)| (*k, 0)).collect();
        children_count.insert(self.root, 0);
        for (_, (_, parent)) in self.loops.iter() {
            *children_count.get_mut(&parent).unwrap() += 1;
        }
        let mut worklist: VecDeque<_> = self.loops.iter().map(|(k, v)| (*k, &v.0)).collect();
        while let Some(pop) = worklist.pop_front() {
            if children_count[&pop.0] == 0 {
                *children_count.get_mut(&self.loops[&pop.0].1).unwrap() -= 1;
                bottom_up.push(pop);
            } else {
                worklist.push_back(pop);
            }
        }
        bottom_up
    }
}

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

    // Step 3: find control nodes inside each loop. For a particular natural
    // loop with header d and a back edge from node n to d, the nodes in the
    // loop are d itself, and all nodes with a path to n not going through d.
    let loop_contents = loop_back_edges.iter().map(|(n, d)| {
        // Compute reachability for each loop back edge.
        let mut loop_contents = loop_reachability(*n, *d, subgraph);
        loop_contents.set(d.idx(), true);
        (d, loop_contents)
    });

    // Step 4: merge loops with same header into a single natural loop.
    let mut loops: HashMap<NodeID, BitVec<u8, Lsb0>> = HashMap::new();
    for (header, contents) in loop_contents {
        if loops.contains_key(header) {
            let old_contents = loops.remove(header).unwrap();
            loops.insert(*header, old_contents | contents);
        } else {
            loops.insert(*header, contents);
        }
    }

    // Step 5: figure out loop tree edges. A loop with header a can only be an
    // outer loop of a loop with header b if a dominates b.
    let loops = loops
        .iter()
        .map(|(header, contents)| {
            let mut dominator = *header;
            while let Some(new_dominator) = dom.imm_dom(dominator) {
                dominator = new_dominator;
                if let Some(outer_contents) = loops.get(&dominator) {
                    if outer_contents[header.idx()] {
                        return (*header, (contents.clone(), dominator));
                    }
                }
            }
            (*header, (contents.clone(), root))
        })
        .collect();

    LoopTree { root, loops }
}

fn loop_reachability(n: NodeID, d: NodeID, subgraph: &Subgraph) -> BitVec<u8, Lsb0> {
    let visited = bitvec![u8, Lsb0; 0; subgraph.original_num_nodes() as usize];

    // n is the root of the traversal, finding d is a termination condition.
    let visited = loop_reachability_helper(n, d, subgraph, visited);

    visited
}

fn loop_reachability_helper(
    n: NodeID,
    d: NodeID,
    subgraph: &Subgraph,
    mut visited: BitVec<u8, Lsb0>,
) -> BitVec<u8, Lsb0> {
    if visited[n.idx()] {
        // If already visited, return early.
        visited
    } else {
        // Set visited to true.
        visited.set(n.idx(), true);

        // Iterate over predecessors.
        for pred in subgraph.preds(n) {
            // Don't traverse d.
            if pred != d {
                visited = loop_reachability_helper(pred, d, subgraph, visited);
            }
        }

        visited
    }
}

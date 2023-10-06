use crate::*;

use std::collections::HashMap;

/*
 * In various parts of the compiler, we want to consider a subset of a complete
 * function graph. For example, for dominators, we often only want to find the
 * dominator tree of only the control subgraph.
 */
#[derive(Debug, Clone)]
pub struct Subgraph {
    nodes: Vec<NodeID>,
    node_numbers: HashMap<NodeID, u32>,
    first_forward_edges: Vec<u32>,
    forward_edges: Vec<u32>,
    first_backward_edges: Vec<u32>,
    backward_edges: Vec<u32>,
}

pub struct SubgraphIterator<'a> {
    nodes: &'a Vec<NodeID>,
    edges: &'a [u32],
}

impl<'a> Iterator for SubgraphIterator<'a> {
    type Item = NodeID;

    fn next(&mut self) -> Option<Self::Item> {
        if self.edges.len() == 0 {
            None
        } else {
            let id = self.edges[0];
            self.edges = &self.edges[1..];
            Some(self.nodes[id as usize])
        }
    }
}

impl Subgraph {
    pub fn num_nodes(&self) -> u32 {
        self.nodes.len() as u32
    }

    pub fn contains_node(&self, id: NodeID) -> bool {
        self.node_numbers.contains_key(&id)
    }

    pub fn preds(&self, id: NodeID) -> SubgraphIterator {
        let number = self.node_numbers[&id];
        if ((number + 1) as usize) < self.first_backward_edges.len() {
            SubgraphIterator {
                nodes: &self.nodes,
                edges: &self.backward_edges[(self.first_backward_edges[number as usize] as usize)
                    ..(self.first_backward_edges[number as usize + 1] as usize)],
            }
        } else {
            SubgraphIterator {
                nodes: &self.nodes,
                edges: &self.backward_edges
                    [(self.first_backward_edges[number as usize] as usize)..],
            }
        }
    }

    pub fn succs(&self, id: NodeID) -> SubgraphIterator {
        let number = self.node_numbers[&id];
        if ((number + 1) as usize) < self.first_forward_edges.len() {
            SubgraphIterator {
                nodes: &self.nodes,
                edges: &self.forward_edges[(self.first_forward_edges[number as usize] as usize)
                    ..(self.first_forward_edges[number as usize + 1] as usize)],
            }
        } else {
            SubgraphIterator {
                nodes: &self.nodes,
                edges: &self.forward_edges[(self.first_forward_edges[number as usize] as usize)..],
            }
        }
    }

    pub fn reverse(self, new_root: NodeID) -> Self {
        let Subgraph {
            mut nodes,
            mut node_numbers,
            first_forward_edges,
            forward_edges,
            mut first_backward_edges,
            mut backward_edges,
        } = self;

        // Since we need to add a "new" root to the subgraph, we first need to
        // identify all the nodes with no forward edges. We're going to
        // simultaneously add the new backward edges from the old leaves to the
        // new root.
        let mut leaf_numbers = vec![];
        let mut new_first_forward_edges = vec![];
        let mut new_forward_edges = vec![];
        let mut old_forward_edges_idx = 0;
        for number in 0..nodes.len() as u32 {
            new_first_forward_edges.push(new_forward_edges.len() as u32);
            let num_edges = if ((number + 1) as usize) < first_forward_edges.len() {
                first_forward_edges[number as usize + 1] - first_forward_edges[number as usize]
            } else {
                forward_edges.len() as u32 - first_forward_edges[number as usize]
            };
            if num_edges == 0 {
                // Node number of new root will be largest in subgraph.
                new_forward_edges.push(nodes.len() as u32);
                leaf_numbers.push(number);
            } else {
                for _ in 0..num_edges {
                    new_forward_edges.push(forward_edges[old_forward_edges_idx]);
                    old_forward_edges_idx += 1;
                }
            }
        }

        // There are no backward edges from the root node.
        new_first_forward_edges.push(new_forward_edges.len() as u32);

        // To reverse the edges in the graph, just swap the forward and backward
        // edge vectors. Thus, we add the forward edges from the new root to
        // the old leaves in the backward edge arrays.
        node_numbers.insert(new_root, nodes.len() as u32);
        nodes.push(new_root);
        first_backward_edges.push(backward_edges.len() as u32);
        for leaf in leaf_numbers.iter() {
            backward_edges.push(*leaf);
        }

        // Swap forward and backward edges.
        assert!(nodes.len() == first_backward_edges.len());
        assert!(nodes.len() == new_first_forward_edges.len());
        Subgraph {
            nodes,
            node_numbers,
            first_forward_edges: first_backward_edges,
            forward_edges: backward_edges,
            first_backward_edges: new_first_forward_edges,
            backward_edges: new_forward_edges,
        }
    }
}

/*
 * Top level subgraph construction routine. Takes a function reference and a
 * predicate - the predicate selects which nodes from the function will be
 * included in the subgraph. An edge is added to the subgraph if it's between
 * two nodes that each pass the predicate.
 */
pub fn subgraph<F>(function: &Function, def_use: &ImmutableDefUseMap, predicate: F) -> Subgraph
where
    F: Fn(&Node) -> bool,
{
    let mut subgraph = Subgraph {
        nodes: vec![],
        node_numbers: HashMap::new(),
        first_forward_edges: vec![],
        forward_edges: vec![],
        first_backward_edges: vec![],
        backward_edges: vec![],
    };

    // Step 1: collect predicated nodes.
    for (idx, node) in function.nodes.iter().enumerate() {
        if predicate(node) {
            subgraph
                .node_numbers
                .insert(NodeID::new(idx), subgraph.nodes.len() as u32);
            subgraph.nodes.push(NodeID::new(idx));
        }
    }

    // Step 2: collect backwards edges. This is fairly easy, since use-def
    // edges are explicitly stored.
    for id in subgraph.nodes.iter() {
        subgraph
            .first_backward_edges
            .push(subgraph.backward_edges.len() as u32);
        let uses = get_uses(&function.nodes[id.idx()]);
        for use_id in uses.as_ref() {
            // Any predecessor node that satisfies the predicate already got
            // added to node numbers. We need to get the node number anyway,
            // so we don't have to do a redundant predicate check.
            if let Some(number) = subgraph.node_numbers.get(use_id) {
                subgraph.backward_edges.push(*number);
            }
        }
    }

    // Step 3: collect forwards edges. This is also easy, since we already have
    // the def-use edges of this function.
    for id in subgraph.nodes.iter() {
        subgraph
            .first_forward_edges
            .push(subgraph.forward_edges.len() as u32);

        // Only difference is that we iterate over users, not uses.
        let users = def_use.get_users(*id);
        for user_id in users.as_ref() {
            // Any successor node that satisfies the predicate already got
            // added to node numbers. We need to get the node number anyway,
            // so we don't have to do a redundant predicate check.
            if let Some(number) = subgraph.node_numbers.get(user_id) {
                subgraph.forward_edges.push(*number);
            }
        }
    }

    subgraph
}

/*
 * Get the control subgraph of a function.
 */
pub fn control_subgraph(function: &Function, def_use: &ImmutableDefUseMap) -> Subgraph {
    use Node::*;

    subgraph(function, def_use, |node| match node {
        Start
        | Region { preds: _ }
        | If {
            control: _,
            cond: _,
        }
        | Fork {
            control: _,
            factor: _,
        }
        | Join { control: _ }
        | Return {
            control: _,
            data: _,
        }
        | Match { control: _, sum: _ } => true,
        ReadProd { prod, index } => match function.nodes[prod.idx()] {
            // ReadProd nodes are control nodes if their predecessor is a
            // legal control node, and if it's the right index.
            Match { control: _, sum: _ }
            | If {
                control: _,
                cond: _,
            } => true,
            _ => false,
        },
        _ => false,
    })
}

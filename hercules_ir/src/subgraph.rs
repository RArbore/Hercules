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
        | Join {
            control: _,
            data: _,
        }
        | Return {
            control: _,
            value: _,
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
            Fork {
                control: _,
                factor: _,
            }
            | Join {
                control: _,
                data: _,
            } => *index == 0,
            _ => false,
        },
        _ => false,
    })
}

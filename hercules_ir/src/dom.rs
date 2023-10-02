extern crate bitvec;

use crate::*;

use std::collections::HashMap;

/*
 * Custom type for storing a dominator tree. For each control node, store its
 * immediate dominator.
 */
#[derive(Debug, Clone)]
pub struct DomTree {
    idom: HashMap<NodeID, NodeID>,
}

impl DomTree {
    pub fn imm_dom(&self, x: NodeID) -> Option<NodeID> {
        self.idom.get(&x).map(|x| x.clone())
    }

    pub fn does_imm_dom(&self, a: NodeID, b: NodeID) -> bool {
        self.imm_dom(b) == Some(a)
    }

    pub fn does_dom(&self, a: NodeID, b: NodeID) -> bool {
        let mut iter = Some(b);

        // Go up dominator tree until finding a, or root of tree.
        while let Some(b) = iter {
            if b == a {
                return true;
            }
            iter = self.imm_dom(b);
        }
        false
    }

    pub fn does_prop_dom(&self, a: NodeID, b: NodeID) -> bool {
        a != b && self.does_dom(a, b)
    }
}

/*
 * Top level function for calculating dominator trees. Uses the semi-NCA
 * algorithm, as described in "Finding Dominators in Practice".
 */
pub fn dominator(function: &Function) -> DomTree {
    // Step 1: compute the sub-CFG for the function. This is the graph the
    // dominator tree will be built for.
    let backward_sub_cfg = control_nodes(function);
    let forward_sub_cfg = reorient_sub_cfg(&backward_sub_cfg);

    // Step 2: compute pre-order DFS of CFG.
    let (preorder, mut parents) = preorder(&forward_sub_cfg);
    let mut node_numbers = HashMap::new();
    for (number, node) in preorder.iter().enumerate() {
        node_numbers.insert(node, number);
    }

    // Step 3: define eval, which will be used to compute semi-dominators.
    let mut eval_stack = vec![];
    let mut labels: Vec<_> = (0..preorder.len()).collect();
    let mut eval = |v, last_linked, mut parents: HashMap<NodeID, NodeID>, semi: Vec<NodeID>| {
        let p_v = &parents[v];
        let p_v_n = node_numbers[p_v];
        if p_v_n < last_linked {
            return (labels[p_v_n], parents, semi);
        }

        // Get ancestors of v, except for the virtual root.
        assert!(eval_stack.is_empty());
        let mut iter = *v;
        let mut p_iter = parents[v];
        loop {
            eval_stack.push(iter);
            iter = p_iter;
            p_iter = parents[&iter];
            if node_numbers[&p_iter] < last_linked {
                break;
            }
        }

        // Perform path compression.
        let mut iter_label_number = labels[node_numbers[&iter]];
        for node in eval_stack.drain(..).rev() {
            *parents.get_mut(&node).unwrap() = parents[&iter];
            let node_label_number = labels[node_numbers[&node]];
            if node_numbers[&semi[iter_label_number]] < node_numbers[&semi[node_label_number]] {
                labels[node_numbers[&node]] = labels[node_numbers[&iter]]
            } else {
                iter_label_number = node_label_number;
            }
            iter = node;
        }

        return (labels[node_numbers[&iter]], parents, semi);
    };

    // Step 4: initialize idom.
    let mut idom = HashMap::new();
    for w in preorder[1..].iter() {
        // Each idom starts as the parent node.
        idom.insert(w, parents[w]);
    }

    // Step 5: compute semi-dominators. This implementation is based off of
    // LLVM's dominator implementation.
    let mut semi = vec![NodeID::new(0); preorder.len()];
    for w_n in (2..preorder.len()).rev() {
        let w = preorder[w_n];
        semi[w_n] = parents[&w];
        for v in forward_sub_cfg[&w].iter() {
            let (new_semi_index, new_parents, new_semi) = eval(&v, w_n + 1, parents, semi);
            parents = new_parents;
            semi = new_semi;
            let new_semi_node = semi[new_semi_index];
            if node_numbers[&new_semi_node] < node_numbers[&semi[w_n]] {
                semi[w_n] = new_semi_node;
            }
        }
    }

    todo!()
}

/*
 * Enum for storing control uses of a node. Calculated alongside control nodes
 * in control_nodes.
 */
#[derive(Debug, Clone)]
pub enum ControlUses<'a> {
    Zero,
    One([NodeID; 1]),
    Variable(&'a Box<[NodeID]>),
}

impl<'a> AsRef<[NodeID]> for ControlUses<'a> {
    fn as_ref(&self) -> &[NodeID] {
        match self {
            ControlUses::Zero => &[],
            ControlUses::One(x) => x,
            ControlUses::Variable(x) => x,
        }
    }
}

pub type BackwardSubCFG<'a> = HashMap<NodeID, ControlUses<'a>>;

/*
 * Top level function for getting all the control nodes in a function. Also
 * returns the control uses of each control node, in effect returning the
 * control subset of the IR graph.
 */
pub fn control_nodes(function: &Function) -> BackwardSubCFG {
    use Node::*;

    let mut control_nodes = HashMap::new();
    for (idx, node) in function.nodes.iter().enumerate() {
        match node {
            Start => {
                control_nodes.insert(NodeID::new(idx), ControlUses::Zero);
            }
            Region { preds } => {
                control_nodes.insert(NodeID::new(idx), ControlUses::Variable(&preds));
            }
            If { control, cond: _ }
            | Fork { control, factor: _ }
            | Join { control, data: _ }
            | Return { control, value: _ }
            | Match { control, sum: _ } => {
                control_nodes.insert(NodeID::new(idx), ControlUses::One([*control]));
            }
            ReadProd { prod, index } => match function.nodes[prod.idx()] {
                // ReadProd nodes are control nodes if their predecessor is a
                // legal control node, and if it's the right index.
                Match { control: _, sum: _ }
                | If {
                    control: _,
                    cond: _,
                } => {
                    control_nodes.insert(NodeID::new(idx), ControlUses::One([*prod]));
                }
                Fork {
                    control: _,
                    factor: _,
                }
                | Join {
                    control: _,
                    data: _,
                } => {
                    if *index == 0 {
                        control_nodes.insert(NodeID::new(idx), ControlUses::One([*prod]));
                    }
                }
                _ => {}
            },
            _ => {}
        }
    }
    control_nodes
}

pub type ForwardSubCFG = HashMap<NodeID, Vec<NodeID>>;

/*
 * Utility for getting def-use edges of sub CFG.
 */
pub fn reorient_sub_cfg(backward: &BackwardSubCFG) -> ForwardSubCFG {
    let mut forward = HashMap::new();

    // Every control node needs to be a key in forward, even if it has no
    // def-use edges originating from it (the return node), so explicitly add
    // them all here.
    for key in backward.keys() {
        forward.insert(*key, vec![]);
    }

    // Then, insert def-use edges. Unwrap since all keys are initialized above
    // with empty vectors.
    for (user, defs) in backward.iter() {
        for def in defs.as_ref() {
            forward.get_mut(def).unwrap().push(*user);
        }
    }

    forward
}

fn preorder(forward_sub_cfg: &ForwardSubCFG) -> (Vec<NodeID>, HashMap<NodeID, NodeID>) {
    // Initialize order vector and visited hashmap for tracking which nodes have
    // been visited.
    let order = Vec::with_capacity(forward_sub_cfg.len());

    // Explicitly keep track of parents in DFS tree. Doubles as a visited set.
    let parents = HashMap::new();

    // Order and parents are threaded through arguments / return pair of
    // reverse_postorder_helper for ownership reasons.
    preorder_helper(NodeID::new(0), forward_sub_cfg, order, parents)
}

fn preorder_helper(
    node: NodeID,
    forward_sub_cfg: &ForwardSubCFG,
    mut order: Vec<NodeID>,
    mut parents: HashMap<NodeID, NodeID>,
) -> (Vec<NodeID>, HashMap<NodeID, NodeID>) {
    assert!(forward_sub_cfg.contains_key(&node));
    if parents.contains_key(&node) {
        // If already visited, return early.
        (order, parents)
    } else {
        // Keep track of DFS parent for region nodes.
        if let Some(parent) = order.last() {
            // Only node where the above isn't true is the start node, which
            // has no incoming edge. Thus, there's no need to insert the start
            // node into the parents map for tracking visitation.
            parents.insert(node, *parent);
        }

        // Before iterating users, push this node.
        order.push(node);

        // Iterate over users.
        for user in forward_sub_cfg.get(&node).unwrap() {
            (order, parents) = preorder_helper(*user, forward_sub_cfg, order, parents);
        }

        (order, parents)
    }
}

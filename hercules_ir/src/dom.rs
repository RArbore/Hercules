extern crate bitvec;

use crate::*;

use std::collections::BTreeMap;

/*
 * Custom type for storing a dominator tree. For each control node, store its
 * immediate dominator.
 */
#[derive(Debug, Clone)]
pub struct DomTree {
    idom: BTreeMap<NodeID, NodeID>,
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
    let mut node_numbers = BTreeMap::new();
    for (number, node) in preorder.iter().enumerate() {
        node_numbers.insert(node, number);
    }
    parents.insert(NodeID::new(0), NodeID::new(0));
    println!("Backward: {:?}", backward_sub_cfg);
    println!("Forward: {:?}", forward_sub_cfg);
    println!("Preorder: {:?}", preorder);
    println!("Parents: {:?}", parents);
    println!("Node Numbers: {:?}", node_numbers);
    let mut idom = BTreeMap::new();
    for w in preorder[1..].iter() {
        // Each idom starts as the parent node.
        idom.insert(*w, parents[w]);
    }

    // Step 3: define snca_compress, which will be used to compute semi-
    // dominators, and initialize various variables.
    let mut semi = vec![0; preorder.len()];
    let mut labels: Vec<_> = (0..preorder.len()).collect();
    let mut ancestors = vec![0; preorder.len()];
    fn snca_compress(
        v_n: usize,
        mut ancestors: Vec<usize>,
        mut labels: Vec<usize>,
    ) -> (Vec<usize>, Vec<usize>) {
        let u_n = ancestors[v_n];

        if u_n != 0 {
            (ancestors, labels) = snca_compress(u_n, ancestors, labels);
            if labels[u_n] < labels[v_n] {
                labels[v_n] = labels[u_n];
            }
            ancestors[v_n] = ancestors[u_n];
        }

        (ancestors, labels)
    }

    // Step 4: compute semi-dominators.
    for w_n in (1..preorder.len()).rev() {
        println!("w: {:?}   w_n: {:?}", preorder[w_n], w_n);
        println!(
            "Semis: {:?}",
            (0..preorder.len())
                .map(|idx| (preorder[idx].idx(), preorder[semi[idx]].idx()))
                .collect::<BTreeMap<_, _>>()
        );

        semi[w_n] = w_n;
        for v in backward_sub_cfg[&preorder[w_n]].as_ref() {
            let v_n = node_numbers[&v];
            (ancestors, labels) = snca_compress(v_n, ancestors, labels);
            semi[w_n] = std::cmp::min(semi[w_n], labels[v_n]);
        }
        labels[w_n] = semi[w_n];
        ancestors[w_n] = node_numbers[&parents[&preorder[w_n]]];

        println!(
            "Semis: {:?}",
            (0..preorder.len())
                .map(|idx| (preorder[idx].idx(), preorder[semi[idx]].idx()))
                .collect::<BTreeMap<_, _>>()
        );
        println!("");
    }

    println!(
        "Semis: {:?}",
        (0..preorder.len())
            .map(|idx| (preorder[idx].idx(), preorder[semi[idx]].idx()))
            .collect::<BTreeMap<_, _>>()
    );

    // Step 5: compute idom.
    for v_n in 1..preorder.len() {
        let v = preorder[v_n];
        while node_numbers[&idom[&v]] > semi[v_n] {
            *idom.get_mut(&v).unwrap() = idom[&idom[&v]];
        }
    }

    println!("Immediate Dominators: {:?}", idom);

    DomTree { idom }
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

pub type BackwardSubCFG<'a> = BTreeMap<NodeID, ControlUses<'a>>;

/*
 * Top level function for getting all the control nodes in a function. Also
 * returns the control uses of each control node, in effect returning the
 * control subset of the IR graph.
 */
pub fn control_nodes(function: &Function) -> BackwardSubCFG {
    use Node::*;

    let mut control_nodes = BTreeMap::new();
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

pub type ForwardSubCFG = BTreeMap<NodeID, Vec<NodeID>>;

/*
 * Utility for getting def-use edges of sub CFG.
 */
pub fn reorient_sub_cfg(backward: &BackwardSubCFG) -> ForwardSubCFG {
    let mut forward = BTreeMap::new();

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

fn preorder(forward_sub_cfg: &ForwardSubCFG) -> (Vec<NodeID>, BTreeMap<NodeID, NodeID>) {
    // Initialize order vector and visited hashmap for tracking which nodes have
    // been visited.
    let order = Vec::with_capacity(forward_sub_cfg.len());

    // Explicitly keep track of parents in DFS tree. Doubles as a visited set.
    let parents = BTreeMap::new();

    // Order and parents are threaded through arguments / return pair of
    // reverse_postorder_helper for ownership reasons.
    preorder_helper(NodeID::new(0), None, forward_sub_cfg, order, parents)
}

fn preorder_helper(
    node: NodeID,
    parent: Option<NodeID>,
    forward_sub_cfg: &ForwardSubCFG,
    mut order: Vec<NodeID>,
    mut parents: BTreeMap<NodeID, NodeID>,
) -> (Vec<NodeID>, BTreeMap<NodeID, NodeID>) {
    assert!(forward_sub_cfg.contains_key(&node));
    if parents.contains_key(&node) {
        // If already visited, return early.
        (order, parents)
    } else {
        // Keep track of DFS parent for region nodes.
        if let Some(parent) = parent {
            // Only node where the above isn't true is the start node, which
            // has no incoming edge. Thus, there's no need to insert the start
            // node into the parents map for tracking visitation.
            parents.insert(node, parent);
        }

        // Before iterating users, push this node.
        order.push(node);

        // Iterate over users.
        for user in forward_sub_cfg.get(&node).unwrap() {
            (order, parents) = preorder_helper(*user, Some(node), forward_sub_cfg, order, parents);
        }

        (order, parents)
    }
}

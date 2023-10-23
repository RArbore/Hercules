use std::collections::HashMap;

use crate::*;

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

    /*
     * Check if a node is in the dom tree (if the node is the root of the tree,
     * will still return true).
     */
    pub fn is_non_root(&self, x: NodeID) -> bool {
        self.idom.contains_key(&x)
    }

    /*
     * Typically, node ID 0 is the root of the dom tree. Under this assumption,
     * this function checks if a node is in the dom tree.
     */
    pub fn contains_conventional(&self, x: NodeID) -> bool {
        x == NodeID::new(0) || self.idom.contains_key(&x)
    }

    pub fn get_underlying_map(&self) -> &HashMap<NodeID, NodeID> {
        &self.idom
    }
}

/*
 * Top level function for calculating dominator trees. Uses the semi-NCA
 * algorithm, as described in "Finding Dominators in Practice".
 */
pub fn dominator(subgraph: &Subgraph, root: NodeID) -> DomTree {
    // Step 1: compute pre-order DFS of subgraph.
    let (preorder, mut parents) = preorder(&subgraph, root);
    let mut node_numbers = HashMap::new();
    for (number, node) in preorder.iter().enumerate() {
        node_numbers.insert(node, number);
    }
    parents.insert(root, root);
    let mut idom = HashMap::new();
    for w in preorder[1..].iter() {
        // Each idom starts as the parent node.
        idom.insert(*w, parents[w]);
    }

    // Step 2: define snca_compress, which will be used to compute semi-
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

    // Step 3: compute semi-dominators.
    for w_n in (1..preorder.len()).rev() {
        semi[w_n] = w_n;
        for v in subgraph.preds(preorder[w_n]) {
            let v_n = node_numbers[&v];
            (ancestors, labels) = snca_compress(v_n, ancestors, labels);
            semi[w_n] = std::cmp::min(semi[w_n], labels[v_n]);
        }
        labels[w_n] = semi[w_n];
        ancestors[w_n] = node_numbers[&parents[&preorder[w_n]]];
    }

    // Step 4: compute idom.
    for v_n in 1..preorder.len() {
        let v = preorder[v_n];
        while node_numbers[&idom[&v]] > semi[v_n] {
            *idom.get_mut(&v).unwrap() = idom[&idom[&v]];
        }
    }

    DomTree { idom }
}

fn preorder(subgraph: &Subgraph, root: NodeID) -> (Vec<NodeID>, HashMap<NodeID, NodeID>) {
    // Initialize order vector and visited hashmap for tracking which nodes have
    // been visited.
    let order = Vec::with_capacity(subgraph.num_nodes() as usize);

    // Explicitly keep track of parents in DFS tree. Doubles as a visited set.
    let parents = HashMap::new();

    // Order and parents are threaded through arguments / return pair of
    // reverse_postorder_helper for ownership reasons.
    preorder_helper(root, None, subgraph, order, parents)
}

fn preorder_helper(
    node: NodeID,
    parent: Option<NodeID>,
    subgraph: &Subgraph,
    mut order: Vec<NodeID>,
    mut parents: HashMap<NodeID, NodeID>,
) -> (Vec<NodeID>, HashMap<NodeID, NodeID>) {
    assert!(subgraph.contains_node(node));
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
        for user in subgraph.succs(node) {
            (order, parents) = preorder_helper(user, Some(node), subgraph, order, parents);
        }

        (order, parents)
    }
}

/*
 * Top level function for calculating post-dominator trees. Reverses the edges
 * in the subgraph, and then runs normal dominator analysis. Takes an owned
 * subgraph, since we need to reverse it. Also take a fake root node ID to
 * insert in the reversed subgraph. This will be the root of the resulting
 * dominator tree.
 */
pub fn postdominator(subgraph: Subgraph, fake_root: NodeID) -> DomTree {
    // Step 1: reverse the subgraph.
    let reversed_subgraph = subgraph.reverse(fake_root);

    // Step 2: run dominator analysis on the reversed subgraph. Use the fake
    // root as the root of the dominator analysis.
    dominator(&reversed_subgraph, fake_root)
}

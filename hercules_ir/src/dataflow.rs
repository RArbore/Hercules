extern crate bitvec;

use dataflow::bitvec::prelude::*;

use crate::*;

/*
 * Trait for a type that is a semilattice. Semilattice types must also be Eq,
 * so that the dataflow analysis can determine when to terminate.
 */
pub trait Semilattice: Eq {
    fn meet(a: &Self, b: &Self) -> Self;
    fn bottom() -> Self;
    fn top() -> Self;
}

/*
 * Top level dataflow function. This routine is slightly more generic than the
 * typical textbook definition. The flow function takes an ordered slice of
 * predecessor lattice values, rather than an unordered set. Thus, the flow
 * function can perform non-associative operations on the "in" lattice values.
 * This makes this routine useful for some analyses, such as typechecking.
 */
pub fn dataflow<L, F, D>(
    function: &Function,
    reverse_post_order: &Vec<NodeID>,
    flow_function: F,
    auxiliary_data: &D,
) -> Vec<L>
where
    L: Semilattice,
    F: Fn(&[&L], &D, NodeID) -> L,
{
    // Step 1: create initial set of "in" points. The start node is initialized
    // to bottom, and everything else is initialized to top.
    let ins: Vec<L> = (0..function.nodes.len())
        .map(|id| if id == 0 { L::bottom() } else { L::top() })
        .collect();

    // Step 2: create initial set of "out" points.
    let mut outs: Vec<L> = ins
        .into_iter()
        .enumerate()
        .map(|(id, l)| flow_function(&[&l], auxiliary_data, NodeID::new(id)))
        .collect();

    // Step 3: compute NodeUses for each node in function.
    let uses: Vec<NodeUses> = function.nodes.iter().map(|n| get_uses(n)).collect();

    // Step 4: peform main dataflow loop.
    loop {
        let mut change = false;

        // Iterate nodes in reverse post order.
        for node in reverse_post_order {
            // Assemble the "out" values of the predecessors of this node. This
            // vector's definition is hopefully LICMed out, so that we don't do
            // an allocation per node. This can't be done manually because of
            // Rust's ownership rules (in particular, pred_outs holds a
            // reference to a value inside outs, which is mutated below).
            let mut pred_outs = vec![];
            for u in uses[node.idx()].as_ref() {
                pred_outs.push(&outs[u.idx()]);
            }

            // Compute new "out" value from predecessor "out" values.
            let new_out = flow_function(&pred_outs[..], auxiliary_data, *node);
            if outs[node.idx()] != new_out {
                change = true;
            }

            // Update outs vector.
            outs[node.idx()] = new_out;
        }

        // If no lattice value changed, we've reached the maximum fixed point
        // solution, and can terminate.
        if !change {
            break;
        }
    }

    // Step 5: return "out" set.
    outs
}

/*
 * Compute reverse post order of nodes in function.
 */
pub fn reverse_postorder(def_uses: &ImmutableDefUseMap) -> Vec<NodeID> {
    // Initialize order vector and bitset for tracking which nodes have been
    // visited.
    let order = Vec::with_capacity(def_uses.num_nodes());
    let visited = bitvec![u8, Lsb0; 0; def_uses.num_nodes()];

    // Order and visited are threaded through arguments / return pair of
    // reverse_postorder_helper for ownership reasons.
    let (mut order, _) = reverse_postorder_helper(NodeID::new(0), def_uses, order, visited);

    // Reverse order in-place.
    order.reverse();
    order
}

fn reverse_postorder_helper(
    node: NodeID,
    def_uses: &ImmutableDefUseMap,
    mut order: Vec<NodeID>,
    mut visited: BitVec<u8, Lsb0>,
) -> (Vec<NodeID>, BitVec<u8, Lsb0>) {
    if visited[node.idx()] {
        // If already visited, return early.
        (order, visited)
    } else {
        // Set visited to true.
        visited.set(node.idx(), true);

        // Iterate over users.
        for user in def_uses.get_users(node) {
            (order, visited) = reverse_postorder_helper(*user, def_uses, order, visited);
        }

        // After iterating users, push this node.
        order.push(node);
        (order, visited)
    }
}

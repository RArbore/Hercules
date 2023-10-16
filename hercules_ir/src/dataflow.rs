extern crate bitvec;

use dataflow::bitvec::prelude::*;

use crate::*;

/*
 * Trait for a type that is a semilattice. Semilattice types must also be Eq,
 * so that the dataflow analysis can determine when to terminate.
 */
pub trait Semilattice: Eq + Clone {
    fn meet(a: &Self, b: &Self) -> Self;
    fn bottom() -> Self;
    fn top() -> Self;
}

/*
 * Top level forward dataflow function. This routine is slightly more generic
 * than the typical textbook definition. The flow function takes an ordered
 * slice of predecessor lattice values, rather than a single lattice value.
 * Thus, the flow function can perform non-associative and non-commutative
 * operations on the "in" lattice values. This makes this routine more useful
 * for some analyses, such as typechecking. To perform the typical behavior,
 * the flow function should start by meeting the input lattice values into a
 * single lattice value.
 */
pub fn forward_dataflow<L, F>(
    function: &Function,
    reverse_postorder: &Vec<NodeID>,
    mut flow_function: F,
) -> Vec<L>
where
    L: Semilattice,
    F: FnMut(&[&L], NodeID) -> L,
{
    forward_dataflow_global(function, reverse_postorder, |global_outs, node_id| {
        let uses = get_uses(&function.nodes[node_id.idx()]);
        let pred_outs: Vec<_> = uses
            .as_ref()
            .iter()
            .map(|id| &global_outs[id.idx()])
            .collect();
        flow_function(&pred_outs, node_id)
    })
}

/*
 * The previous forward dataflow routine wraps around this dataflow routine,
 * where the flow function doesn't just have access to this nodes input lattice
 * values, but also all the current lattice values for all the nodes. This is
 * useful for some dataflow analyses, such as typechecking and reachability. The
 * "global" in forward_dataflow_global refers to having a global view of the out
 * lattice values.
 */
pub fn forward_dataflow_global<L, F>(
    function: &Function,
    reverse_postorder: &Vec<NodeID>,
    mut flow_function: F,
) -> Vec<L>
where
    L: Semilattice,
    F: FnMut(&[L], NodeID) -> L,
{
    // Step 1: create initial set of "out" points.
    let start_node_output = flow_function(&[], NodeID::new(0));
    let mut first_ins = vec![L::top(); function.nodes.len()];
    first_ins[0] = start_node_output;
    let mut outs: Vec<L> = (0..function.nodes.len())
        .map(|id| flow_function(&first_ins, NodeID::new(id)))
        .collect();

    // Step 2: peform main dataflow loop.
    loop {
        let mut change = false;

        // Iterate nodes in reverse post order.
        for node_id in reverse_postorder {
            // Compute new "out" value from predecessor "out" values.
            let new_out = flow_function(&outs, *node_id);
            if outs[node_id.idx()] != new_out {
                change = true;
            }

            // Update outs vector.
            outs[node_id.idx()] = new_out;
        }

        // If no lattice value changed, we've reached the maximum fixed point
        // solution, and can terminate.
        if !change {
            break;
        }
    }

    // Step 3: return "out" set.
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

/*
 * A bit vector set is a very general kind of semilattice. This variant is for
 * "intersecting" flow functions.
 */
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum IntersectNodeSet {
    Empty,
    Bits(BitVec<u8, Lsb0>),
    Full,
}

impl IntersectNodeSet {
    pub fn is_set(&self, id: NodeID) -> bool {
        match self {
            IntersectNodeSet::Empty => false,
            IntersectNodeSet::Bits(bits) => bits[id.idx()],
            IntersectNodeSet::Full => true,
        }
    }
}

impl Semilattice for IntersectNodeSet {
    fn meet(a: &Self, b: &Self) -> Self {
        match (a, b) {
            (IntersectNodeSet::Full, b) => b.clone(),
            (a, IntersectNodeSet::Full) => a.clone(),
            (IntersectNodeSet::Bits(a), IntersectNodeSet::Bits(b)) => {
                assert!(
                    a.len() == b.len(),
                    "IntersectNodeSets must have same length to meet."
                );
                IntersectNodeSet::Bits(a.clone() & b)
            }
            _ => IntersectNodeSet::Empty,
        }
    }

    fn bottom() -> Self {
        // For intersecting flow functions, the bottom state is empty.
        IntersectNodeSet::Empty
    }

    fn top() -> Self {
        // For intersecting flow functions, the top state is full.
        IntersectNodeSet::Full
    }
}

/*
 * A bit vector set is a very general kind of semilattice. This variant is for
 * "unioning" flow functions.
 */
#[derive(PartialEq, Eq, Clone, Debug)]
pub enum UnionNodeSet {
    Empty,
    Bits(BitVec<u8, Lsb0>),
    Full,
}

impl UnionNodeSet {
    pub fn is_set(&self, id: NodeID) -> bool {
        match self {
            UnionNodeSet::Empty => false,
            UnionNodeSet::Bits(bits) => bits[id.idx()],
            UnionNodeSet::Full => true,
        }
    }
}

impl Semilattice for UnionNodeSet {
    fn meet(a: &Self, b: &Self) -> Self {
        match (a, b) {
            (UnionNodeSet::Empty, b) => b.clone(),
            (a, UnionNodeSet::Empty) => a.clone(),
            (UnionNodeSet::Bits(a), UnionNodeSet::Bits(b)) => {
                assert!(
                    a.len() == b.len(),
                    "UnionNodeSets must have same length to meet."
                );
                UnionNodeSet::Bits(a.clone() | b)
            }
            _ => UnionNodeSet::Full,
        }
    }

    fn bottom() -> Self {
        // For unioning flow functions, the bottom state is full.
        UnionNodeSet::Full
    }

    fn top() -> Self {
        // For unioning flow functions, the top state is empty.
        UnionNodeSet::Empty
    }
}

/*
 * Below are some common flow functions. They all take a slice of semilattice
 * references as their first argument, and a node ID as their second. However,
 * they may in addition take more arguments (meaning that these functions
 * should be used inside closures at a callsite of a top level dataflow
 * function).
 */

/*
 * Flow function for collecting all of a node's uses of "control outputs". What
 * this flow function does is collect all immediate phi, thread ID, and collect
 * nodes that every other node depends on through data nodes. Flow is ended at
 * a control node, or at a phi, thread ID, or collect node.
 */
pub fn control_output_flow(
    inputs: &[&UnionNodeSet],
    node_id: NodeID,
    function: &Function,
) -> UnionNodeSet {
    // Step 1: union inputs.
    let mut out = UnionNodeSet::top();
    for input in inputs {
        out = UnionNodeSet::meet(&out, input);
    }
    let node = &function.nodes[node_id.idx()];

    // Step 2: clear all bits, if applicable.
    if node.is_strictly_control() || node.is_thread_id() || node.is_collect() || node.is_phi() {
        out = UnionNodeSet::Empty;
    }

    // Step 3: set bit for current node, if applicable.
    if node.is_thread_id() || node.is_collect() || node.is_phi() {
        let mut singular = bitvec![u8, Lsb0; 0; function.nodes.len()];
        singular.set(node_id.idx(), true);
        out = UnionNodeSet::meet(&out, &UnionNodeSet::Bits(singular));
    }

    out
}

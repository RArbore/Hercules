extern crate bitvec;

use self::bitvec::prelude::*;
use self::bitvec::slice::*;

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
    dataflow_global(function, reverse_postorder, |global_outs, node_id| {
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
 * Top level backward dataflow function. Instead of passing the uses' lattice
 * values to the flow function, passes in the users' lattice values.
 */
pub fn backward_dataflow<L, F>(
    function: &Function,
    def_use: &ImmutableDefUseMap,
    reverse_postorder: &Vec<NodeID>,
    mut flow_function: F,
) -> Vec<L>
where
    L: Semilattice,
    F: FnMut(&[&L], NodeID) -> L,
{
    let mut postorder = reverse_postorder.clone();
    postorder.reverse();
    dataflow_global(function, &postorder, |global_outs, node_id| {
        let users = def_use.get_users(node_id);
        let succ_outs: Vec<_> = users
            .as_ref()
            .iter()
            .map(|id| &global_outs[id.idx()])
            .collect();
        flow_function(&succ_outs, node_id)
    })
}

/*
 * The previous forward dataflow routines wraps around this dataflow routine,
 * where the flow function doesn't just have access to this nodes input lattice
 * values, but also all the current lattice values for all the nodes. This is
 * useful for some dataflow analyses, such as reachability. The "global" in
 * dataflow_global refers to having a global view of the out lattice values.
 */
pub fn dataflow_global<L, F>(
    function: &Function,
    order: &Vec<NodeID>,
    mut flow_function: F,
) -> Vec<L>
where
    L: Semilattice,
    F: FnMut(&[L], NodeID) -> L,
{
    // Step 1: create initial set of "out" points.
    let first_ins = vec![L::top(); function.nodes.len()];
    let mut outs: Vec<L> = (0..function.nodes.len())
        .map(|id| flow_function(&first_ins, NodeID::new(id)))
        .collect();

    // Step 2: peform main dataflow loop.
    loop {
        let mut change = false;

        // Iterate nodes in specified order.
        for node_id in order {
            // Compute new "out" value from previous "out" values.
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

    pub fn nodes(&self, num_nodes: u32) -> NodeSetIterator {
        match self {
            IntersectNodeSet::Empty => NodeSetIterator::Empty,
            IntersectNodeSet::Bits(bitvec) => {
                NodeSetIterator::Bits(bitvec.iter_ones().map(NodeID::new))
            }
            IntersectNodeSet::Full => NodeSetIterator::Full(0, num_nodes),
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

    pub fn nodes(&self, num_nodes: u32) -> NodeSetIterator {
        match self {
            UnionNodeSet::Empty => NodeSetIterator::Empty,
            UnionNodeSet::Bits(bitvec) => {
                NodeSetIterator::Bits(bitvec.iter_ones().map(NodeID::new))
            }
            UnionNodeSet::Full => NodeSetIterator::Full(0, num_nodes),
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

#[derive(Clone, Debug)]
pub enum NodeSetIterator<'a> {
    Empty,
    Bits(std::iter::Map<IterOnes<'a, u8, LocalBits>, fn(usize) -> ir::NodeID>),
    Full(u32, u32),
}

impl<'a> Iterator for NodeSetIterator<'a> {
    type Item = NodeID;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            NodeSetIterator::Empty => None,
            NodeSetIterator::Bits(iter) => iter.next(),
            NodeSetIterator::Full(idx, cap) => {
                if idx < cap {
                    let id = NodeID::new(*idx as usize);
                    *idx += 1;
                    Some(id)
                } else {
                    None
                }
            }
        }
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
    let mut out = inputs
        .into_iter()
        .fold(UnionNodeSet::top(), |a, b| UnionNodeSet::meet(&a, b));
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

/*
 * Flow function for collecting all of a data node's immediate uses / users of
 * control nodes. Useful for code generation. Since this is for immediate uses /
 * users of control nodes, control node uses / users do not propagate through
 * control nodes, or through control output nodes (phis, thread IDs, collects).
 */
pub fn immediate_control_flow(
    inputs: &[&UnionNodeSet],
    mut node_id: NodeID,
    function: &Function,
) -> UnionNodeSet {
    let mut out = UnionNodeSet::top();

    // Step 1: replace node if this is a phi, thread ID, or collect.
    if let Node::Phi { control, data: _ }
    | Node::ThreadID { control }
    | Node::Collect { control, data: _ } = &function.nodes[node_id.idx()]
    {
        node_id = *control;
    } else {
        // Union node inputs if not a special case.
        out = inputs
            .into_iter()
            .fold(UnionNodeSet::top(), |a, b| UnionNodeSet::meet(&a, b));
    }
    let node = &function.nodes[node_id.idx()];

    // Step 2: figure out if this node is a control node.
    let control = if let Node::ReadProd { prod, index: _ } = node {
        function.nodes[prod.idx()].is_strictly_control()
    } else {
        node.is_strictly_control()
    };

    // Step 3: clear all bits and set bit for current node, if applicable.
    if control {
        let mut singular = bitvec![u8, Lsb0; 0; function.nodes.len()];
        singular.set(node_id.idx(), true);
        out = UnionNodeSet::Bits(singular);
    }

    out
}

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
 * Top level dataflow function.
 */
pub fn dataflow<L, F, D>(function: &Function, flow_function: F, auxiliary_data: &D) -> Vec<L>
where
    L: Semilattice,
    F: Fn(L, &D, NodeID) -> L,
{
    // Step 1: create initial set of "in" points. The start node is initialized
    // to bottom, and everything else is initialized to top.
    let points: Vec<L> = (0..function.nodes.len())
        .map(|id| if id == 0 { L::bottom() } else { L::top() })
        .collect();

    todo!()
}

/*
 * Compute reverse post order of nodes in function.
 */
pub fn reverse_postorder(def_uses: &ImmutableDefUseMap) -> Vec<NodeID> {
    let order = vec![];
    let visited = bitvec![u8, Lsb0; 0; def_uses.num_nodes()];
    let (mut order, _) = reverse_postorder_helper(NodeID::new(0), def_uses, order, visited);
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
        (order, visited)
    } else {
        visited.set(node.idx(), true);
        for user in def_uses.get_users(node) {
            (order, visited) = reverse_postorder_helper(*user, def_uses, order, visited);
        }
        order.push(node);
        (order, visited)
    }
}

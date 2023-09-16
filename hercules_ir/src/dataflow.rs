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
pub fn dataflow<L, F, D>(function: Function, flow_function: F, auxiliary_data: &D) -> Vec<L>
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
pub fn reverse_postorder(function: Function) -> Vec<NodeID> {
    todo!()
}

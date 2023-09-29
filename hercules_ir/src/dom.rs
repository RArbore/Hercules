use crate::*;

/*
 * Custom type for storing a dominator tree. For each node except the start
 * node, store its immediate dominator.
 */
#[derive(Debug, Clone)]
pub struct DomTree {
    immediate_dominator: Vec<NodeID>,
}

/*
 * Top level function for calculating dominator trees.
 */
pub fn dominator(function: &Function) -> DomTree {
    todo!()
}

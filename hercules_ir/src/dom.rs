use crate::*;

use std::collections::HashMap;

/*
 * Custom type for storing a dominator tree. For each control node, store its
 * immediate dominator.
 */
#[derive(Debug, Clone)]
pub struct DomTree {
    imm_doms: HashMap<NodeID, NodeID>,
}

impl DomTree {
    pub fn imm_dom(&self, x: NodeID) -> Option<NodeID> {
        self.imm_doms.get(&x).map(|x| x.clone())
    }

    pub fn does_imm_dom(&self, a: NodeID, b: NodeID) -> bool {
        self.imm_dom(b) == Some(a)
    }

    pub fn does_dom(&self, a: NodeID, b: NodeID) -> bool {
        let mut iter = Some(b);
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
 * Top level function for calculating dominator trees.
 */
pub fn dominator(function: &Function) -> DomTree {
    todo!()
}

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
 * Top level function for calculating dominator trees.
 */
pub fn dominator(function: &Function) -> DomTree {
    // Step 1: compute the sub-CFG for the function. This is the graph the
    // dominator tree will be built for.
    let sub_cfg = control_nodes(function);

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

pub type SubCFG<'a> = Vec<(NodeID, ControlUses<'a>)>;

/*
 * Top level function for getting all the control nodes in a function. Also
 * returns the control uses of each control node, in effect returning the
 * control subset of the IR graph.
 */
pub fn control_nodes(function: &Function) -> SubCFG {
    use Node::*;

    let mut control_nodes = vec![];
    for (idx, node) in function.nodes.iter().enumerate() {
        match node {
            Start => {
                control_nodes.push((NodeID::new(idx), ControlUses::Zero));
            }
            Region { preds } => {
                control_nodes.push((NodeID::new(idx), ControlUses::Variable(&preds)));
            }
            If { control, cond: _ }
            | Fork { control, factor: _ }
            | Join { control, data: _ }
            | Return { control, value: _ }
            | Match { control, sum: _ } => {
                control_nodes.push((NodeID::new(idx), ControlUses::One([*control])));
            }
            ReadProd { prod, index } => match function.nodes[prod.idx()] {
                // ReadProd nodes are control nodes if their predecessor is a
                // legal control node, and if it's the right index.
                Match { control: _, sum: _ } => {
                    control_nodes.push((NodeID::new(idx), ControlUses::One([*prod])));
                }
                If {
                    control: _,
                    cond: _,
                }
                | Fork {
                    control: _,
                    factor: _,
                }
                | Join {
                    control: _,
                    data: _,
                } => {
                    if *index == 0 {
                        control_nodes.push((NodeID::new(idx), ControlUses::One([*prod])))
                    }
                }
                _ => {}
            },
            _ => {}
        }
    }
    control_nodes
}

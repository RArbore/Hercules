extern crate hercules_ir;

use std::collections::HashMap;
use std::iter::zip;

use self::hercules_ir::dataflow::*;
use self::hercules_ir::def_use::*;
use self::hercules_ir::ir::*;

use crate::ccp::ReachabilityLattice;

/* Copy Propagation
 * In the Hercules IR, the only copies that occur (ignoring expressions
 * involving identies, occurs through phi node.
 * We use a lattice that tracks 
 * 1. Reachability - is it possible for the node to be reached during some
 *    execution?
 * 2. Copy - is the node's value equal to that of some other node.
 *    This is considered to never be the case on non-phi nodes only defined on
 *    phi nodes.
 *    In the copy lattice, top indicates that the value is unknown and bottom
 *    indicates that the value is known to not always be that of another node
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CopyPropLattice {
    reachability: ReachabilityLattice,
    copy: CopyLattice,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CopyLattice {
    Top,
    Copy(NodeID),
    Bottom,
}

impl CopyPropLattice {
    fn is_reachable(&self) -> bool {
        self.reachability == ReachabilityLattice::Reachable
    }

    fn get_copy(&self) -> Option<NodeID> {
        if let CopyLattice::Copy(node) = &self.copy {
            Some(node.clone())
        } else {
            None
        }
    }
}

impl Semilattice for CopyPropLattice {
    fn meet(a: &Self, b: &Self) -> Self {
        CopyPropLattice {
            reachability: ReachabilityLattice::meet(&a.reachability, &b.reachability),
            copy: CopyLattice::meet(&a.copy, &b.copy),
        }
    }

    fn bottom() -> Self {
        CopyPropLattice {
            reachability: ReachabilityLattice::bottom(),
            copy: CopyLattice::bottom(),
        }
    }

    fn top() -> Self {
        CopyPropLattice {
            reachability: ReachabilityLattice::top(),
            copy : CopyLattice::top(),
        }
    }
}

impl Semilattice for CopyLattice {
    fn meet(a: &Self, b: &Self) -> Self {
        match (a, b) {
            (CopyLattice::Top, b) => b.clone(),
            (a, CopyLattice::Top) => a.clone(),
            (CopyLattice::Copy(node1), CopyLattice::Copy(node2)) => {
                if node1 == node2 {
                    CopyLattice::Copy(*node1)
                } else {
                    CopyLattice::Bottom
                }
            }
            _ => CopyLattice::Bottom,
        }
    }

    fn bottom() -> Self {
        CopyLattice::Bottom
    }

    fn top() -> Self {
        CopyLattice::Top
    }
}

/*
 * Top level function to run copy propagation
 */
pub fn copy_prop(
    function: &mut Function,
    //def_use: &ImmutableDefUseMap,
    reverse_postorder: &Vec<NodeID>,
) {
    // Step 1: run copy analysis to understand the function.
    let result = dataflow_global(&function, reverse_postorder, |inputs, node_id| {
        copy_prop_flow_function(inputs, node_id, &function)
    });

    let mut map = HashMap::new();

    // Step 2: replace references to copied phi nodes with the node the phi
    // copies and mark the phi as dead
    println!("Copy Propagations Results");
    for (node_idx, node) in function.nodes.iter_mut().enumerate() {
        if let Some(copy) = result[node_idx].get_copy() {
            println!("phi {} = node {}", node_idx, copy.idx());
            *node = Node::Start;
        } else {
            for u in get_uses_mut(node).as_mut() {
                let old_id = **u;
                if let Some(node) = result[old_id.idx()].get_copy()  {
                    map.insert(old_id.idx(), node.idx());
                    **u = node;
                }
            }
        }
    }
}

fn copy_prop_flow_function(
    inputs: &[CopyPropLattice],
    node_id: NodeID,
    function: &Function,
) -> CopyPropLattice {
    let node = &function.nodes[node_id.idx()];
    match node {
        Node::Start => CopyPropLattice::bottom(),
        // For a region, meet over the predecessors, what we really care about
        // here is just whether the region is reachable
        Node::Region { preds } => preds.iter().fold(CopyPropLattice::top(), |val, id| {
            CopyPropLattice::meet(&val, &inputs[id.idx()])
        }),
        Node::If { control, cond: _ } => inputs[control.idx()].clone(),
        Node::Match { control, sum: _ } => inputs[control.idx()].clone(),
        Node::Fork { control, factor: _ } => inputs[control.idx()].clone(),
        Node::Join { control } => inputs[control.idx()].clone(),
        // Phi nodes must look at the reachability of the inputs to its
        // corresponding region node to determine the value reaching it
        Node::Phi { control, data } => {
            // Get the control predecessors of the corresponding region.
            let region_preds = if let Node::Region { preds } = &function.nodes[control.idx()] {
                preds
            } else {
                panic!("A phi's control input must be a region node.")
            };
            println!("phi {}", node_id.idx());
            // We only consider data nodes whose corresponding control input is
            // reachable. 
            // We consider three cases based on whether the data node is a copy
            // 1. Top (i.e. we don't know), we continue to ignore the input
            //    since it may be a copy or not, we don't know
            // 2. Copy (i.e. the data input is itself a copy of another node)
            //    then we consider whether this phi is a copy of that same node
            // 3. Bottom (i.e. the data input is not a copy) then we consider
            //    whether this phi is a copy of the data input
            let val = zip(region_preds.iter(), data.iter()).fold(
                CopyPropLattice {
                    reachability: inputs[control.idx()].reachability.clone(),
                    copy: CopyLattice::top(),
                },
                |val, (control_id, data_id)| {
                    // If a control input to the region node is reachable, then
                    // and only then do we meet with the data input's constant
                    // lattice value. Note that the copy value is just the data
                    // node unless that node is itself a copy
                    if inputs[control_id.idx()].is_reachable() {
                        print!("- input {} is reachable: ", data_id.idx());
                        match inputs[data_id.idx()].copy {
                            CopyLattice::Top => println!("top"),
                            CopyLattice::Copy(node) => println!("copy of {}", node.idx()),
                            CopyLattice::Bottom => println!("bottom"),
                        }
                        CopyPropLattice::meet(&val, 
                            &CopyPropLattice {
                                reachability : inputs[data_id.idx()].reachability.clone(),
                                copy : 
                                    match inputs[data_id.idx()].copy {
                                        CopyLattice::Top => CopyLattice::Top,
                                        CopyLattice::Copy(node) => CopyLattice::Copy(node),
                                        CopyLattice::Bottom => CopyLattice::Copy(*data_id),
                                    }
                            })
                    } else {
                        val
                    }
                },
            );

            print!("phi {}: ", node_id.idx());
            match val.copy {
                CopyLattice::Top => println!("top"),
                CopyLattice::Copy(node) => println!("copy of {}", node.idx()),
                CopyLattice::Bottom => println!("bottom"),
            }

            val
        }
        Node::ThreadID { control } => inputs[control.idx()].clone(),
        Node::Reduce {
            control,
            init: _,
            reduct: _,
        } => inputs[control.idx()].clone(),
        Node::Return { control, data : _ } => CopyPropLattice {
            reachability: inputs[control.idx()].reachability.clone(),
            copy: CopyLattice::bottom(),
        },
        Node::Parameter { index: _ } => CopyPropLattice::bottom(),
        // A constant node is the "source" of concrete constant lattice values.
        Node::Constant { id : _ } => CopyPropLattice::bottom(),
        Node::DynamicConstant { id: _ } => CopyPropLattice::bottom(),
        Node::Unary { input, op : _ } => {
            CopyPropLattice {
                reachability : inputs[input.idx()].reachability.clone(),
                copy : CopyLattice::bottom(),
            }
        }
        Node::Binary { left, right, op : _ } => {
            CopyPropLattice {
                reachability : 
                    ReachabilityLattice::meet(
                        &inputs[left.idx()].reachability,
                        &inputs[right.idx()].reachability),
                copy : CopyLattice::bottom(),
            }
        }
        Node::Ternary {
            first,
            second,
            third,
            op : _,
        } => {
            CopyPropLattice {
                reachability : 
                    ReachabilityLattice::meet(
                        &ReachabilityLattice::meet(
                            &inputs[first.idx()].reachability,
                            &inputs[second.idx()].reachability),
                        &inputs[third.idx()].reachability),
                copy : CopyLattice::bottom(),
            }
        }
        // Call nodes are uninterpretable.
        Node::Call {
            function: _,
            dynamic_constants: _,
            args,
        } => CopyPropLattice {
            reachability: args.iter().fold(ReachabilityLattice::top(), |val, id| {
                ReachabilityLattice::meet(&val, &inputs[id.idx()].reachability)
            }),
            copy: CopyLattice::bottom(),
        },
        Node::Read { collect, indices : _ } =>
            CopyPropLattice {
                reachability : inputs[collect.idx()].reachability.clone(),
                copy : CopyLattice::bottom(),
            },
        Node::Write {
            collect,
            data,
            indices,
        } => {
            let mut reachability = ReachabilityLattice::meet(
                &inputs[collect.idx()].reachability,
                &inputs[data.idx()].reachability,
            );
            for index in indices.iter() {
                if let Index::Position(positions) = index {
                    for position in positions.iter() {
                        reachability = ReachabilityLattice::meet(
                            &reachability,
                            &inputs[position.idx()].reachability,
                        );
                    }
                }
            }
            CopyPropLattice {
                reachability,
                copy: CopyLattice::bottom(),
            }
        }
    }
}

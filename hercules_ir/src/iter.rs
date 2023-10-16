use std::collections::HashMap;
use std::iter::zip;

use crate::*;

/*
 * The iter lattice tracks, for each node, the following information:
 * 1. Reachability - is it possible for this node to be reached during any
 *    execution?
 * 2. Constant - does this node evaluate to a constant expression?
 * The iter lattice is formulated as a combination of consistuent lattices. The
 * flow function for the iter dataflow analysis "crosses" information across the
 * sub lattices - for example, whether a condition is constant may inform
 * whether a branch target is reachable. This analysis uses interpreted
 * constants, so constant one plus constant one results in constant two.
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IterLattice {
    reachability: ReachabilityLattice,
    constant: ConstantLattice,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ReachabilityLattice {
    Unreachable,
    Reachable,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantLattice {
    Top,
    Constant(Constant),
    Bottom,
}

impl IterLattice {
    fn is_reachable(&self) -> bool {
        self.reachability == ReachabilityLattice::Reachable
    }
}

impl Semilattice for IterLattice {
    fn meet(a: &Self, b: &Self) -> Self {
        IterLattice {
            reachability: ReachabilityLattice::meet(&a.reachability, &b.reachability),
            constant: ConstantLattice::meet(&a.constant, &b.constant),
        }
    }

    fn bottom() -> Self {
        IterLattice {
            reachability: ReachabilityLattice::bottom(),
            constant: ConstantLattice::bottom(),
        }
    }

    fn top() -> Self {
        IterLattice {
            reachability: ReachabilityLattice::top(),
            constant: ConstantLattice::top(),
        }
    }
}

impl Semilattice for ReachabilityLattice {
    fn meet(a: &Self, b: &Self) -> Self {
        match (a, b) {
            (ReachabilityLattice::Unreachable, ReachabilityLattice::Unreachable) => {
                ReachabilityLattice::Unreachable
            }
            _ => ReachabilityLattice::Reachable,
        }
    }

    fn bottom() -> Self {
        ReachabilityLattice::Reachable
    }

    fn top() -> Self {
        ReachabilityLattice::Unreachable
    }
}

impl Semilattice for ConstantLattice {
    fn meet(a: &Self, b: &Self) -> Self {
        match (a, b) {
            (ConstantLattice::Top, b) => b.clone(),
            (a, ConstantLattice::Top) => a.clone(),
            (ConstantLattice::Constant(cons1), ConstantLattice::Constant(cons2)) => {
                if cons1 == cons2 {
                    ConstantLattice::Constant(cons1.clone())
                } else {
                    ConstantLattice::Bottom
                }
            }
            _ => ConstantLattice::Bottom,
        }
    }

    fn bottom() -> Self {
        ConstantLattice::Bottom
    }

    fn top() -> Self {
        ConstantLattice::Top
    }
}

/*
 * Top level function to run "iter" optimization. Named after the "iter"
 * optimization from the OpenJDK HotSpot compiler. Runs constant propgataion,
 * unreachable code elimination, and global value numbering, at once. Needs to
 * take ownership of constants vector from function's module (or at least a copy
 * of it) since this pass may create new constants. Might make sense to Arc +
 * Mutex the constants vector if multithreading is ever considered.
 */
pub fn iter(
    function: Function,
    constants: Vec<Constant>,
    reverse_postorder: &Vec<NodeID>,
) -> (Function, Vec<Constant>) {
    // Step 1: collect constants into a map from constant values to IDs.
    let reverse_constants = constants
        .into_iter()
        .enumerate()
        .map(|(idx, cons)| (cons, ConstantID::new(idx)))
        .collect();

    // Step 2: run iter analysis to understand the function.
    let (result, reverse_constants) =
        iter_analysis(&function, reverse_constants, reverse_postorder);
    println!("{:?}", result);

    // Step 3: re-create constants vector for module.
    let mut constants = vec![Constant::Boolean(false); reverse_constants.len()];
    for (cons, id) in reverse_constants {
        constants[id.idx()] = cons;
    }

    (function, constants)
}

fn iter_analysis(
    function: &Function,
    mut reverse_constants: HashMap<Constant, ConstantID>,
    reverse_postorder: &Vec<NodeID>,
) -> (Vec<IterLattice>, HashMap<Constant, ConstantID>) {
    let result = forward_dataflow_global(function, reverse_postorder, |inputs, node_id| {
        iter_flow_function(inputs, node_id, function, &mut reverse_constants)
    });
    (result, reverse_constants)
}

fn iter_flow_function(
    inputs: &[IterLattice],
    node_id: NodeID,
    function: &Function,
    reverse_constants: &mut HashMap<Constant, ConstantID>,
) -> IterLattice {
    let node = &function.nodes[node_id.idx()];
    match node {
        Node::Start => IterLattice::bottom(),
        Node::Region { preds } => preds.iter().fold(IterLattice::top(), |val, id| {
            IterLattice::meet(&val, &inputs[id.idx()])
        }),
        // If node has only one output, so doesn't directly handle crossover of
        // reachability and constant propagation. ReadProd handles that.
        Node::If { control, cond: _ } => inputs[control.idx()].clone(),
        Node::Fork { control, factor: _ } => inputs[control.idx()].clone(),
        Node::Join { control } => inputs[control.idx()].clone(),
        // Phi nodes must look at the reachability of the inputs to its
        // corresponding region node to determine the constant value being
        // output.
        Node::Phi { control, data } => {
            // Get the control predecessors of the corresponding region.
            let region_preds = if let Node::Region { preds } = &function.nodes[control.idx()] {
                preds
            } else {
                panic!("A phi's control input must be a region node.")
            };
            zip(region_preds.iter(), data.iter()).fold(
                IterLattice {
                    reachability: inputs[control.idx()].reachability.clone(),
                    constant: ConstantLattice::top(),
                },
                |val, (control_id, data_id)| {
                    // If a control input to the region node is reachable, then
                    // and only then do we meet with the data input's constant
                    // lattice value.
                    if inputs[control_id.idx()].is_reachable() {
                        IterLattice::meet(&val, &inputs[data_id.idx()])
                    } else {
                        val
                    }
                },
            )
        }
        // Technically, if the dynamic constant of the corresponding fork is
        // constant one, then a ThreadID node holds constant value zero.
        // However, dynamic constants don't interact with node level
        // analysis, so we will already know that a fork is useless early on
        // and can remove the fork and join before we ever get here. Thus,
        // it is not a useful case to program.
        Node::ThreadID { control } => inputs[control.idx()].clone(),
        // At least for now, collect nodes always produce unknown values. It may
        // be worthwile to add interpretation of constants for collect nodes,
        // but it would involve plumbing dynamic constant and fork join pairing
        // information here, and I don't feel like doing that.
        Node::Collect { control, data: _ } => inputs[control.idx()].clone(),
        Node::Return { control, data: _ } => inputs[control.idx()].clone(),
        _ => IterLattice::bottom(),
    }
}

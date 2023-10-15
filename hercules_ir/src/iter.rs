use std::collections::HashMap;

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
    let result = forward_dataflow(function, reverse_postorder, |inputs, node_id| {
        iter_flow_function(inputs, node_id, function, &mut reverse_constants)
    });
    (result, reverse_constants)
}

fn iter_flow_function(
    inputs: &[&IterLattice],
    node_id: NodeID,
    function: &Function,
    reverse_constants: &mut HashMap<Constant, ConstantID>,
) -> IterLattice {
    todo!()
}

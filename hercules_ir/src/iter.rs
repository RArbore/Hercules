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
    Constant(ConstantID),
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
            (ConstantLattice::Constant(id1), ConstantLattice::Constant(id2)) => {
                if id1 == id2 {
                    ConstantLattice::Constant(*id1)
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
 * unreachable code elimination, and global value numbering, at once.
 */
pub fn iter(function: Function) -> Function {
    function
}

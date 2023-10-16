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

impl ConstantLattice {
    fn is_top(&self) -> bool {
        *self == ConstantLattice::Top
    }

    fn is_bottom(&self) -> bool {
        *self == ConstantLattice::Bottom
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
    // Step 1: run iter analysis to understand the function.
    let result = forward_dataflow_global(&function, reverse_postorder, |inputs, node_id| {
        iter_flow_function(inputs, node_id, &function, &constants)
    });
    println!("{:?}", result);

    (function, constants)
}

fn iter_flow_function(
    inputs: &[IterLattice],
    node_id: NodeID,
    function: &Function,
    old_constants: &Vec<Constant>,
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
        // TODO: This should produce a constant zero if the dynamic constant for
        // for the corresponding fork is one.
        Node::ThreadID { control } => inputs[control.idx()].clone(),
        // TODO: At least for now, collect nodes always produce unknown values.
        // It may be worthwile to add interpretation of constants for collect
        // nodes, but it would involve plumbing dynamic constant and fork join
        // pairing information here, and I don't feel like doing that.
        Node::Collect { control, data: _ } => inputs[control.idx()].clone(),
        Node::Return { control, data: _ } => inputs[control.idx()].clone(),
        Node::Parameter { index: _ } => IterLattice::bottom(),
        // A constant node is the "source" of concrete constant lattice values.
        Node::Constant { id } => IterLattice {
            reachability: ReachabilityLattice::bottom(),
            constant: ConstantLattice::Constant(old_constants[id.idx()].clone()),
        },
        // TODO: This should really be constant interpreted, since dynamic
        // constants as values are used frequently.
        Node::DynamicConstant { id: _ } => IterLattice::bottom(),
        // Interpret unary op on constant. TODO: avoid UB.
        Node::Unary { input, op } => {
            let IterLattice {
                ref reachability,
                ref constant,
            } = inputs[input.idx()];

            let new_constant = if let ConstantLattice::Constant(cons) = constant {
                let new_cons = match (op, cons) {
                    (UnaryOperator::Not, Constant::Boolean(val)) => Constant::Boolean(!val),
                    (UnaryOperator::Neg, Constant::Integer8(val)) => Constant::Integer8(-val),
                    (UnaryOperator::Neg, Constant::Integer16(val)) => Constant::Integer16(-val),
                    (UnaryOperator::Neg, Constant::Integer32(val)) => Constant::Integer32(-val),
                    (UnaryOperator::Neg, Constant::Integer64(val)) => Constant::Integer64(-val),
                    (UnaryOperator::Neg, Constant::Float32(val)) => Constant::Float32(-val),
                    (UnaryOperator::Neg, Constant::Float64(val)) => Constant::Float64(-val),
                    (UnaryOperator::Bitflip, Constant::Integer8(val)) => Constant::Integer8(!val),
                    (UnaryOperator::Bitflip, Constant::Integer16(val)) => Constant::Integer16(!val),
                    (UnaryOperator::Bitflip, Constant::Integer32(val)) => Constant::Integer32(!val),
                    (UnaryOperator::Bitflip, Constant::Integer64(val)) => Constant::Integer64(!val),
                    _ => panic!("Unsupported combination of unary operation and constant value. Did typechecking succeed?")
                };
                ConstantLattice::Constant(new_cons)
            } else {
                constant.clone()
            };

            IterLattice {
                reachability: reachability.clone(),
                constant: new_constant,
            }
        }
        // Interpret binary op on constants. TODO: avoid UB.
        Node::Binary { left, right, op } => {
            let IterLattice {
                reachability: ref left_reachability,
                constant: ref left_constant,
            } = inputs[left.idx()];
            let IterLattice {
                reachability: ref right_reachability,
                constant: ref right_constant,
            } = inputs[right.idx()];

            let new_constant = if let (
                ConstantLattice::Constant(left_cons),
                ConstantLattice::Constant(right_cons),
            ) = (left_constant, right_constant)
            {
                let new_cons = match (op, left_cons, right_cons) {
                    (BinaryOperator::Add, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val + right_val),
                    (BinaryOperator::Add, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val + right_val),
                    (BinaryOperator::Add, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val + right_val),
                    (BinaryOperator::Add, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val + right_val),
                    (BinaryOperator::Add, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val + right_val),
                    (BinaryOperator::Add, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val + right_val),
                    (BinaryOperator::Add, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val + right_val),
                    (BinaryOperator::Add, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val + right_val),
                    (BinaryOperator::Add, Constant::Float32(left_val), Constant::Float32(right_val)) => Constant::Float32(*left_val + *right_val),
                    (BinaryOperator::Add, Constant::Float64(left_val), Constant::Float64(right_val)) => Constant::Float64(*left_val + *right_val),
                    (BinaryOperator::Sub, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val - right_val),
                    (BinaryOperator::Sub, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val - right_val),
                    (BinaryOperator::Sub, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val - right_val),
                    (BinaryOperator::Sub, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val - right_val),
                    (BinaryOperator::Sub, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val - right_val),
                    (BinaryOperator::Sub, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val - right_val),
                    (BinaryOperator::Sub, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val - right_val),
                    (BinaryOperator::Sub, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val - right_val),
                    (BinaryOperator::Sub, Constant::Float32(left_val), Constant::Float32(right_val)) => Constant::Float32(*left_val - *right_val),
                    (BinaryOperator::Sub, Constant::Float64(left_val), Constant::Float64(right_val)) => Constant::Float64(*left_val - *right_val),
                    (BinaryOperator::Mul, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val * right_val),
                    (BinaryOperator::Mul, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val * right_val),
                    (BinaryOperator::Mul, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val * right_val),
                    (BinaryOperator::Mul, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val * right_val),
                    (BinaryOperator::Mul, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val * right_val),
                    (BinaryOperator::Mul, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val * right_val),
                    (BinaryOperator::Mul, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val * right_val),
                    (BinaryOperator::Mul, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val * right_val),
                    (BinaryOperator::Mul, Constant::Float32(left_val), Constant::Float32(right_val)) => Constant::Float32(*left_val * *right_val),
                    (BinaryOperator::Mul, Constant::Float64(left_val), Constant::Float64(right_val)) => Constant::Float64(*left_val * *right_val),
                    (BinaryOperator::Div, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val / right_val),
                    (BinaryOperator::Div, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val / right_val),
                    (BinaryOperator::Div, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val / right_val),
                    (BinaryOperator::Div, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val / right_val),
                    (BinaryOperator::Div, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val / right_val),
                    (BinaryOperator::Div, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val / right_val),
                    (BinaryOperator::Div, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val / right_val),
                    (BinaryOperator::Div, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val / right_val),
                    (BinaryOperator::Div, Constant::Float32(left_val), Constant::Float32(right_val)) => Constant::Float32(*left_val / *right_val),
                    (BinaryOperator::Div, Constant::Float64(left_val), Constant::Float64(right_val)) => Constant::Float64(*left_val / *right_val),
                    (BinaryOperator::Rem, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val % right_val),
                    (BinaryOperator::Rem, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val % right_val),
                    (BinaryOperator::Rem, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val % right_val),
                    (BinaryOperator::Rem, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val % right_val),
                    (BinaryOperator::Rem, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val % right_val),
                    (BinaryOperator::Rem, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val % right_val),
                    (BinaryOperator::Rem, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val % right_val),
                    (BinaryOperator::Rem, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val % right_val),
                    (BinaryOperator::Rem, Constant::Float32(left_val), Constant::Float32(right_val)) => Constant::Float32(*left_val % *right_val),
                    (BinaryOperator::Rem, Constant::Float64(left_val), Constant::Float64(right_val)) => Constant::Float64(*left_val % *right_val),
                    (BinaryOperator::LT, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Boolean(left_val < right_val),
                    (BinaryOperator::LT, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Boolean(left_val < right_val),
                    (BinaryOperator::LT, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Boolean(left_val < right_val),
                    (BinaryOperator::LT, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Boolean(left_val < right_val),
                    (BinaryOperator::LT, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::Boolean(left_val < right_val),
                    (BinaryOperator::LT, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::Boolean(left_val < right_val),
                    (BinaryOperator::LT, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::Boolean(left_val < right_val),
                    (BinaryOperator::LT, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::Boolean(left_val < right_val),
                    (BinaryOperator::LT, Constant::Float32(left_val), Constant::Float32(right_val)) => Constant::Boolean(*left_val < *right_val),
                    (BinaryOperator::LT, Constant::Float64(left_val), Constant::Float64(right_val)) => Constant::Boolean(*left_val < *right_val),
                    (BinaryOperator::LTE, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Boolean(left_val <= right_val),
                    (BinaryOperator::LTE, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Boolean(left_val <= right_val),
                    (BinaryOperator::LTE, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Boolean(left_val <= right_val),
                    (BinaryOperator::LTE, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Boolean(left_val <= right_val),
                    (BinaryOperator::LTE, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::Boolean(left_val <= right_val),
                    (BinaryOperator::LTE, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::Boolean(left_val <= right_val),
                    (BinaryOperator::LTE, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::Boolean(left_val <= right_val),
                    (BinaryOperator::LTE, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::Boolean(left_val <= right_val),
                    (BinaryOperator::LTE, Constant::Float32(left_val), Constant::Float32(right_val)) => Constant::Boolean(*left_val <= *right_val),
                    (BinaryOperator::LTE, Constant::Float64(left_val), Constant::Float64(right_val)) => Constant::Boolean(*left_val <= *right_val),
                    (BinaryOperator::GT, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Boolean(left_val > right_val),
                    (BinaryOperator::GT, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Boolean(left_val > right_val),
                    (BinaryOperator::GT, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Boolean(left_val > right_val),
                    (BinaryOperator::GT, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Boolean(left_val > right_val),
                    (BinaryOperator::GT, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::Boolean(left_val > right_val),
                    (BinaryOperator::GT, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::Boolean(left_val > right_val),
                    (BinaryOperator::GT, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::Boolean(left_val > right_val),
                    (BinaryOperator::GT, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::Boolean(left_val > right_val),
                    (BinaryOperator::GT, Constant::Float32(left_val), Constant::Float32(right_val)) => Constant::Boolean(*left_val > *right_val),
                    (BinaryOperator::GT, Constant::Float64(left_val), Constant::Float64(right_val)) => Constant::Boolean(*left_val > *right_val),
                    (BinaryOperator::GTE, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Boolean(left_val >= right_val),
                    (BinaryOperator::GTE, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Boolean(left_val >= right_val),
                    (BinaryOperator::GTE, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Boolean(left_val >= right_val),
                    (BinaryOperator::GTE, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Boolean(left_val >= right_val),
                    (BinaryOperator::GTE, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::Boolean(left_val >= right_val),
                    (BinaryOperator::GTE, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::Boolean(left_val >= right_val),
                    (BinaryOperator::GTE, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::Boolean(left_val >= right_val),
                    (BinaryOperator::GTE, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::Boolean(left_val >= right_val),
                    (BinaryOperator::GTE, Constant::Float32(left_val), Constant::Float32(right_val)) => Constant::Boolean(*left_val >= *right_val),
                    (BinaryOperator::GTE, Constant::Float64(left_val), Constant::Float64(right_val)) => Constant::Boolean(*left_val >= *right_val),
                    // EQ and NE can be implemented more easily, since we don't
                    // need to unpack the constants.
                    (BinaryOperator::EQ, left_val, right_val) => Constant::Boolean(left_val == right_val),
                    (BinaryOperator::NE, left_val, right_val) => Constant::Boolean(left_val != right_val),
                    (BinaryOperator::Or, Constant::Boolean(left_val), Constant::Boolean(right_val)) => Constant::Boolean(*left_val || *right_val),
                    (BinaryOperator::Or, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val | right_val),
                    (BinaryOperator::Or, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val | right_val),
                    (BinaryOperator::Or, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val | right_val),
                    (BinaryOperator::Or, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val | right_val),
                    (BinaryOperator::Or, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val | right_val),
                    (BinaryOperator::Or, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val | right_val),
                    (BinaryOperator::Or, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val | right_val),
                    (BinaryOperator::Or, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val | right_val),
                    (BinaryOperator::And, Constant::Boolean(left_val), Constant::Boolean(right_val)) => Constant::Boolean(*left_val && *right_val),
                    (BinaryOperator::And, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val & right_val),
                    (BinaryOperator::And, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val & right_val),
                    (BinaryOperator::And, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val & right_val),
                    (BinaryOperator::And, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val & right_val),
                    (BinaryOperator::And, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val & right_val),
                    (BinaryOperator::And, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val & right_val),
                    (BinaryOperator::And, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val & right_val),
                    (BinaryOperator::And, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val & right_val),
                    (BinaryOperator::Xor, Constant::Boolean(left_val), Constant::Boolean(right_val)) => Constant::Boolean(*left_val ^ *right_val),
                    (BinaryOperator::Xor, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val ^ right_val),
                    (BinaryOperator::Xor, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val ^ right_val),
                    (BinaryOperator::Xor, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val ^ right_val),
                    (BinaryOperator::Xor, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val ^ right_val),
                    (BinaryOperator::Xor, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val ^ right_val),
                    (BinaryOperator::Xor, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val ^ right_val),
                    (BinaryOperator::Xor, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val ^ right_val),
                    (BinaryOperator::Xor, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val ^ right_val),
                    (BinaryOperator::LSh, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val << right_val),
                    (BinaryOperator::LSh, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val << right_val),
                    (BinaryOperator::LSh, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val << right_val),
                    (BinaryOperator::LSh, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val << right_val),
                    (BinaryOperator::LSh, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val << right_val),
                    (BinaryOperator::LSh, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val << right_val),
                    (BinaryOperator::LSh, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val << right_val),
                    (BinaryOperator::LSh, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val << right_val),
                    (BinaryOperator::RSh, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val >> right_val),
                    (BinaryOperator::RSh, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val >> right_val),
                    (BinaryOperator::RSh, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val >> right_val),
                    (BinaryOperator::RSh, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val >> right_val),
                    (BinaryOperator::RSh, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val >> right_val),
                    (BinaryOperator::RSh, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val >> right_val),
                    (BinaryOperator::RSh, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val >> right_val),
                    (BinaryOperator::RSh, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val >> right_val),
                    _ => panic!("Unsupported combination of binary operation and constant values. Did typechecking succeed?")
                };
                ConstantLattice::Constant(new_cons)
            } else if left_constant.is_top() || right_constant.is_top() {
                ConstantLattice::top()
            } else {
                ConstantLattice::bottom()
            };

            IterLattice {
                reachability: ReachabilityLattice::meet(left_reachability, right_reachability),
                constant: new_constant,
            }
        }
        _ => IterLattice::bottom(),
    }
}

use std::collections::HashMap;
use std::iter::zip;

use crate::*;

/*
 * The ccp lattice tracks, for each node, the following information:
 * 1. Reachability - is it possible for this node to be reached during any
 *    execution?
 * 2. Constant - does this node evaluate to a constant expression?
 * The ccp lattice is formulated as a combination of consistuent lattices. The
 * flow function for the ccp dataflow analysis "crosses" information across the
 * sub lattices - for example, whether a condition is constant may inform
 * whether a branch target is reachable. This analysis uses interpreted
 * constants, so constant one plus constant one results in constant two.
 */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CCPLattice {
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

impl CCPLattice {
    fn is_reachable(&self) -> bool {
        self.reachability == ReachabilityLattice::Reachable
    }

    fn get_constant(&self) -> Option<Constant> {
        if let ConstantLattice::Constant(cons) = &self.constant {
            Some(cons.clone())
        } else {
            None
        }
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

impl Semilattice for CCPLattice {
    fn meet(a: &Self, b: &Self) -> Self {
        CCPLattice {
            reachability: ReachabilityLattice::meet(&a.reachability, &b.reachability),
            constant: ConstantLattice::meet(&a.constant, &b.constant),
        }
    }

    fn bottom() -> Self {
        CCPLattice {
            reachability: ReachabilityLattice::bottom(),
            constant: ConstantLattice::bottom(),
        }
    }

    fn top() -> Self {
        CCPLattice {
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
 * Top level function to run conditional constant propagation. Needs to take
 * ownership of constants vector from function's module since this pass may
 * create new constants. Might make sense to Arc + Mutex the constants vector
 * if multithreading is ever considered.
 */
pub fn ccp(
    function: &mut Function,
    constants: &mut Vec<Constant>,
    reverse_postorder: &Vec<NodeID>,
) {
    // Step 1: run ccp analysis to understand the function.
    let result = forward_dataflow_global(&function, reverse_postorder, |inputs, node_id| {
        ccp_flow_function(inputs, node_id, &function, &constants)
    });

    // Step 2: update uses of constants. Any node that doesn't produce a
    // constant value, but does use a newly found constant value, needs to be
    // updated to use the newly found constant.

    // Step 2.1: assemble reverse constant map. We created a bunch of constants
    // during the analysis, so we need to intern them.
    let mut reverse_constant_map: HashMap<Constant, ConstantID> = constants
        .iter()
        .enumerate()
        .map(|(idx, cons)| (cons.clone(), ConstantID::new(idx)))
        .collect();

    // Helper function for interning constants in the lattice.
    let mut get_constant_id = |cons| {
        if let Some(id) = reverse_constant_map.get(&cons) {
            *id
        } else {
            let id = ConstantID::new(reverse_constant_map.len());
            reverse_constant_map.insert(cons.clone(), id);
            id
        }
    };

    // Step 2.2: for every node, update uses of now constant nodes. We need to
    // separately create constant nodes, since we are mutably looping over the
    // function nodes separately.
    let mut new_constant_nodes = vec![];
    let base_cons_node_idx = function.nodes.len();
    for node in function.nodes.iter_mut() {
        for u in get_uses_mut(node).as_mut() {
            let old_id = **u;
            if let Some(cons) = result[old_id.idx()].get_constant() {
                // Get ConstantID for this constant.
                let cons_id = get_constant_id(cons);

                // Search new_constant_nodes for a constant IR node that already
                // referenced this ConstantID.
                if let Some(new_nodes_idx) = new_constant_nodes
                    .iter()
                    .enumerate()
                    .filter(|(_, id)| **id == cons_id)
                    .map(|(idx, _)| idx)
                    .next()
                {
                    // If there is already a constant IR node, calculate what
                    // the NodeID will be for it, and set the use to that ID.
                    **u = NodeID::new(base_cons_node_idx + new_nodes_idx);
                } else {
                    // If there is not already a constant IR node for this
                    // ConstantID, add this ConstantID to the new_constant_nodes
                    // list. Set the use to the corresponding NodeID for the new
                    // constant IR node.
                    let cons_node_id = NodeID::new(base_cons_node_idx + new_constant_nodes.len());
                    new_constant_nodes.push(cons_id);
                    **u = cons_node_id;
                }
            }
        }
    }

    // Step 2.3: add new constant nodes into nodes of function.
    for node in new_constant_nodes {
        function.nodes.push(Node::Constant { id: node });
    }

    // Step 2.4: re-create module's constants vector from interning map.
    *constants = vec![Constant::Boolean(false); reverse_constant_map.len()];
    for (cons, id) in reverse_constant_map {
        constants[id.idx()] = cons;
    }
}

fn ccp_flow_function(
    inputs: &[CCPLattice],
    node_id: NodeID,
    function: &Function,
    old_constants: &Vec<Constant>,
) -> CCPLattice {
    let node = &function.nodes[node_id.idx()];
    match node {
        Node::Start => CCPLattice::bottom(),
        Node::Region { preds } => preds.iter().fold(CCPLattice::top(), |val, id| {
            CCPLattice::meet(&val, &inputs[id.idx()])
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
                CCPLattice {
                    reachability: inputs[control.idx()].reachability.clone(),
                    constant: ConstantLattice::top(),
                },
                |val, (control_id, data_id)| {
                    // If a control input to the region node is reachable, then
                    // and only then do we meet with the data input's constant
                    // lattice value.
                    if inputs[control_id.idx()].is_reachable() {
                        CCPLattice::meet(&val, &inputs[data_id.idx()])
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
        Node::Return { control, data } => CCPLattice {
            reachability: inputs[control.idx()].reachability.clone(),
            constant: inputs[data.idx()].constant.clone(),
        },
        Node::Parameter { index: _ } => CCPLattice::bottom(),
        // A constant node is the "source" of concrete constant lattice values.
        Node::Constant { id } => CCPLattice {
            reachability: ReachabilityLattice::bottom(),
            constant: ConstantLattice::Constant(old_constants[id.idx()].clone()),
        },
        // TODO: This should really be constant interpreted, since dynamic
        // constants as values are used frequently.
        Node::DynamicConstant { id: _ } => CCPLattice::bottom(),
        // Interpret unary op on constant. TODO: avoid UB.
        Node::Unary { input, op } => {
            let CCPLattice {
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

            CCPLattice {
                reachability: reachability.clone(),
                constant: new_constant,
            }
        }
        // Interpret binary op on constants. TODO: avoid UB.
        Node::Binary { left, right, op } => {
            let CCPLattice {
                reachability: ref left_reachability,
                constant: ref left_constant,
            } = inputs[left.idx()];
            let CCPLattice {
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
            } else if (left_constant.is_top() && !right_constant.is_bottom())
                || (!left_constant.is_bottom() && right_constant.is_top())
            {
                ConstantLattice::top()
            } else {
                ConstantLattice::meet(left_constant, right_constant)
            };

            CCPLattice {
                reachability: ReachabilityLattice::meet(left_reachability, right_reachability),
                constant: new_constant,
            }
        }
        // Call nodes are uninterpretable.
        Node::Call {
            function: _,
            dynamic_constants: _,
            args,
        } => CCPLattice {
            reachability: args.iter().fold(ReachabilityLattice::top(), |val, id| {
                ReachabilityLattice::meet(&val, &inputs[id.idx()].reachability)
            }),
            constant: ConstantLattice::bottom(),
        },
        // ReadProd handles reachability when following an if or match.
        Node::ReadProd { prod, index } => match &function.nodes[prod.idx()] {
            Node::If { control: _, cond } => {
                let cond_constant = &inputs[cond.idx()].constant;
                let if_reachability = &inputs[prod.idx()].reachability;
                let if_constant = &inputs[prod.idx()].constant;

                let new_reachability = if cond_constant.is_top() {
                    ReachabilityLattice::top()
                } else if let ConstantLattice::Constant(cons) = cond_constant {
                    if let Constant::Boolean(val) = cons {
                        if *val && *index == 0 {
                            // If condition is true and this is the false
                            // branch, then unreachable.
                            ReachabilityLattice::top()
                        } else if !val && *index == 1 {
                            // If condition is true and this is the true branch,
                            // then unreachable.
                            ReachabilityLattice::top()
                        } else {
                            if_reachability.clone()
                        }
                    } else {
                        panic!("Attempted to interpret ReadProd node, where corresponding if node has a non-boolean constant input. Did typechecking succeed?")
                    }
                } else {
                    if_reachability.clone()
                };

                CCPLattice {
                    reachability: new_reachability,
                    constant: if_constant.clone(),
                }
            }
            Node::Match { control: _, sum } => {
                let sum_constant = &inputs[sum.idx()].constant;
                let if_reachability = &inputs[prod.idx()].reachability;
                let if_constant = &inputs[prod.idx()].constant;

                let new_reachability = if sum_constant.is_top() {
                    ReachabilityLattice::top()
                } else if let ConstantLattice::Constant(cons) = sum_constant {
                    if let Constant::Summation(_, variant, _) = cons {
                        if *variant as usize != *index {
                            // If match variant is not the same as this branch,
                            // then unreachable.
                            ReachabilityLattice::top()
                        } else {
                            if_reachability.clone()
                        }
                    } else {
                        panic!("Attempted to interpret ReadProd node, where corresponding match node has a non-summation constant input. Did typechecking succeed?")
                    }
                } else {
                    if_reachability.clone()
                };

                CCPLattice {
                    reachability: new_reachability,
                    constant: if_constant.clone(),
                }
            }
            _ => {
                let CCPLattice {
                    ref reachability,
                    ref constant,
                } = inputs[prod.idx()];

                let new_constant = if let ConstantLattice::Constant(cons) = constant {
                    let new_cons = if let Constant::Product(_, fields) = cons {
                        // Index into product constant to get result constant.
                        old_constants[fields[*index].idx()].clone()
                    } else {
                        panic!("Attempted to interpret ReadProd on non-product constant. Did typechecking succeed?")
                    };
                    ConstantLattice::Constant(new_cons)
                } else {
                    constant.clone()
                };

                CCPLattice {
                    reachability: reachability.clone(),
                    constant: new_constant,
                }
            }
        },
        // WriteProd is uninterpreted for now.
        Node::WriteProd {
            prod,
            data,
            index: _,
        } => CCPLattice {
            reachability: ReachabilityLattice::meet(
                &inputs[prod.idx()].reachability,
                &inputs[data.idx()].reachability,
            ),
            constant: ConstantLattice::bottom(),
        },
        Node::ReadArray { array, index } => {
            let CCPLattice {
                reachability: ref array_reachability,
                constant: ref array_constant,
            } = inputs[array.idx()];
            let CCPLattice {
                reachability: ref index_reachability,
                constant: ref index_constant,
            } = inputs[index.idx()];

            let new_constant = if let (
                ConstantLattice::Constant(array_cons),
                ConstantLattice::Constant(index_cons),
            ) = (array_constant, index_constant)
            {
                let new_cons = match (array_cons, index_cons) {
                    (Constant::Array(_, elems), Constant::UnsignedInteger8(idx)) => {
                        elems[*idx as usize]
                    }
                    (Constant::Array(_, elems), Constant::UnsignedInteger16(idx)) => {
                        elems[*idx as usize]
                    }
                    (Constant::Array(_, elems), Constant::UnsignedInteger32(idx)) => {
                        elems[*idx as usize]
                    }
                    (Constant::Array(_, elems), Constant::UnsignedInteger64(idx)) => {
                        elems[*idx as usize]
                    }
                    _ => panic!("Unsupported inputs to ReadArray node. Did typechecking succeed?"),
                };
                ConstantLattice::Constant(old_constants[new_cons.idx()].clone())
            } else if (array_constant.is_top() && !index_constant.is_bottom())
                || (!array_constant.is_bottom() && index_constant.is_top())
            {
                ConstantLattice::top()
            } else {
                ConstantLattice::meet(array_constant, index_constant)
            };

            CCPLattice {
                reachability: ReachabilityLattice::meet(array_reachability, index_reachability),
                constant: new_constant,
            }
        }
        // WriteArray is uninterpreted for now.
        Node::WriteArray { array, data, index } => CCPLattice {
            reachability: ReachabilityLattice::meet(
                &ReachabilityLattice::meet(
                    &inputs[array.idx()].reachability,
                    &inputs[data.idx()].reachability,
                ),
                &inputs[index.idx()].reachability,
            ),
            constant: ConstantLattice::bottom(),
        },
        Node::Match { control, sum: _ } => inputs[control.idx()].clone(),
        _ => CCPLattice::bottom(),
    }
}

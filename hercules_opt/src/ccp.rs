extern crate hercules_ir;

use std::collections::HashMap;
use std::iter::zip;

use self::hercules_ir::dataflow::*;
use self::hercules_ir::def_use::*;
use self::hercules_ir::ir::*;

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
 * Top level function to run conditional constant propagation.
 */
pub fn ccp(
    function: &mut Function,
    types: &Vec<Type>,
    constants: &mut Vec<Constant>,
    def_use: &ImmutableDefUseMap,
    reverse_postorder: &Vec<NodeID>,
) {
    // Step 1: run ccp analysis to understand the function.
    let result = dataflow_global(&function, reverse_postorder, |inputs, node_id| {
        ccp_flow_function(inputs, node_id, &function, &types, &constants)
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

    // Step 3: delete dead branches. Any nodes that are unreachable should be
    // deleted. Any if or match nodes that now are light on users need to be
    // removed immediately, since if and match nodes have requirements on the
    // number of users.

    // Step 3.1: delete unreachable nodes. Loop over the length of the dataflow
    // result instead of the function's node list, since in step 2, constant
    // nodes were added that don't have a corresponding lattice result.
    for idx in 0..result.len() {
        if !result[idx].is_reachable() {
            function.nodes[idx] = Node::Start;
        }
    }

    // Step 3.2: remove uses of data nodes in phi nodes corresponding to
    // unreachable uses in corresponding region nodes.
    for phi_id in (0..result.len()).map(NodeID::new) {
        if let Node::Phi { control, data } = &function.nodes[phi_id.idx()] {
            if let Node::Region { preds } = &function.nodes[control.idx()] {
                let new_data = zip(preds.iter(), data.iter())
                    .filter(|(pred, _)| result[pred.idx()].is_reachable())
                    .map(|(_, datum)| *datum)
                    .collect();
                function.nodes[phi_id.idx()] = Node::Phi {
                    control: *control,
                    data: new_data,
                };
            }
        }
    }

    // Step 3.3: remove uses of unreachable nodes in region nodes.
    for node in function.nodes.iter_mut() {
        if let Node::Region { preds } = node {
            *preds = preds
                .iter()
                .filter(|pred| result[pred.idx()].is_reachable())
                .map(|x| *x)
                .collect();
        }
    }

    // Step 3.4: remove if and match nodes with one reachable user.
    for branch_id in (0..result.len()).map(NodeID::new) {
        if let Node::If { control, cond: _ } | Node::Match { control, sum: _ } =
            function.nodes[branch_id.idx()].clone()
        {
            let users = def_use.get_users(branch_id);
            let mut reachable_users = users
                .iter()
                .filter(|user| result[user.idx()].is_reachable());
            let the_reachable_user = reachable_users
                .next()
                .expect("During CCP, found a branch with no reachable users.");

            // The reachable users iterator will contain one user if we need to
            // remove this branch node.
            if let None = reachable_users.next() {
                // The user is a Read node, which in turn has one user.
                assert!(
                    def_use.get_users(*the_reachable_user).len() == 1,
                    "Control Read node doesn't have exactly one user."
                );
                let target = def_use.get_users(*the_reachable_user)[0];

                // For each use in the target of the reachable Read, turn it
                // into a use of the node proceeding this branch node.
                for u in get_uses_mut(&mut function.nodes[target.idx()]).as_mut() {
                    if **u == *the_reachable_user {
                        **u = control;
                    }
                }

                // Remove this branch node, since it is malformed. Also remove
                // all successor Read nodes.
                function.nodes[branch_id.idx()] = Node::Start;
                for user in users {
                    function.nodes[user.idx()] = Node::Start;
                }
            }
        }
    }

    // Step 4: collapse region chains.
    collapse_region_chains(function, def_use);
}

/*
 * Top level function to collapse region chains. A chain is a list of at least
 * one region node that takes only one control input. Region chains can be
 * deleted. The use of the head of the chain can turn into the use by the user
 * of the tail of the chain.
 */
pub fn collapse_region_chains(function: &mut Function, def_use: &ImmutableDefUseMap) {
    // Loop over all region nodes. It's fine to modify the function as we loop
    // over it.
    for id in (0..function.nodes.len()).map(NodeID::new) {
        if let Node::Region { preds } = &function.nodes[id.idx()] {
            if preds.len() == 1 {
                // Step 1: bridge gap between use and user.
                let predecessor = preds[0];
                let successor = def_use
                    .get_users(id)
                    .iter()
                    .filter(|x| !function.nodes[x.idx()].is_phi())
                    .next()
                    .expect("Region node doesn't have a non-phi user.");

                // Set successor's use of this region to use the region's use.
                for u in get_uses_mut(&mut function.nodes[successor.idx()]).as_mut() {
                    if **u == id {
                        **u = predecessor;
                    }
                }

                // Delete this region.
                function.nodes[id.idx()] = Node::Start;

                // Step 2: bridge gap between uses and users of corresponding
                // phi nodes.
                let phis: Vec<NodeID> = def_use
                    .get_users(id)
                    .iter()
                    .map(|x| *x)
                    .filter(|x| function.nodes[x.idx()].is_phi())
                    .collect();
                for phi_id in phis {
                    let data_uses =
                        if let Node::Phi { control, data } = &function.nodes[phi_id.idx()] {
                            assert!(*control == id);
                            data
                        } else {
                            panic!()
                        };
                    assert!(data_uses.len() == 1, "Phi node doesn't have exactly one data use, while corresponding region had exactly one control use.");
                    let predecessor = data_uses[0];

                    // Set successors' use of this phi to use the phi's use.
                    for successor in def_use.get_users(phi_id) {
                        for u in get_uses_mut(&mut function.nodes[successor.idx()]).as_mut() {
                            if **u == phi_id {
                                **u = predecessor;
                            }
                        }
                    }

                    // Delete this phi.
                    function.nodes[phi_id.idx()] = Node::Start;
                }
            }
        }
    }
}

fn ccp_flow_function(
    inputs: &[CCPLattice],
    node_id: NodeID,
    function: &Function,
    types: &Vec<Type>,
    old_constants: &Vec<Constant>,
) -> CCPLattice {
    let node = &function.nodes[node_id.idx()];
    match node {
        Node::Start => CCPLattice::bottom(),
        Node::Region { preds } => preds.iter().fold(CCPLattice::top(), |val, id| {
            CCPLattice::meet(&val, &inputs[id.idx()])
        }),
        // If node has only one output, if doesn't directly handle crossover of
        // reachability and constant propagation. Read handles that.
        Node::If { control, cond: _ } => inputs[control.idx()].clone(),
        Node::Match { control, sum: _ } => inputs[control.idx()].clone(),
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
        // TODO: At least for now, reduce nodes always produce unknown values.
        Node::Reduce {
            control,
            init: _,
            reduct: _,
        } => inputs[control.idx()].clone(),
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
                    (UnaryOperator::Not, Constant::Integer8(val)) => Constant::Integer8(!val),
                    (UnaryOperator::Not, Constant::Integer16(val)) => Constant::Integer16(!val),
                    (UnaryOperator::Not, Constant::Integer32(val)) => Constant::Integer32(!val),
                    (UnaryOperator::Not, Constant::Integer64(val)) => Constant::Integer64(!val),
                    (UnaryOperator::Neg, Constant::Integer8(val)) => Constant::Integer8(-val),
                    (UnaryOperator::Neg, Constant::Integer16(val)) => Constant::Integer16(-val),
                    (UnaryOperator::Neg, Constant::Integer32(val)) => Constant::Integer32(-val),
                    (UnaryOperator::Neg, Constant::Integer64(val)) => Constant::Integer64(-val),
                    (UnaryOperator::Neg, Constant::Float32(val)) => Constant::Float32(-val),
                    (UnaryOperator::Neg, Constant::Float64(val)) => Constant::Float64(-val),
                    (UnaryOperator::Neg, Constant::Zero(id)) => Constant::Zero(*id),
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
                let type_to_zero_cons = |ty_id: TypeID| {
                    match types[ty_id.idx()] {
                    Type::Boolean => Constant::Boolean(false),
                    Type::Integer8 => Constant::Integer8(0),
                    Type::Integer16 => Constant::Integer16(0),
                    Type::Integer32 => Constant::Integer32(0),
                    Type::Integer64 => Constant::Integer64(0),
                    Type::UnsignedInteger8 => Constant::UnsignedInteger8(0),
                    Type::UnsignedInteger16 => Constant::UnsignedInteger16(0),
                    Type::UnsignedInteger32 => Constant::UnsignedInteger32(0),
                    Type::UnsignedInteger64 => Constant::UnsignedInteger64(0),
                    Type::Float32 => Constant::Float32(ordered_float::OrderedFloat::<f32>(0.0)),
                    Type::Float64 => Constant::Float64(ordered_float::OrderedFloat::<f64>(0.0)),
                    _ => panic!("Unsupported combination of binary operation and constant values. Did typechecking succeed?")
                }
                };
                let left_cons = if let Constant::Zero(id) = left_cons {
                    type_to_zero_cons(*id)
                } else {
                    left_cons.clone()
                };
                let right_cons = if let Constant::Zero(id) = right_cons {
                    type_to_zero_cons(*id)
                } else {
                    right_cons.clone()
                };
                let new_cons = match (op, left_cons, right_cons) {
                    (BinaryOperator::Add, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val + right_val),
                    (BinaryOperator::Add, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val + right_val),
                    (BinaryOperator::Add, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val + right_val),
                    (BinaryOperator::Add, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val + right_val),
                    (BinaryOperator::Add, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val + right_val),
                    (BinaryOperator::Add, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val + right_val),
                    (BinaryOperator::Add, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val + right_val),
                    (BinaryOperator::Add, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val + right_val),
                    (BinaryOperator::Add, Constant::Float32(left_val), Constant::Float32(right_val)) => Constant::Float32(left_val + right_val),
                    (BinaryOperator::Add, Constant::Float64(left_val), Constant::Float64(right_val)) => Constant::Float64(left_val + right_val),
                    (BinaryOperator::Sub, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val - right_val),
                    (BinaryOperator::Sub, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val - right_val),
                    (BinaryOperator::Sub, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val - right_val),
                    (BinaryOperator::Sub, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val - right_val),
                    (BinaryOperator::Sub, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val - right_val),
                    (BinaryOperator::Sub, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val - right_val),
                    (BinaryOperator::Sub, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val - right_val),
                    (BinaryOperator::Sub, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val - right_val),
                    (BinaryOperator::Sub, Constant::Float32(left_val), Constant::Float32(right_val)) => Constant::Float32(left_val - right_val),
                    (BinaryOperator::Sub, Constant::Float64(left_val), Constant::Float64(right_val)) => Constant::Float64(left_val - right_val),
                    (BinaryOperator::Mul, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val * right_val),
                    (BinaryOperator::Mul, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val * right_val),
                    (BinaryOperator::Mul, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val * right_val),
                    (BinaryOperator::Mul, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val * right_val),
                    (BinaryOperator::Mul, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val * right_val),
                    (BinaryOperator::Mul, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val * right_val),
                    (BinaryOperator::Mul, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val * right_val),
                    (BinaryOperator::Mul, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val * right_val),
                    (BinaryOperator::Mul, Constant::Float32(left_val), Constant::Float32(right_val)) => Constant::Float32(left_val * right_val),
                    (BinaryOperator::Mul, Constant::Float64(left_val), Constant::Float64(right_val)) => Constant::Float64(left_val * right_val),
                    (BinaryOperator::Div, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val / right_val),
                    (BinaryOperator::Div, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val / right_val),
                    (BinaryOperator::Div, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val / right_val),
                    (BinaryOperator::Div, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val / right_val),
                    (BinaryOperator::Div, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val / right_val),
                    (BinaryOperator::Div, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val / right_val),
                    (BinaryOperator::Div, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val / right_val),
                    (BinaryOperator::Div, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val / right_val),
                    (BinaryOperator::Div, Constant::Float32(left_val), Constant::Float32(right_val)) => Constant::Float32(left_val / right_val),
                    (BinaryOperator::Div, Constant::Float64(left_val), Constant::Float64(right_val)) => Constant::Float64(left_val / right_val),
                    (BinaryOperator::Rem, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val % right_val),
                    (BinaryOperator::Rem, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val % right_val),
                    (BinaryOperator::Rem, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val % right_val),
                    (BinaryOperator::Rem, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val % right_val),
                    (BinaryOperator::Rem, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val % right_val),
                    (BinaryOperator::Rem, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val % right_val),
                    (BinaryOperator::Rem, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val % right_val),
                    (BinaryOperator::Rem, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val % right_val),
                    (BinaryOperator::Rem, Constant::Float32(left_val), Constant::Float32(right_val)) => Constant::Float32(left_val % right_val),
                    (BinaryOperator::Rem, Constant::Float64(left_val), Constant::Float64(right_val)) => Constant::Float64(left_val % right_val),
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
                    (BinaryOperator::Or, Constant::Boolean(left_val), Constant::Boolean(right_val)) => Constant::Boolean(left_val || right_val),
                    (BinaryOperator::Or, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val | right_val),
                    (BinaryOperator::Or, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val | right_val),
                    (BinaryOperator::Or, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val | right_val),
                    (BinaryOperator::Or, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val | right_val),
                    (BinaryOperator::Or, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val | right_val),
                    (BinaryOperator::Or, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val | right_val),
                    (BinaryOperator::Or, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val | right_val),
                    (BinaryOperator::Or, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val | right_val),
                    (BinaryOperator::And, Constant::Boolean(left_val), Constant::Boolean(right_val)) => Constant::Boolean(left_val && right_val),
                    (BinaryOperator::And, Constant::Integer8(left_val), Constant::Integer8(right_val)) => Constant::Integer8(left_val & right_val),
                    (BinaryOperator::And, Constant::Integer16(left_val), Constant::Integer16(right_val)) => Constant::Integer16(left_val & right_val),
                    (BinaryOperator::And, Constant::Integer32(left_val), Constant::Integer32(right_val)) => Constant::Integer32(left_val & right_val),
                    (BinaryOperator::And, Constant::Integer64(left_val), Constant::Integer64(right_val)) => Constant::Integer64(left_val & right_val),
                    (BinaryOperator::And, Constant::UnsignedInteger8(left_val), Constant::UnsignedInteger8(right_val)) => Constant::UnsignedInteger8(left_val & right_val),
                    (BinaryOperator::And, Constant::UnsignedInteger16(left_val), Constant::UnsignedInteger16(right_val)) => Constant::UnsignedInteger16(left_val & right_val),
                    (BinaryOperator::And, Constant::UnsignedInteger32(left_val), Constant::UnsignedInteger32(right_val)) => Constant::UnsignedInteger32(left_val & right_val),
                    (BinaryOperator::And, Constant::UnsignedInteger64(left_val), Constant::UnsignedInteger64(right_val)) => Constant::UnsignedInteger64(left_val & right_val),
                    (BinaryOperator::Xor, Constant::Boolean(left_val), Constant::Boolean(right_val)) => Constant::Boolean(left_val ^ right_val),
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
        Node::Ternary {
            first,
            second,
            third,
            op,
        } => {
            let CCPLattice {
                reachability: ref first_reachability,
                constant: ref first_constant,
            } = inputs[first.idx()];
            let CCPLattice {
                reachability: ref second_reachability,
                constant: ref second_constant,
            } = inputs[second.idx()];
            let CCPLattice {
                reachability: ref third_reachability,
                constant: ref third_constant,
            } = inputs[third.idx()];

            let new_constant = if let (
                ConstantLattice::Constant(first_cons),
                ConstantLattice::Constant(second_cons),
                ConstantLattice::Constant(third_cons),
            ) = (first_constant, second_constant, third_constant)
            {
                let new_cons = match(op, first_cons, second_cons, third_cons) {
                    (TernaryOperator::Select, Constant::Boolean(first_val), second_val, third_val) => if *first_val {second_val.clone()} else {third_val.clone()},
                    _ => panic!("Unsupported combination of ternary operation and constant values. Did typechecking succeed?")
                };
                ConstantLattice::Constant(new_cons)
            } else if (first_constant.is_top()
                && !second_constant.is_bottom()
                && !third_constant.is_bottom())
                || (!first_constant.is_bottom()
                    && second_constant.is_top()
                    && !first_constant.is_bottom())
                || (!first_constant.is_bottom()
                    && !second_constant.is_bottom()
                    && third_constant.is_top())
            {
                ConstantLattice::top()
            } else {
                ConstantLattice::meet(
                    first_constant,
                    &ConstantLattice::meet(second_constant, third_constant),
                )
            };

            CCPLattice {
                reachability: ReachabilityLattice::meet(
                    first_reachability,
                    &ReachabilityLattice::meet(second_reachability, third_reachability),
                ),
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
        // Read handles reachability when following an if or match.
        Node::Read { collect, indices } => match &function.nodes[collect.idx()] {
            Node::If { control: _, cond } => {
                let cond_constant = &inputs[cond.idx()].constant;
                let if_reachability = &inputs[collect.idx()].reachability;
                let if_constant = &inputs[collect.idx()].constant;

                let new_reachability = if cond_constant.is_top() {
                    ReachabilityLattice::top()
                } else if let ConstantLattice::Constant(cons) = cond_constant {
                    if let Constant::Boolean(val) = cons {
                        if *val && indices[0] == Index::Control(0) {
                            // If condition is true and this is the false
                            // branch, then unreachable.
                            ReachabilityLattice::top()
                        } else if !val && indices[0] == Index::Control(1) {
                            // If condition is true and this is the true branch,
                            // then unreachable.
                            ReachabilityLattice::top()
                        } else {
                            if_reachability.clone()
                        }
                    } else {
                        panic!("Attempted to interpret Read node, where corresponding if node has a non-boolean constant input. Did typechecking succeed?")
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
                let if_reachability = &inputs[collect.idx()].reachability;
                let if_constant = &inputs[collect.idx()].constant;

                let new_reachability = if sum_constant.is_top() {
                    ReachabilityLattice::top()
                } else if let ConstantLattice::Constant(cons) = sum_constant {
                    if let Constant::Summation(_, variant, _) = cons {
                        if Index::Control(*variant as usize) != indices[0] {
                            // If match variant is not the same as this branch,
                            // then unreachable.
                            ReachabilityLattice::top()
                        } else {
                            if_reachability.clone()
                        }
                    } else {
                        panic!("Attempted to interpret Read node, where corresponding match node has a non-summation constant input. Did typechecking succeed?")
                    }
                } else {
                    if_reachability.clone()
                };

                CCPLattice {
                    reachability: new_reachability,
                    constant: if_constant.clone(),
                }
            }
            _ => CCPLattice {
                reachability: inputs[collect.idx()].reachability.clone(),
                constant: ConstantLattice::bottom(),
            },
        },
        // Write is uninterpreted for now.
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
            CCPLattice {
                reachability,
                constant: ConstantLattice::bottom(),
            }
        }
    }
}

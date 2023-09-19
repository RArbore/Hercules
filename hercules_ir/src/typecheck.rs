use crate::*;

use std::collections::HashMap;
use std::iter::zip;

use self::TypeSemilattice::*;

/*
 * Enum for type semilattice.
 */
#[derive(Eq, Clone)]
enum TypeSemilattice {
    Unconstrained,
    Concrete(TypeID),
    Error(String),
}

impl TypeSemilattice {
    fn is_unconstrained(&self) -> bool {
        self == &Unconstrained
    }

    fn is_concrete(&self) -> bool {
        if let Concrete(_) = self {
            true
        } else {
            false
        }
    }

    fn is_error(&self) -> bool {
        if let Error(_) = self {
            true
        } else {
            false
        }
    }

    // During typeflow, the return node is given an error type, even when
    // typechecking succeeds. This is done so that any node that uses a return
    // node will have its output type set to this error. In the top-level type
    // checking function, we ignore this particular error if the node being
    // checked is a return node.
    fn get_return_type_error() -> Self {
        Error(String::from("No node can take a return node as input."))
    }
}

impl PartialEq for TypeSemilattice {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Unconstrained, Unconstrained) => true,
            (Concrete(id1), Concrete(id2)) => id1 == id2,
            (Error(_), Error(_)) => true,
            _ => false,
        }
    }
}

impl Semilattice for TypeSemilattice {
    fn meet(a: &Self, b: &Self) -> Self {
        match (a, b) {
            (Unconstrained, Unconstrained) => Unconstrained,
            (Unconstrained, b) => b.clone(),
            (a, Unconstrained) => a.clone(),
            (Concrete(id1), Concrete(id2)) => {
                if id1 == id2 {
                    Concrete(*id1)
                } else {
                    // Error will only allocate when a type error has occurred.
                    // In that case, we're less concerned about speed to the
                    // compiler, and more allocations are acceptable.
                    Error(format!(
                        "Couldn't reconcile two different concrete types, with IDs {} and {}.",
                        id1.idx(),
                        id2.idx()
                    ))
                }
            }
            (Error(msg), _) => Error(msg.clone()),
            (_, Error(msg)) => Error(msg.clone()),
        }
    }

    fn bottom() -> Self {
        // Strings don't allocate unless they actually contain characters, so
        // this is cheap.
        Error(String::new())
    }

    fn top() -> Self {
        Unconstrained
    }
}

/*
 * Top level typecheck function.
 */
pub fn typecheck(
    function: &Function,
    types: &mut Vec<Type>,
    constants: &Vec<ir::Constant>,
    reverse_post_order: &Vec<NodeID>,
) -> Result<Vec<TypeID>, String> {
    // Step 1: assemble a reverse type map. This is needed to get or create the
    // ID of potentially new types.
    let mut reverse_type_map: HashMap<Type, TypeID> = types
        .iter()
        .enumerate()
        .map(|(idx, ty)| (ty.clone(), TypeID::new(idx)))
        .collect();

    // Step 2: run dataflow. This is an occurrence of dataflow where the flow
    // function performs a non-associative operation on the predecessor "out"
    // values.
    let result = dataflow(
        function,
        reverse_post_order,
        typeflow,
        &mut (function, types, constants, &mut reverse_type_map),
    );

    // Step 3: add type for empty product. This is the type of the return node.
    let empty_prod_ty = Type::Product(Box::new([]));
    let empty_prod_id = if let Some(id) = reverse_type_map.get(&empty_prod_ty) {
        *id
    } else {
        let id = TypeID::new(reverse_type_map.len());
        reverse_type_map.insert(empty_prod_ty.clone(), id);
        types.push(empty_prod_ty);
        id
    };

    // Step 4: convert the individual type lattice values into a list of
    // concrete type values, or a single error.
    zip(result.into_iter(), function.nodes.iter())
        .map(|(x, n)| match x {
            Unconstrained => Err(String::from("Found unconstrained type in program.")),
            Concrete(id) => Ok(id),
            Error(msg) => {
                if n.is_return() && Error(msg.clone()) == TypeSemilattice::get_return_type_error() {
                    Ok(empty_prod_id)
                } else {
                    Err(msg)
                }
            }
        })
        .collect()
}

/*
 * Flow function for typechecking.
 */
fn typeflow(
    inputs: &[&TypeSemilattice],
    auxiliary: &mut (
        &Function,
        &mut Vec<Type>,
        &Vec<ir::Constant>,
        &mut HashMap<Type, TypeID>,
    ),
    id: NodeID,
) -> TypeSemilattice {
    let (function, types, constants, reverse_type_map) = auxiliary;

    // Whenever we want to reference a specific type (for example, for the
    // start node), we need to get its type ID. This helper function gets the
    // ID if it already exists. If the type doesn't already exist, the helper
    // adds it to the type intern list.
    let get_type_id =
        |ty: Type, types: &mut Vec<Type>, reverse_type_map: &mut HashMap<Type, TypeID>| -> TypeID {
            if let Some(id) = reverse_type_map.get(&ty) {
                *id
            } else {
                let id = TypeID::new(reverse_type_map.len());
                reverse_type_map.insert(ty.clone(), id);
                types.push(ty);
                id
            }
        };

    // Each node requires different type logic. This unfortunately results in a
    // large match statement. Oh well. Each arm returns the lattice value for
    // the "out" type of the node.
    match &function.nodes[id.idx()] {
        Node::Start => {
            if inputs.len() != 0 {
                return Error(String::from("Start node must have zero inputs."));
            }

            // The start node is the producer of the control token.
            Concrete(get_type_id(
                Type::Control(Box::new([])),
                types,
                reverse_type_map,
            ))
        }
        Node::Region { preds: _ } => {
            if inputs.len() == 0 {
                return Error(String::from("Region node must have at least one input."));
            }

            let mut meet = inputs[0].clone();
            for l in inputs[1..].iter() {
                meet = TypeSemilattice::meet(&meet, l);
            }

            // Only special case is if concrete type is non-control. In
            // this case, we override that concrete type with an error,
            // since the input types must all be control types. Any other
            // lattice value can be returned as-is.
            if let Concrete(id) = meet {
                if !types[id.idx()].is_control() {
                    return Error(String::from(
                        "Region node's input type cannot be non-control.",
                    ));
                }
            }

            meet
        }
        Node::If {
            control: _,
            cond: _,
        } => {
            if inputs.len() != 2 {
                return Error(String::from("If node must have exactly two inputs."));
            }

            // Check type of data input first, since we may return while
            // checking control input.
            if let Concrete(id) = inputs[1] {
                if !types[id.idx()].is_bool() {
                    return Error(String::from(
                        "If node's condition input cannot have non-boolean type.",
                    ));
                }
            } else if inputs[1].is_error() {
                // If an input has an error lattice value, it must be
                // propagated.
                return inputs[1].clone();
            }

            if let Concrete(id) = inputs[0] {
                if !types[id.idx()].is_control() {
                    return Error(String::from(
                        "If node's control input cannot have non-control type.",
                    ));
                } else {
                    let out_ty = Type::Product(Box::new([*id, *id]));
                    return Concrete(get_type_id(out_ty, types, reverse_type_map));
                }
            }

            inputs[0].clone()
        }
        Node::Fork { control: _, factor } => {
            if inputs.len() != 1 {
                return Error(String::from("Fork node must have exactly one input."));
            }

            if let Concrete(id) = inputs[0] {
                if let Type::Control(factors) = &types[id.idx()] {
                    // Fork adds a new factor to the thread spawn factor list.
                    let mut new_factors = factors.clone().into_vec();
                    new_factors.push(*factor);

                    // Out type is a pair - first element is the control type,
                    // second is the index type (u64). Each thread gets a
                    // different thread ID at runtime.
                    let control_out_id = get_type_id(
                        Type::Control(new_factors.into_boxed_slice()),
                        types,
                        reverse_type_map,
                    );
                    let index_out_id =
                        get_type_id(Type::UnsignedInteger64, types, reverse_type_map);
                    let out_ty = Type::Product(Box::new([control_out_id, index_out_id]));
                    return Concrete(get_type_id(out_ty, types, reverse_type_map));
                } else {
                    return Error(String::from(
                        "Fork node's input cannot have non-control type.",
                    ));
                }
            }

            inputs[0].clone()
        }
        Node::Join {
            control: _,
            data: _,
        } => {
            if inputs.len() != 2 {
                return Error(String::from("Join node must have exactly two inputs."));
            }

            // If the data input isn't concrete, we can't assemble a concrete
            // output type yet, so just return data input's type (either
            // unconstrained or error) instead.
            if let Concrete(data_id) = inputs[1] {
                if types[data_id.idx()].is_control() {
                    return Error(String::from(
                        "Join node's second input must not have a control type.",
                    ));
                }

                // Similarly, if the control input isn't concrete yet, we can't
                // assemble a concrete output type, so just return the control
                // input non-concrete type.
                if let Concrete(control_id) = inputs[0] {
                    if let Type::Control(factors) = &types[control_id.idx()] {
                        // Join removes a factor from the factor list.
                        if factors.len() == 0 {
                            return Error(String::from("Join node's first input must have a control type with at least one thread replication factor."));
                        }
                        let mut new_factors = factors.clone().into_vec();
                        let dc_id = new_factors.pop().unwrap();

                        // Out type is a pair - first element is the control
                        // type, second is the result array from the parallel
                        // computation.
                        let control_out_id = get_type_id(
                            Type::Control(new_factors.into_boxed_slice()),
                            types,
                            reverse_type_map,
                        );
                        let array_out_id =
                            get_type_id(Type::Array(*data_id, dc_id), types, reverse_type_map);
                        let out_ty = Type::Product(Box::new([control_out_id, array_out_id]));
                        return Concrete(get_type_id(out_ty, types, reverse_type_map));
                    } else {
                        return Error(String::from(
                            "Join node's first input cannot have non-control type.",
                        ));
                    }
                } else {
                    return inputs[0].clone();
                }
            }

            inputs[1].clone()
        }
        Node::Phi {
            control: _,
            data: _,
        } => {
            if inputs.len() < 2 {
                return Error(String::from("Phi node must have at least two inputs."));
            }

            // Check type of control input first, since this may produce an
            // error.
            if let Concrete(id) = inputs[inputs.len() - 1] {
                if !types[id.idx()].is_control() {
                    return Error(String::from(
                        "Phi node's control input cannot have non-control type.",
                    ));
                }
            } else if inputs[inputs.len() - 1].is_error() {
                // If an input has an error lattice value, it must be
                // propagated.
                return inputs[inputs.len() - 1].clone();
            }

            // Output type of phi node is same type as every data input.
            let mut meet = inputs[0].clone();
            for l in inputs[1..inputs.len() - 1].iter() {
                if let Concrete(id) = l {
                    if types[id.idx()].is_control() {
                        return Error(String::from(
                            "Phi node's data inputs cannot have control type.",
                        ));
                    }
                }
                meet = TypeSemilattice::meet(&meet, l);
            }

            meet
        }
        Node::Return {
            control: _,
            value: _,
        } => {
            if inputs.len() != 2 {
                return Error(String::from("Return node must have exactly two inputs."));
            }

            // Check type of control input first, since this may produce an
            // error.
            if let Concrete(id) = inputs[0] {
                if let Type::Control(factors) = &types[id.idx()] {
                    if factors.len() != 0 {
                        return Error(String::from(
                            "Return node's control input must have no thread replications.",
                        ));
                    }
                } else {
                    return Error(String::from(
                        "Return node's control input cannot have non-control type.",
                    ));
                }
            } else if inputs[0].is_error() {
                // If an input has an error lattice value, it must be
                // propagated.
                return inputs[0].clone();
            }

            if let Concrete(id) = inputs[1] {
                if *id != function.return_type {
                    return Error(String::from("Return node's data input type must be the same as the function's return type."));
                }
            } else if inputs[1].is_error() {
                return inputs[1].clone();
            }

            // Return nodes are special - they cannot have any users. Thus, we
            // set the return node's lattice value to a specific error. When
            // converting lattice values to types, this particular error gets
            // converted to an empty product type if it's the type of a return
            // node. If any node uses a return node, it's lattice value will be
            // this error. This will result in a normal error when attempting to
            // extract conrete types.
            TypeSemilattice::get_return_type_error()
        }
        Node::Parameter { index } => {
            if inputs.len() != 0 {
                return Error(String::from("Parameter node must have zero inputs."));
            }

            if *index >= function.param_types.len() {
                return Error(String::from("Parameter node must reference an index corresponding to an existing function argument."));
            }

            let param_id = function.param_types[*index];

            Concrete(param_id)
        }
        Node::Constant { id } => {
            if inputs.len() != 0 {
                return Error(String::from("Constant node must have zero inputs."));
            }

            match constants[id.idx()] {
                Constant::Boolean(_) => {
                    Concrete(get_type_id(Type::Boolean, types, reverse_type_map))
                }
                Constant::Integer8(_) => {
                    Concrete(get_type_id(Type::Integer8, types, reverse_type_map))
                }
                Constant::Integer16(_) => {
                    Concrete(get_type_id(Type::Integer16, types, reverse_type_map))
                }
                Constant::Integer32(_) => {
                    Concrete(get_type_id(Type::Integer32, types, reverse_type_map))
                }
                Constant::Integer64(_) => {
                    Concrete(get_type_id(Type::Integer64, types, reverse_type_map))
                }
                Constant::UnsignedInteger8(_) => {
                    Concrete(get_type_id(Type::UnsignedInteger8, types, reverse_type_map))
                }
                Constant::UnsignedInteger16(_) => Concrete(get_type_id(
                    Type::UnsignedInteger16,
                    types,
                    reverse_type_map,
                )),
                Constant::UnsignedInteger32(_) => Concrete(get_type_id(
                    Type::UnsignedInteger32,
                    types,
                    reverse_type_map,
                )),
                Constant::UnsignedInteger64(_) => Concrete(get_type_id(
                    Type::UnsignedInteger64,
                    types,
                    reverse_type_map,
                )),
                Constant::Float32(_) => {
                    Concrete(get_type_id(Type::Float32, types, reverse_type_map))
                }
                Constant::Float64(_) => {
                    Concrete(get_type_id(Type::Float64, types, reverse_type_map))
                }
                Constant::Product(id, _) => Concrete(id),
                Constant::Summation(id, _, _) => Concrete(id),
                Constant::Array(id, _) => Concrete(id),
            }
        }
        Node::DynamicConstant { id: _ } => {
            if inputs.len() != 0 {
                return Error(String::from("DynamicConstant node must have zero inputs."));
            }

            Concrete(get_type_id(
                Type::UnsignedInteger64,
                types,
                reverse_type_map,
            ))
        }
        Node::Unary { input: _, op } => {
            if inputs.len() != 1 {
                return Error(String::from("Unary node must have exactly one input."));
            }

            if let Concrete(id) = inputs[0] {
                match op {
                    UnaryOperator::Not => {
                        if !types[id.idx()].is_bool() {
                            return Error(String::from(
                                "Not unary node input cannot have non-boolean type.",
                            ));
                        }
                    }
                    UnaryOperator::Neg => {
                        if !types[id.idx()].is_arithmetic() {
                            return Error(String::from(
                                "Neg unary node input cannot have non-arithmetic type.",
                            ));
                        }
                    }
                    UnaryOperator::Bitflip => {
                        if !types[id.idx()].is_fixed() {
                            return Error(String::from(
                                "Bitflip unary node input cannot have non-fixed type.",
                            ));
                        }
                    }
                }
            }

            inputs[0].clone()
        }
        _ => todo!(),
    }
}

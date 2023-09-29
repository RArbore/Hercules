use std::collections::HashMap;
use std::iter::zip;

use crate::*;

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
    fn is_error(&self) -> bool {
        if let Error(_) = self {
            true
        } else {
            false
        }
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

pub type ModuleTyping = Vec<Vec<TypeID>>;

/*
 * Top level typecheck function. Typechecking is a module-wide analysis.
 * Returns a type for every node in every function.
 */
pub fn typecheck(
    module: &mut Module,
    reverse_postorders: &Vec<Vec<NodeID>>,
) -> Result<ModuleTyping, String> {
    // Step 1: assemble a reverse type map. This is needed to get or create the
    // ID of potentially new types. Break down module into references to
    // individual elements at this point, so that borrows don't overlap each
    // other.
    let Module {
        ref functions,
        ref mut types,
        ref constants,
        ref dynamic_constants,
    } = module;
    let mut reverse_type_map: HashMap<Type, TypeID> = types
        .iter()
        .enumerate()
        .map(|(idx, ty)| (ty.clone(), TypeID::new(idx)))
        .collect();

    // Step 2: run dataflow. This is an occurrence of dataflow where the flow
    // function performs a non-associative operation on the predecessor "out"
    // values.
    let results: Vec<Vec<TypeSemilattice>> = zip(functions, reverse_postorders)
        .map(|(function, reverse_postorder)| {
            forward_dataflow(function, reverse_postorder, |inputs, id| {
                typeflow(
                    inputs,
                    id,
                    function,
                    functions,
                    types,
                    constants,
                    dynamic_constants,
                    &mut reverse_type_map,
                )
            })
        })
        .collect();

    // Step 3: convert the individual type lattice values into lists of
    // concrete type values, or a single error.
    results
        .into_iter()
        // For each type list, we want to convert its element TypeSemilattices
        // into Result<TypeID, String>.
        .map(|result| {
            result
                .into_iter()
                // For each TypeSemilattice, convert into Result<TypeID, String>.
                .map(|x| match x {
                    Unconstrained => Err(String::from("Found unconstrained type in program.")),
                    Concrete(id) => Ok(id),
                    Error(msg) => Err(msg.clone()),
                })
                .collect()
        })
        .collect()
}

/*
 * Flow function for typechecking.
 */
fn typeflow(
    inputs: &[&TypeSemilattice],
    node: &Node,
    function: &Function,
    functions: &Vec<Function>,
    types: &mut Vec<Type>,
    constants: &Vec<Constant>,
    dynamic_constants: &Vec<DynamicConstant>,
    reverse_type_map: &mut HashMap<Type, TypeID>,
) -> TypeSemilattice {
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
    match node {
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

            Concrete(get_type_id(
                Type::Product(Box::new([])),
                types,
                reverse_type_map,
            ))
        }
        Node::Parameter { index } => {
            if inputs.len() != 0 {
                return Error(String::from("Parameter node must have zero inputs."));
            }

            if *index >= function.param_types.len() {
                return Error(String::from("Parameter node must reference an index corresponding to an existing function argument."));
            }

            // Type of parameter is stored directly in function.
            let param_id = function.param_types[*index];

            Concrete(param_id)
        }
        Node::Constant { id } => {
            if inputs.len() != 0 {
                return Error(String::from("Constant node must have zero inputs."));
            }

            // Most constants' type are obvious.
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
                // Product, summation, and array constants are exceptions.
                // Technically, only summation constants need to explicitly
                // store their type, but product and array constants also
                // explicitly store their type specifically to make this code
                // simpler (although their type could be derived from the
                // constant itself).
                Constant::Product(id, _) => {
                    if let Type::Product(_) = types[id.idx()] {
                        Concrete(id)
                    } else {
                        Error(String::from(
                            "Product constant must store an explicit product type.",
                        ))
                    }
                }
                Constant::Summation(id, _, _) => {
                    if let Type::Summation(_) = types[id.idx()] {
                        Concrete(id)
                    } else {
                        Error(String::from(
                            "Summation constant must store an explicit summation type.",
                        ))
                    }
                }
                // Array typechecking also consists of validating the number of constant elements.
                Constant::Array(id, ref elems) => {
                    if let Type::Array(_, dc_id) = types[id.idx()] {
                        if dynamic_constants[dc_id.idx()] == DynamicConstant::Constant(elems.len())
                        {
                            Concrete(id)
                        } else {
                            Error(String::from("Array constant must have the correct number of constant elements as specified by its type."))
                        }
                    } else {
                        Error(String::from(
                            "Array constant must store an explicit array type.",
                        ))
                    }
                }
            }
        }
        Node::DynamicConstant { id: _ } => {
            if inputs.len() != 0 {
                return Error(String::from("DynamicConstant node must have zero inputs."));
            }

            // Dynamic constants are always u64.
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
        Node::Binary {
            left: _,
            right: _,
            op,
        } => {
            if inputs.len() != 2 {
                return Error(String::from("Binary node must have exactly two inputs."));
            }

            let input_ty = TypeSemilattice::meet(inputs[0], inputs[1]);

            if let Concrete(id) = input_ty {
                match op {
                    BinaryOperator::Add
                    | BinaryOperator::Sub
                    | BinaryOperator::Mul
                    | BinaryOperator::Div
                    | BinaryOperator::Rem => {
                        if !types[id.idx()].is_arithmetic() {
                            return Error(format!(
                                "{:?} binary node input cannot have non-arithmetic type.",
                                op,
                            ));
                        }
                    }
                    BinaryOperator::LT
                    | BinaryOperator::LTE
                    | BinaryOperator::GT
                    | BinaryOperator::GTE => {
                        if !types[id.idx()].is_arithmetic() {
                            return Error(format!(
                                "{:?} binary node input cannot have non-arithmetic type.",
                                op,
                            ));
                        }

                        // Comparison operators change the input type.
                        return Concrete(get_type_id(Type::Boolean, types, reverse_type_map));
                    }
                    BinaryOperator::EQ | BinaryOperator::NE => {
                        if types[id.idx()].is_control() {
                            return Error(format!(
                                "{:?} binary node input cannot have control type.",
                                op,
                            ));
                        }

                        // Equality operators potentially change the input type.
                        return Concrete(get_type_id(Type::Boolean, types, reverse_type_map));
                    }
                    BinaryOperator::Or
                    | BinaryOperator::And
                    | BinaryOperator::Xor
                    | BinaryOperator::LSh
                    | BinaryOperator::RSh => {
                        if !types[id.idx()].is_fixed() {
                            return Error(format!(
                                "{:?} binary node input cannot have non-fixed type.",
                                op,
                            ));
                        }
                    }
                }
            }

            inputs[0].clone()
        }
        Node::Call {
            function: callee_id,
            dynamic_constants: dc_args,
            args: _,
        } => {
            let callee = &functions[callee_id.idx()];

            // Check number of run-time arguments.
            if inputs.len() != callee.param_types.len() {
                return Error(format!(
                    "Call node has {} inputs, but calls a function with {} parameters.",
                    inputs.len(),
                    callee.param_types.len(),
                ));
            }

            // Check number of dynamic constant arguments.
            if dc_args.len() != callee.num_dynamic_constants as usize {
                return Error(format!(
                    "Call node references {} dynamic constants, but calls a function expecting {} dynamic constants.",
                    dc_args.len(),
                    callee.num_dynamic_constants
                ));
            }

            // Check argument types.
            for (input, param_ty) in zip(inputs.iter(), callee.param_types.iter()) {
                if let Concrete(input_id) = input {
                    if input_id != param_ty {
                        return Error(String::from(
                            "Call node mismatches argument types with callee function.",
                        ));
                    }
                } else if input.is_error() {
                    // If an input type is an error, we must propagate it.
                    return (*input).clone();
                }
            }

            Concrete(callee.return_type)
        }
        Node::ReadProd { prod: _, index } => {
            if inputs.len() != 1 {
                return Error(String::from("ReadProd node must have exactly one input."));
            }

            // If the input type isn't concrete, just propagate input type.
            if let Concrete(id) = inputs[0] {
                if let Type::Product(elem_tys) = &types[id.idx()] {
                    if *index >= elem_tys.len() {
                        // ReadProd's index being out of range is a type error.
                        return Error(String::from("ReadProd node's index must be within range of input product type's element list."));
                    } else {
                        return Concrete(elem_tys[*index]);
                    }
                } else {
                    return Error(String::from(
                        "ReadProd node's input type must be a product type.",
                    ));
                }
            }

            inputs[0].clone()
        }
        Node::WriteProd {
            prod: _,
            data: _,
            index,
        } => {
            if inputs.len() != 2 {
                return Error(String::from("WriteProd node must have exactly two inputs."));
            }

            // If the input type isn't concrete, just propagate input type.
            if let Concrete(id) = inputs[0] {
                if let Type::Product(elem_tys) = &types[id.idx()] {
                    if *index >= elem_tys.len() {
                        // ReadProd's index being out of range is a type error.
                        return Error(String::from("WriteProd node's index must be within range of input product type's element list."));
                    } else if let Concrete(data_id) = inputs[1] {
                        if elem_tys[*index] != *data_id {
                            return Error(format!("WriteProd node's data input doesn't match the type of the element at index {} inside the product type.", index));
                        } else if let Type::Control(_) = &types[data_id.idx()] {
                            return Error(String::from(
                                "WriteProd node's data input cannot have a control type.",
                            ));
                        }
                    } else if inputs[1].is_error() {
                        // If an input lattice value is an error, we must
                        // propagate it.
                        return inputs[1].clone();
                    }
                    return Concrete(elem_tys[*index]);
                } else {
                    return Error(String::from(
                        "WriteProd node's input type must be a product type.",
                    ));
                }
            }

            inputs[0].clone()
        }
        Node::ReadArray { array: _, index: _ } => {
            if inputs.len() != 2 {
                return Error(String::from("ReadArray node must have exactly two inputs."));
            }

            // Check that index has unsigned type.
            if let Concrete(id) = inputs[1] {
                if !types[id.idx()].is_unsigned() {
                    return Error(String::from(
                        "ReadArray node's index input must have unsigned type.",
                    ));
                }
            } else if inputs[1].is_error() {
                return inputs[1].clone();
            }

            // If array input is concrete, we can get type of ReadArray node.
            if let Concrete(id) = inputs[0] {
                if let Type::Array(elem_id, _) = types[id.idx()] {
                    return Concrete(elem_id);
                } else {
                    return Error(String::from(
                        "ReadArray node's array input must have array type.",
                    ));
                }
            }

            inputs[0].clone()
        }
        Node::WriteArray {
            array: _,
            data: _,
            index: _,
        } => {
            if inputs.len() != 3 {
                return Error(String::from("WriteArray node must have exactly 3 inputs."));
            }

            // Check that index has unsigned type.
            if let Concrete(id) = inputs[2] {
                if !types[id.idx()].is_unsigned() {
                    return Error(String::from(
                        "WriteArray node's index input must have unsigned type.",
                    ));
                }
            } else if inputs[2].is_error() {
                return inputs[2].clone();
            }

            // Check that array and data types match.
            if let Concrete(array_id) = inputs[0] {
                if let Type::Array(elem_id, _) = types[array_id.idx()] {
                    if let Concrete(data_id) = inputs[1] {
                        if elem_id != *data_id {
                            return Error(String::from("WriteArray node's array and data inputs must have compatible types (type of data input must be the same as the array input's element type)."));
                        } else if let Type::Control(_) = &types[data_id.idx()] {
                            return Error(String::from(
                                "WriteArray node's data input cannot have a control type.",
                            ));
                        }
                    }
                } else {
                    return Error(String::from(
                        "WriteArray node's array input must have array type.",
                    ));
                }
            }

            // If an input type is an error, we must propagate it.
            if inputs[1].is_error() {
                return inputs[1].clone();
            }

            inputs[0].clone()
        }
        Node::Match { control: _, sum: _ } => {
            if inputs.len() != 2 {
                return Error(String::from("Match node must have exactly two inputs."));
            }

            // Check sum and control inputs simultaneously, since both need to
            // be concrete to determine a concrete type for a match node.
            if let (Concrete(control_id), Concrete(sum_id)) = (inputs[0], inputs[1]) {
                if let Type::Summation(variants) = &types[sum_id.idx()] {
                    if !types[control_id.idx()].is_control() {
                        return Error(String::from(
                            "Match node's control input cannot have non-control type.",
                        ));
                    } else {
                        let out_ty =
                            Type::Product(vec![*control_id; variants.len()].into_boxed_slice());
                        return Concrete(get_type_id(out_ty, types, reverse_type_map));
                    }
                } else {
                    return Error(String::from(
                        "Match node's condition input cannot have non-sum type.",
                    ));
                }
            }

            // Otherwise, currently unconstrained, or an error.
            match TypeSemilattice::meet(inputs[0], inputs[1]) {
                TypeSemilattice::Unconstrained => TypeSemilattice::Unconstrained,
                TypeSemilattice::Concrete(_) => TypeSemilattice::Unconstrained,
                TypeSemilattice::Error(msg) => TypeSemilattice::Error(msg),
            }
        }
        Node::BuildSum {
            data: _,
            sum_ty,
            variant,
        } => {
            if inputs.len() != 1 {
                return Error(String::from("BuildSum node must have exactly one input."));
            }

            if let Concrete(id) = inputs[0] {
                if let Type::Control(_) = &types[id.idx()] {
                    return Error(String::from(
                        "BuildSum node's data input cannot have a control type.",
                    ));
                }

                // BuildSum node stores its own result type.
                if let Type::Summation(variants) = &types[sum_ty.idx()] {
                    // Must reference an existing variant.
                    if *variant >= variants.len() {
                        return Error(String::from("BuildSum node's variant number must be in range of valid variant numbers for referenced sum type."));
                    }

                    // The variant type has to be the same as the type of data.
                    if *id == variants[*variant] {
                        return Error(String::from(
                            "BuildSum node's input type must match the referenced variant type.",
                        ));
                    }

                    return Concrete(*sum_ty);
                } else {
                    return Error(String::from("BuildSum node must reference a sum type."));
                }
            }

            inputs[0].clone()
        }
        Node::ExtractSum { data: _, variant } => {
            if inputs.len() != 1 {
                return Error(String::from("ExtractSum node must have exactly one input."));
            }

            if let Concrete(id) = inputs[0] {
                if let Type::Summation(variants) = &types[id.idx()] {
                    // Must reference an existing variant.
                    if *variant >= variants.len() {
                        return Error(String::from("BuildSum node's variant number must be in range of valid variant numbers for referenced sum type."));
                    }

                    return Concrete(variants[*variant]);
                } else {
                    return Error(String::from(
                        "ExtractSum node's input cannot have non-sum type.",
                    ));
                }
            }

            inputs[0].clone()
        }
    }
}

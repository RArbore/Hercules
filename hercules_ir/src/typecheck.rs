use std::collections::HashMap;
use std::iter::zip;

use crate::*;

use self::TypeSemilattice::*;

/*
 * Enum for type semilattice.
 */
#[derive(Eq, Clone, Debug)]
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

/* Define custom PartialEq, so that dataflow will terminate right away if there
 * are errors.
 */
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
            (Unconstrained, b) => b.clone(),
            (a, Unconstrained) => a.clone(),
            (Concrete(id1), Concrete(id2)) => {
                if id1 == id2 {
                    Concrete(*id1)
                } else {
                    // Error will only allocate when a type error has occurred.
                    // In that case, we're less concerned about speed of the
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

    // Also create a join replication factor map. This is needed to typecheck
    // collect node.
    let mut join_factor_map: HashMap<NodeID, DynamicConstantID> = HashMap::new();

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
                    &mut join_factor_map,
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
    node_id: NodeID,
    function: &Function,
    functions: &Vec<Function>,
    types: &mut Vec<Type>,
    constants: &Vec<Constant>,
    dynamic_constants: &Vec<DynamicConstant>,
    reverse_type_map: &mut HashMap<Type, TypeID>,
    join_factor_map: &mut HashMap<NodeID, DynamicConstantID>,
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

    // We need to make sure dynamic constant parameters reference valid dynamic
    // constant parameters of the current function. This involves traversing a
    // given dynamic constant expression to determine that all referenced
    // parameter dynamic constants are valid.
    fn check_dynamic_constants(
        root: DynamicConstantID,
        dynamic_constants: &Vec<DynamicConstant>,
        num_parameters: u32,
    ) -> bool {
        match dynamic_constants[root.idx()] {
            DynamicConstant::Parameter(idx) => idx < num_parameters as usize,
            _ => true,
        }
    }

    // Each node requires different type logic. This unfortunately results in a
    // large match statement. Oh well. Each arm returns the lattice value for
    // the "out" type of the node.
    match &function.nodes[node_id.idx()] {
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
                }
            }

            inputs[0].clone()
        }
        Node::Fork { control: _, factor } => {
            if inputs.len() != 1 {
                return Error(String::from("Fork node must have exactly one input."));
            }

            if !check_dynamic_constants(*factor, dynamic_constants, function.num_dynamic_constants)
            {
                return Error(String::from("Referenced parameter dynamic constant is not a valid dynamic constant parameter for the current function."));
            }

            if let Concrete(id) = inputs[0] {
                if let Type::Control(factors) = &types[id.idx()] {
                    // Fork adds a new factor to the thread spawn factor list.
                    let mut new_factors = factors.clone().into_vec();
                    new_factors.push(node_id);

                    // Out type is control type, with the new thread spawn
                    // factor.
                    let control_out_id = get_type_id(
                        Type::Control(new_factors.into_boxed_slice()),
                        types,
                        reverse_type_map,
                    );
                    return Concrete(control_out_id);
                } else {
                    return Error(String::from(
                        "Fork node's input cannot have non-control type.",
                    ));
                }
            }

            inputs[0].clone()
        }
        Node::Join { control: _ } => {
            if inputs.len() != 1 {
                return Error(String::from("Join node must have exactly two inputs."));
            }

            // If the control input isn't concrete yet, we can't assemble a
            // concrete output type, so just return the control input non-
            // concrete type.
            if let Concrete(control_id) = inputs[0] {
                if let Type::Control(factors) = &types[control_id.idx()] {
                    // Join removes a factor from the factor list.
                    if factors.len() == 0 {
                        return Error(String::from("Join node's first input must have a control type with at least one thread replication factor."));
                    }
                    let mut new_factors = factors.clone().into_vec();
                    let factor = if let Node::Fork { control: _, factor } =
                        function.nodes[new_factors.pop().unwrap().idx()]
                    {
                        factor
                    } else {
                        panic!("Node ID in factor list doesn't correspond with a fork node.");
                    };
                    join_factor_map.insert(node_id, factor);

                    // Out type is the new control type.
                    let control_out_id = get_type_id(
                        Type::Control(new_factors.into_boxed_slice()),
                        types,
                        reverse_type_map,
                    );
                    return Concrete(control_out_id);
                } else {
                    return Error(String::from(
                        "Join node's first input cannot have non-control type.",
                    ));
                }
            }

            inputs[0].clone()
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
        Node::ThreadID { control: _ } => {
            if inputs.len() != 1 {
                return Error(String::from("ThreadID node must have exactly one input."));
            }

            // If type of control input is an error, we must propagate it.
            if inputs[0].is_error() {
                return inputs[0].clone();
            }

            // Type of thread ID is always u64.
            Concrete(get_type_id(
                Type::UnsignedInteger64,
                types,
                reverse_type_map,
            ))
        }
        Node::Reduce {
            control: _,
            init: _,
            reduct: _,
        } => {
            if inputs.len() != 3 {
                return Error(String::from("Reduce node must have exactly two inputs."));
            }

            if let (Concrete(control_id), Concrete(init_id), Concrete(reduct_id)) =
                (inputs[0], inputs[1], inputs[2])
            {
                // Check control input is control.
                if let Type::Control(_) = types[control_id.idx()] {
                } else {
                    return Error(String::from(
                        "Reduce node's control input must have control type.",
                    ));
                }

                // Check init input isn't control.
                if let Type::Control(_) = types[init_id.idx()] {
                    return Error(String::from(
                        "Reduce node's initialization input must not have control type.",
                    ));
                }

                // Check reduct input isn't control.
                if let Type::Control(_) = types[reduct_id.idx()] {
                    return Error(String::from(
                        "Reduce node's reduction input must not have control type.",
                    ));
                }

                TypeSemilattice::meet(inputs[1], inputs[2])
            } else if inputs[0].is_error() {
                inputs[0].clone()
            } else {
                TypeSemilattice::meet(inputs[1], inputs[2])
            }
        }
        Node::Return {
            control: _,
            data: _,
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
            if inputs.len() != 1 {
                return Error(String::from("Parameter node must have one input."));
            }

            if *index >= function.param_types.len() {
                return Error(String::from("Parameter node must reference an index corresponding to an existing function argument."));
            }

            // Type of parameter is stored directly in function.
            let param_id = function.param_types[*index];

            Concrete(param_id)
        }
        Node::Constant { id } => {
            if inputs.len() != 1 {
                return Error(String::from("Constant node must have one input."));
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
                    if let Type::Array(_, dc_ids) = &types[id.idx()] {
                        let mut total_num_elems = 1;
                        for dc_id in dc_ids.iter() {
                            total_num_elems *= if let DynamicConstant::Constant(extent) =
                                dynamic_constants[dc_id.idx()]
                            {
                                extent
                            } else {
                                return Error(String::from("Array constant type must reference only constant valued dynamic constants."));
                            };
                        }
                        if total_num_elems != 1 && total_num_elems != elems.len() {
                            return Error(String::from("Array constant must have a compatible amount of elements as the extent of the array."));
                        }
                        Concrete(id)
                    } else {
                        Error(String::from(
                            "Array constant must store an explicit array type.",
                        ))
                    }
                }
                // Zero constants need to store their type, and we trust it.
                Constant::Zero(id) => Concrete(id),
            }
        }
        Node::DynamicConstant { id } => {
            if inputs.len() != 1 {
                return Error(String::from("DynamicConstant node must have one input."));
            }

            if !check_dynamic_constants(*id, dynamic_constants, function.num_dynamic_constants) {
                return Error(String::from("Referenced parameter dynamic constant is not a valid dynamic constant parameter for the current function."));
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
                        if !types[id.idx()].is_bool() && !types[id.idx()].is_fixed() {
                            return Error(String::from(
                                "Not unary node input cannot have non-bool and non-fixed type.",
                            ));
                        }
                    }
                    UnaryOperator::Neg => {
                        if types[id.idx()].is_unsigned() {
                            return Error(String::from(
                                "Neg unary node input cannot have unsigned type.",
                            ));
                        }
                        if !types[id.idx()].is_arithmetic() {
                            return Error(String::from(
                                "Neg unary node input cannot have non-arithmetic type.",
                            ));
                        }
                    }
                    UnaryOperator::Cast(dst_id) => {
                        let src_ty = &types[id.idx()];
                        let dst_ty = &types[dst_id.idx()];
                        if cast_compatible(src_ty, dst_ty) {
                            return Concrete(*dst_id);
                        } else {
                            return Error(String::from(
                                "Cast unary node has incompatible input and output types.",
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
                    BinaryOperator::Or | BinaryOperator::And | BinaryOperator::Xor => {
                        if !types[id.idx()].is_fixed() && !types[id.idx()].is_bool() {
                            return Error(format!(
                                "{:?} binary node input cannot have non-fixed type and non-boolean type.",
                                op,
                            ));
                        }
                    }
                    BinaryOperator::LSh | BinaryOperator::RSh => {
                        if !types[id.idx()].is_fixed() {
                            return Error(format!(
                                "{:?} binary node input cannot have non-fixed type.",
                                op,
                            ));
                        }
                    }
                }
            }

            input_ty.clone()
        }
        Node::Ternary {
            first: _,
            second: _,
            third: _,
            op,
        } => {
            if inputs.len() != 3 {
                return Error(String::from("Ternary node must have exactly three inputs."));
            }

            if let Concrete(id) = inputs[0] {
                match op {
                    TernaryOperator::Select => {
                        if !types[id.idx()].is_bool() {
                            return Error(String::from(
                                "Select ternary node input cannot have non-bool condition input.",
                            ));
                        }

                        let data_ty = TypeSemilattice::meet(inputs[1], inputs[2]);
                        if let Concrete(data_id) = data_ty {
                            return Concrete(data_id);
                        } else {
                            return data_ty;
                        }
                    }
                }
            }

            Error(String::from("Unhandled ternary types."))
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

            for dc_id in dc_args.iter() {
                if !check_dynamic_constants(
                    *dc_id,
                    dynamic_constants,
                    function.num_dynamic_constants,
                ) {
                    return Error(String::from("Referenced parameter dynamic constant is not a valid dynamic constant parameter for the current function."));
                }
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
        Node::Read {
            collect: _,
            indices,
        } => {
            if indices.len() == 0 {
                return Error(String::from("Read node must have at least one index."));
            }

            // Traverse the collect input's type tree downwards.
            if let Concrete(mut collect_id) = inputs[0] {
                for index in indices.iter() {
                    match (&types[collect_id.idx()], index) {
                        (Type::Product(fields), Index::Field(field)) => {
                            if *field >= fields.len() {
                                return Error(String::from("Read node's field index must be in the range of the product type being indexed."));
                            }
                            collect_id = fields[*field];
                        }
                        (Type::Summation(variants), Index::Variant(variant)) => {
                            if *variant >= variants.len() {
                                return Error(String::from("Read node's variant index must be in the range of the variant type being indexed."));
                            }
                            collect_id = variants[*variant];
                        }
                        (Type::Array(elem_ty_id, dim_sizes), Index::Position(indices)) => {
                            if dim_sizes.len() != indices.len() {
                                return Error(String::from("Read node's position index must have the same number of dimensions as the array type being indexed."));
                            }
                            collect_id = *elem_ty_id;
                        }
                        (Type::Control(_), Index::Control(_)) => {}
                        _ => {
                            return Error(String::from(
                                "Read node has mismatched input type and indices.",
                            ));
                        }
                    }
                }

                // If successfully traversed, the leaf type is the result.
                return Concrete(collect_id);
            }

            inputs[0].clone()
        }
        Node::Write {
            collect: _,
            data: _,
            indices,
        } => {
            if indices.len() == 0 {
                return Error(String::from("Write node must have at least one index."));
            }

            // Traverse the collect input's type tree downwards.
            if let (Concrete(mut collect_id), Concrete(data_id)) = (inputs[0], inputs[1]) {
                for index in indices.iter() {
                    match (&types[collect_id.idx()], index) {
                        (Type::Product(fields), Index::Field(field)) => {
                            if *field >= fields.len() {
                                return Error(String::from("Write node's field index must be in the range of the product type being indexed."));
                            }
                            collect_id = fields[*field];
                        }
                        (Type::Summation(variants), Index::Variant(variant)) => {
                            if *variant >= variants.len() {
                                return Error(String::from("Write node's variant index must be in the range of the variant type being indexed."));
                            }
                            collect_id = variants[*variant];
                        }
                        (Type::Array(elem_ty_id, dim_sizes), Index::Position(indices)) => {
                            if dim_sizes.len() != indices.len() {
                                return Error(String::from("Write node's position index must have the same number of dimensions as the array type being indexed."));
                            }
                            collect_id = *elem_ty_id;
                        }
                        (Type::Control(_), Index::Control(_)) => {}
                        _ => {
                            return Error(String::from(
                                "Write node has mismatched input type and indices.",
                            ));
                        }
                    }
                }

                // The leaf type being indexed must be what's being written.
                if *data_id != collect_id {
                    return Error(String::from(
                        "Write node has mismatched data type and indexed type.",
                    ));
                }
            }

            // No matter what, the type is the type of the collect input.
            inputs[0].clone()
        }
        Node::Match { control: _, sum: _ } => {
            if inputs.len() != 2 {
                return Error(String::from("Match node must have exactly two inputs."));
            }

            // Check sum and control inputs simultaneously, since both need to
            // be concrete to determine a concrete type for a match node.
            if let (Concrete(control_id), Concrete(sum_id)) = (inputs[0], inputs[1]) {
                if let Type::Summation(_) = &types[sum_id.idx()] {
                    if !types[control_id.idx()].is_control() {
                        return Error(String::from(
                            "Match node's control input cannot have non-control type.",
                        ));
                    } else {
                        return inputs[0].clone();
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
    }
}

/*
 * Top level function for creating a fork-join map. Map is from fork node ID to
 * join node ID, since a join can easily determine the fork it corresponds to
 * (that's the mechanism used to implement this analysis). This analysis depends
 * on type information.
 */
pub fn fork_join_map(
    function: &Function,
    typing: &Vec<TypeID>,
    types: &Vec<Type>,
) -> HashMap<NodeID, NodeID> {
    let mut fork_join_map = HashMap::new();
    for idx in 0..function.nodes.len() {
        // We only care about join nodes.
        if let Node::Join { control } = function.nodes[idx] {
            // A join's input type must be control. Since we have types, if this
            // isn't the case, the typing is incorrect and we should panic.
            if let Type::Control(factors) = &types[typing[control.idx()].idx()] {
                let join_id = NodeID::new(idx);
                let fork_id = *factors.last().unwrap();
                fork_join_map.insert(fork_id, join_id);
            } else {
                panic!("Join node's control predecessor has a non-control type.");
            }
        }
    }
    fork_join_map
}

/*
 * Determine if a given cast conversion is valid.
 */
pub fn cast_compatible(src_ty: &Type, dst_ty: &Type) -> bool {
    // Can convert between any pair of primitive types, as long as the cast is
    // not from a floating point type to a boolean type.
    src_ty.is_primitive() && dst_ty.is_primitive() && !(src_ty.is_float() && dst_ty.is_bool())
}

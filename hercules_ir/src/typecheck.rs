use crate::*;

use std::collections::HashMap;

use Node::*;

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

    // Step 3: convert the individual type lattice values into a list of
    // concrete type values, or a single error.
    result
        .into_iter()
        .map(|x| match x {
            Unconstrained => Err(String::from("Found unconstrained type in program.")),
            Concrete(id) => Ok(id),
            Error(msg) => Err(msg),
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
    let (function, types, constant, reverse_type_map) = auxiliary;

    // Whenever we want to reference a specific type (for example, for the
    // start node), we need to get its type ID. This helper function gets the
    // ID if it already exists. If the type doesn't already exist, the helper
    // adds it to the type intern list.
    let mut get_type_id = |ty: Type| -> TypeID {
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
        Start => {
            if inputs.len() != 0 {
                Error(String::from("Start node must have zero inputs."))
            } else {
                Concrete(get_type_id(Type::Control(Box::new([]))))
            }
        }
        Region { preds: _ } => {
            if inputs.len() == 0 {
                Error(String::from(
                    "Region node must have at least one predecessor.",
                ))
            } else {
                let mut meet = inputs[0].clone();
                for l in inputs[1..].iter() {
                    meet = TypeSemilattice::meet(&meet, l);
                }
                if let Concrete(id) = meet {
                    if let Type::Control(_) = types[id.idx()] {
                        meet
                    } else {
                        Error(String::from(
                            "Region node's input type cannot be non-control.",
                        ))
                    }
                } else {
                    meet
                }
            }
        }
        _ => todo!(),
    }
}

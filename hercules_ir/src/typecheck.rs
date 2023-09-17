use crate::*;

/*
 * Enum for type semilattice.
 */
#[derive(Eq, Clone)]
enum TypeSemilattice {
    Unconstrained,
    Concrete(TypeID),
    Error(String),
}

use self::TypeSemilattice::*;

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
    types: &Vec<Type>,
    constants: &Vec<Constant>,
    reverse_post_order: &Vec<NodeID>,
) -> Result<Vec<Type>, String> {
    todo!()
}

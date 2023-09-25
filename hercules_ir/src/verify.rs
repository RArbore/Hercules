use crate::*;

/*
 * Top level IR verification function. Verification runs passes that produce
 * useful results (typing, dominator trees, etc.), so if verification succeeds,
 * return those useful results. Otherwise, return the first error string found.
 */
pub fn verify(module: &mut Module) -> Result<ModuleTyping, String> {
    let typing = typecheck(module)?;
    for function in module.functions.iter() {
        verify_structure(&function)?;
    }
    Ok(typing)
}

/*
 * There are structural constraints the IR must follow, such as all Phi nodes'
 * control input must be a region node. This is where those properties are
 * verified.
 */
fn verify_structure(function: &Function) -> Result<(), String> {
    for node in function.nodes.iter() {
        match node {
            Node::Phi { control, data: _ } => {
                if let Node::Region { preds: _ } = function.nodes[control.idx()] {
                } else {
                    Err("Phi node's control input must be a region node.")?;
                }
            }
            _ => {}
        };
    }
    Ok(())
}

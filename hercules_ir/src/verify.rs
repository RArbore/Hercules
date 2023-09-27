use std::iter::zip;

use crate::*;

/*
 * Top level IR verification function. Verification runs passes that produce
 * useful results (typing, dominator trees, etc.), so if verification succeeds,
 * return those useful results. Otherwise, return the first error string found.
 */
pub fn verify(module: &mut Module) -> Result<ModuleTyping, String> {
    let def_uses: Vec<_> = module
        .functions
        .iter()
        .map(|function| def_use(function))
        .collect();
    let reverse_postorders: Vec<_> = def_uses
        .iter()
        .map(|def_use| reverse_postorder(def_use))
        .collect();
    let typing = typecheck(module, &reverse_postorders)?;
    for (function, def_use) in zip(module.functions.iter(), def_uses.iter()) {
        verify_structure(function, def_use)?;
    }
    Ok(typing)
}

/*
 * There are structural constraints the IR must follow, such as all Phi nodes'
 * control input must be a region node. This is where those properties are
 * verified.
 */
fn verify_structure(function: &Function, def_use: &ImmutableDefUseMap) -> Result<(), String> {
    for (idx, node) in function.nodes.iter().enumerate() {
        let users = def_use.get_users(NodeID::new(idx));
        match node {
            Node::Phi { control, data: _ } => {
                if let Node::Region { preds: _ } = function.nodes[control.idx()] {
                } else {
                    Err("Phi node's control input must be a region node.")?;
                }
            }
            Node::If {
                control: _,
                cond: _,
            } => {
                if users.len() != 2 {
                    Err(format!("If node must have 2 users, not {}.", users.len()))?;
                }
                if let (
                    Node::ReadProd {
                        prod: _,
                        index: index1,
                    },
                    Node::ReadProd {
                        prod: _,
                        index: index2,
                    },
                ) = (
                    &function.nodes[users[0].idx()],
                    &function.nodes[users[1].idx()],
                ) {
                    if !((*index1 == 0 && *index2 == 1) || (*index1 == 1 && *index2 == 0)) {
                        Err("If node's user ReadProd nodes must reference different elements of If node's output product.")?;
                    }
                } else {
                    Err("If node's users must both be ReadProd nodes.")?;
                }
            }
            Node::Return {
                control: _,
                value: _,
            } => {
                if users.len() != 0 {
                    Err(format!(
                        "Return node must have 0 users, not {}.",
                        users.len()
                    ))?;
                }
            }
            _ => {}
        };
    }
    Ok(())
}

extern crate bitvec;

use std::iter::zip;

use verify::bitvec::prelude::*;

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

    // Typecheck the module.
    let typing = typecheck(module, &reverse_postorders)?;

    // Check the structure of the functions in the module.
    for (function, (def_use, typing)) in
        zip(module.functions.iter(), zip(def_uses.iter(), typing.iter()))
    {
        verify_structure(function, def_use, typing, &module.types)?;
    }

    // Check SSA, fork, and join dominance relations.
    for (function, def_use) in zip(module.functions.iter(), def_uses) {
        let subgraph = control_subgraph(function, &def_use);
        let dom = dominator(&subgraph, NodeID::new(0));
        let postdom = postdominator(subgraph, NodeID::new(function.nodes.len()));
        println!("{:?}", dom);
        println!("{:?}", postdom);
    }

    Ok(typing)
}

/*
 * There are structural constraints the IR must follow, such as all Phi nodes'
 * control input must be a region node. This is where those properties are
 * verified.
 */
fn verify_structure(
    function: &Function,
    def_use: &ImmutableDefUseMap,
    typing: &Vec<TypeID>,
    types: &Vec<Type>,
) -> Result<(), String> {
    for (idx, node) in function.nodes.iter().enumerate() {
        let users = def_use.get_users(NodeID::new(idx));
        match node {
            // If, fork, and join nodes all have the same structural
            // constraints - each must have exactly two ReadProd users, which
            // reference differing elements of the node's output product.
            Node::If {
                control: _,
                cond: _,
            }
            | Node::Fork {
                control: _,
                factor: _,
            }
            | Node::Join {
                control: _,
                data: _,
            } => {
                if users.len() != 2 {
                    Err(format!(
                        "{} node must have 2 users, not {}.",
                        node.upper_case_name(),
                        users.len()
                    ))?;
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
                        Err(format!("{} node's user ReadProd nodes must reference different elements of output product.", node.upper_case_name()))?;
                    }
                } else {
                    Err(format!(
                        "{} node's users must both be ReadProd nodes.",
                        node.upper_case_name()
                    ))?;
                }
            }
            // Phi nodes must depend on a region node.
            Node::Phi { control, data: _ } => {
                if let Node::Region { preds: _ } = function.nodes[control.idx()] {
                } else {
                    Err("Phi node's control input must be a region node.")?;
                }
            }
            // Return nodes must have no users.
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
            // Match nodes are similar to if nodes, but have a variable number
            // of ReadProd users, corresponding to the sum type being matched.
            Node::Match { control: _, sum } => {
                let sum_ty = &types[typing[sum.idx()].idx()];
                if let Type::Summation(tys) = sum_ty {
                    let correct_number_of_users = tys.len();
                    if users.len() != correct_number_of_users {
                        Err(format!(
                            "Match node must have {} users, not {}.",
                            correct_number_of_users,
                            users.len()
                        ))?;
                    }
                    let mut users_covered = bitvec![u8, Lsb0; 0; users.len()];
                    for user in users {
                        if let Node::ReadProd { prod: _, index } = function.nodes[user.idx()] {
                            assert!(index < users.len(), "ReadProd child of match node reads from bad index, but ran after typecheck succeeded.");
                            users_covered.set(index, true);
                        }
                    }
                    if users_covered.count_ones() != users.len() {
                        Err(format!("Match node's user ReadProd nodes must reference all {} elements of match node's output product, but they only reference {} of them.", users.len(), users_covered.count_ones()))?;
                    }
                } else {
                    panic!("Type of match node's sum input is not a summation type, but ran after typecheck succeeded.");
                }
            }
            _ => {}
        };
    }
    Ok(())
}

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
    for (function, (def_use, reverse_postorder)) in zip(
        module.functions.iter(),
        zip(def_uses.iter(), reverse_postorders.iter()),
    ) {
        let control_output_dependencies =
            forward_dataflow(function, reverse_postorder, |inputs, id| {
                control_output_flow(inputs, id, function)
            });
        let subgraph = control_subgraph(function, def_use);
        let dom = dominator(&subgraph, NodeID::new(0));
        let postdom = postdominator(subgraph, NodeID::new(function.nodes.len()));
        verify_dominance_relationships(function, &control_output_dependencies, &dom, &postdom)?;
    }

    Ok(typing)
}

/*
 * There are structural constraints the IR must follow, such as all phi nodes'
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
            // Each if node must have exactly two ReadProd users, which
            // reference differing elements of the node's output product.
            Node::If {
                control: _,
                cond: _,
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
            // ThreadID nodes must depend on a fork node.
            Node::ThreadID { control } => {
                if let Node::Fork {
                    control: _,
                    factor: _,
                } = function.nodes[control.idx()]
                {
                } else {
                    Err("ThreadID node's control input must be a fork node.")?;
                }
            }
            // Collect nodes must depend on a join node.
            Node::Collect { control, data: _ } => {
                if let Node::Join { control: _ } = function.nodes[control.idx()] {
                } else {
                    Err("Collect node's control input must be a join node.")?;
                }
            }
            // Return nodes must have no users.
            Node::Return {
                control: _,
                data: _,
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

/*
 * There are dominance relationships the IR must follow, such as all uses of a
 * phi node must be dominated by the corresponding region node.
 */
fn verify_dominance_relationships(
    function: &Function,
    control_output_dependencies: &Vec<UnionNodeSet>,
    dom: &DomTree,
    postdom: &DomTree,
) -> Result<(), String> {
    for idx in 0..function.nodes.len() {
        let dependencies = &control_output_dependencies[idx];
        for other_idx in 0..function.nodes.len() {
            if dependencies.is_set(NodeID::new(other_idx)) {
                match function.nodes[other_idx] {
                    Node::Phi { control, data: _ } => {
                        // If the current node is a control node and the phi's
                        // region doesn't dominate it, then the phi doesn't
                        // dominate its use.
                        if dom.is_non_root(NodeID::new(idx))
                            && !dom.does_dom(control, NodeID::new(idx))
                        {
                            Err(format!(
                                "Phi node (ID {}) doesn't dominate its use (ID {}).",
                                other_idx, idx
                            ))?;
                        }

                        // If the current node is a phi or collect node whose
                        // corresponding region or join node isn't dominated by
                        // the other phi node, then the other phi doesn't
                        // dominate its use. We don't need to do something
                        // similar for thread ID nodes, since they have no data
                        // input. In fact, it's impossible to reach this point
                        // in control as a thread ID node, since it can't
                        // possibly depend on a phi, thread ID, or collect node
                        // in the first place.
                        if let Node::Phi {
                            control: dominated_control,
                            data: _,
                        } = function.nodes[idx]
                        {
                            if !dom.does_dom(control, dominated_control) {
                                Err(format!(
                                    "Phi node (ID {}) doesn't dominate its use (ID {}).",
                                    other_idx, idx
                                ))?;
                            }
                        } else if let Node::Collect {
                            control: dominated_control,
                            data: _,
                        } = function.nodes[idx]
                        {
                            if !dom.does_dom(control, dominated_control) {
                                Err(format!(
                                    "Phi node (ID {}) doesn't dominate its use (ID {}).",
                                    other_idx, idx
                                ))?;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    Ok(())
}

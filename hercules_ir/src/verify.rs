extern crate bitvec;

use std::collections::HashMap;
use std::iter::zip;

use verify::bitvec::prelude::*;

use crate::*;

/*
 * Top level IR verification function. Verification runs passes that produce
 * useful results (typing, dominator trees, etc.), so if verification succeeds,
 * return those useful results. Otherwise, return the first error string found.
 */
pub fn verify(
    module: &mut Module,
) -> Result<
    (
        Vec<ImmutableDefUseMap>,
        Vec<Vec<NodeID>>,
        ModuleTyping,
        Vec<DomTree>,
        Vec<DomTree>,
        Vec<HashMap<NodeID, NodeID>>,
    ),
    String,
> {
    // Calculate def uses and reverse postorders.
    let def_uses: Vec<_> = module.functions.iter().map(def_use).collect();
    let reverse_postorders: Vec<_> = def_uses.iter().map(reverse_postorder).collect();

    // Typecheck the module.
    let typing = typecheck(module, &reverse_postorders)?;

    // Assemble fork join maps for module.
    let fork_join_maps: Vec<_> = zip(module.functions.iter(), typing.iter())
        .map(|(function, typing)| fork_join_map(function, typing, &module.types))
        .collect();

    // Check the structure of the functions in the module.
    for (function, (def_use, typing)) in
        zip(module.functions.iter(), zip(def_uses.iter(), typing.iter()))
    {
        verify_structure(function, def_use, typing, &module.types)?;
    }

    // Calculate dominator and postdominator trees.
    let subgraphs: Vec<_> = zip(module.functions.iter(), def_uses.iter())
        .map(|(function, def_use)| control_subgraph(function, def_use))
        .collect();
    let doms: Vec<_> = subgraphs
        .iter()
        .map(|subgraph| dominator(subgraph, NodeID::new(0)))
        .collect();
    let postdoms: Vec<_> = zip(subgraphs.into_iter(), module.functions.iter())
        .map(|(subgraph, function)| postdominator(subgraph, NodeID::new(function.nodes.len())))
        .collect();

    // Check dominance relations.
    for idx in 0..module.functions.len() {
        let function = &module.functions[idx];
        let reverse_postorder = &reverse_postorders[idx];
        let dom = &doms[idx];
        let postdom = &postdoms[idx];
        let fork_join_map = &fork_join_maps[idx];

        let control_output_dependencies =
            forward_dataflow(function, reverse_postorder, |inputs, id| {
                control_output_flow(inputs, id, function)
            });
        verify_dominance_relationships(
            function,
            &control_output_dependencies,
            dom,
            postdom,
            fork_join_map,
        )?;
    }

    Ok((
        def_uses,
        reverse_postorders,
        typing,
        doms,
        postdoms,
        fork_join_maps,
    ))
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
    fork_join_map: &HashMap<NodeID, NodeID>,
) -> Result<(), String> {
    // Step 1: check that forks dominate joins, and joins postdominate forks.
    for (fork, join) in fork_join_map.iter() {
        if !dom.does_dom(*fork, *join) {
            Err(format!(
                "Fork node (ID {}) doesn't dominate its corresponding join node (ID {}).",
                fork.idx(),
                join.idx()
            ))?;
        }
        if !postdom.does_dom(*join, *fork) {
            Err(format!(
                "Join node (ID {}) doesn't postdominate its corresponding fork node (ID {}).",
                join.idx(),
                fork.idx()
            ))?;
        }
    }

    // Step 2: collect nodes and corresponding control output dependencies that
    // should be checked.
    let mut to_check = vec![];
    for idx in 0..function.nodes.len() {
        // If this node is a phi node, we need to handle adding dominance checks
        // completely differently.
        if let Node::Phi { control, data } = &function.nodes[idx] {
            // Get the control predecessors of a region. This weird lambda trick
            // is to get around needing to add another nesting level just to
            // unpack the predecessor node.
            let region_preds = (|| {
                if let Node::Region { preds } = &function.nodes[control.idx()] {
                    preds
                } else {
                    panic!("A phi's control input must be a region node.")
                }
            })();

            // The inputs to a phi node don't need to dominate the phi node.
            // However, the data inputs to a phi node do need to hold proper
            // dominance relations with the corresponding control inputs to the
            // phi node's region node.
            for (control_pred, data_pred) in zip(region_preds.iter(), data.iter()) {
                to_check.push((
                    *control_pred,
                    control_output_dependencies[data_pred.idx()].clone(),
                ));
            }
        } else {
            // Having a control output dependency only matters if this node is a
            // control node, or if this node is a control output of a control node.
            // If this node is a control output, then we want to consider the
            // control node itself. We exclude the case of a phi node here, since
            // phi nodes can explicitly have non-dominating inputs. We handle phis
            // separately above.
            let this_id = if let Node::ThreadID {
                control: dominated_control,
            }
            | Node::Collect {
                control: dominated_control,
                data: _,
            } = function.nodes[idx]
            {
                dominated_control
            } else {
                NodeID::new(idx)
            };

            // If the node to be added to the to_check vector isn't even in the
            // dominator tree, don't bother. It doesn't need to be checked for
            // dominance relations.
            if !dom.is_non_root(this_id) {
                continue;
            }

            // control_output_dependencies contains the "out" values from the
            // control output dataflow analysis, while we need the "in" values.
            // This can be easily reconstructed.
            let mut dependencies = UnionNodeSet::top();
            for input in get_uses(&function.nodes[idx]).as_ref() {
                dependencies =
                    UnionNodeSet::meet(&dependencies, &control_output_dependencies[input.idx()]);
            }

            // Add dependencies to check for this node.
            to_check.push((this_id, dependencies));
        }
    }

    // Step 3: check that every node has proper dominance relations with
    // corresponding control output nodes.
    for (this_id, dependencies) in to_check {
        for pred_idx in 0..function.nodes.len() {
            if dependencies.is_set(NodeID::new(pred_idx)) {
                match function.nodes[pred_idx] {
                    // Verify that uses of phis / collect nodes are dominated
                    // by the corresponding region / join nodes, respectively.
                    Node::Phi { control, data: _ } | Node::Collect { control, data: _ } => {
                        if dom.is_non_root(this_id) && !dom.does_dom(control, this_id) {
                            Err(format!(
                                "{} node (ID {}) doesn't dominate its use (ID {}).",
                                function.nodes[pred_idx].upper_case_name(),
                                pred_idx,
                                this_id.idx()
                            ))?;
                        }
                    }
                    // Verify that uses of thread ID nodes are dominated by the
                    // corresponding fork nodes.
                    Node::ThreadID { control } => {
                        if dom.is_non_root(this_id) && !dom.does_dom(control, this_id) {
                            Err(format!(
                                "ThreadID node (ID {}) doesn't dominate its use (ID {}).",
                                pred_idx,
                                this_id.idx()
                            ))?;
                        }

                        // Every use of a thread ID must be postdominated by
                        // the thread ID's fork's corresponding join node. We
                        // don't need to check for the case where the thread ID
                        // flows through the collect node out of the fork-join,
                        // because after the collect, the thread ID is no longer
                        // considered an immediate control output use.
                        if postdom.is_non_root(this_id)
                            && !postdom.does_dom(*fork_join_map.get(&control).unwrap(), this_id)
                        {
                            Err(format!("ThreadID node's (ID {}) fork's join doesn't postdominate its use (ID {}).", pred_idx, this_id.idx()))?;
                        }
                    }
                    // If a dependency is set but depended node isn't a control
                    // output, something is wrong.
                    _ => panic!(
                        "Control output dependency is set for a node that's not a control output."
                    ),
                }
            }
        }
    }

    Ok(())
}

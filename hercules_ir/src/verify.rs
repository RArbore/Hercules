extern crate bitvec;

use std::collections::HashMap;
use std::iter::zip;

use self::bitvec::prelude::*;

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
        Vec<Subgraph>,
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

        // Calculate control output dependencies here, since they are not
        // returned by verify.
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

    // Recalculate subgraphs for return since postdominator analysis modifies
    // them.
    let subgraphs: Vec<_> = zip(module.functions.iter(), def_uses.iter())
        .map(|(function, def_use)| control_subgraph(function, def_use))
        .collect();

    Ok((
        def_uses,
        reverse_postorders,
        typing,
        subgraphs,
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
        if !node.is_start() && idx == 0 {
            Err("There must be a single start node, and its ID must be 0.")?;
        }
        let users = def_use.get_users(NodeID::new(idx));
        match node {
            // A start node must have exactly one control user. Additionally, it
            // may have many parameter, constant, or dynamic constant users.
            Node::Start => {
                if idx != 0 {
                    Err("The start node must be node ID 0.")?;
                }
                let mut found_control = false;
                for user in users {
                    match function.nodes[user.idx()] {
                        Node::Parameter { index: _ }
                        | Node::Constant { id: _ }
                        | Node::DynamicConstant { id: _ } => {}
                        _ => {
                            if function.nodes[user.idx()].is_strictly_control() {
                                if found_control {
                                    Err("A start node must have exactly one control user.")?;
                                } else {
                                    found_control = true;
                                }
                            } else {
                                Err("All users of a start node must be control, Parameter, Constant, or DynamicConstant nodes.")?;
                            }
                        }
                    }
                }
                if !found_control {
                    Err("A start node must have exactly one control user.")?;
                }
            }
            // A region node must have exactly one control user. Additionally,
            // it may have many phi users.
            Node::Region { preds: _ } => {
                let mut found_control = false;
                for user in users {
                    match function.nodes[user.idx()] {
                        Node::Phi {
                            control: _,
                            data: _,
                        } => {}
                        _ => {
                            if function.nodes[user.idx()].is_strictly_control() {
                                if found_control {
                                    Err("A region node must have exactly one control user.")?;
                                } else {
                                    found_control = true;
                                }
                            } else {
                                Err("All region of a start node must be control or Phi nodes.")?;
                            }
                        }
                    }
                }
                if !found_control {
                    Err("A region node must have exactly one control user.")?;
                }
            }
            // A fork node must have exactly one control user. Additionally,
            // it may have many thread ID users.
            Node::Fork {
                control: _,
                factor: _,
            } => {
                let mut found_control = false;
                for user in users {
                    match function.nodes[user.idx()] {
                        Node::ThreadID { control: _ } => {}
                        _ => {
                            if function.nodes[user.idx()].is_strictly_control() {
                                if found_control {
                                    Err("A fork node must have exactly one control user.")?;
                                } else {
                                    found_control = true;
                                }
                            } else {
                                Err("All users of a fork node must be control or ThreadID nodes.")?;
                            }
                        }
                    }
                }
                if !found_control {
                    Err("A fork node must have exactly one control user.")?;
                }
            }
            // A join node must have exactly one control user. Additionally,
            // it may have many reduce users.
            Node::Join { control: _ } => {
                let mut found_control = false;
                for user in users {
                    match function.nodes[user.idx()] {
                        Node::Reduce {
                            control: _,
                            init: _,
                            reduct: _,
                        } => {}
                        _ => {
                            if function.nodes[user.idx()].is_strictly_control() {
                                if found_control {
                                    Err("A join node must have exactly one control user.")?;
                                } else {
                                    found_control = true;
                                }
                            } else {
                                Err("All uses of a join node must be control or Reduce nodes.")?;
                            }
                        }
                    }
                }
                if !found_control {
                    Err("A join node must have exactly one control user.")?;
                }
            }
            // Each if node must have exactly two Read users, which
            // reference differing elements of the node's output product.
            Node::If {
                control: _,
                cond: _,
            } => {
                if users.len() != 2 {
                    Err(format!("If node must have 2 users, not {}.", users.len()))?;
                }
                if let (
                    Node::Read {
                        collect: _,
                        indices: indices1,
                    },
                    Node::Read {
                        collect: _,
                        indices: indices2,
                    },
                ) = (
                    &function.nodes[users[0].idx()],
                    &function.nodes[users[1].idx()],
                ) {
                    if indices1.len() != 1
                        || indices2.len() != 1
                        || !((indices1[0] == Index::Control(0) && indices2[0] == Index::Control(1))
                            || (indices1[0] == Index::Control(1)
                                && indices2[0] == Index::Control(0)))
                    {
                        Err("If node's user Read nodes must reference different elements of output product.")?;
                    }
                } else {
                    Err("If node's users must both be Read nodes.")?;
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
            Node::Reduce {
                control,
                init: _,
                reduct: _,
            } => {
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
            // of Read users, corresponding to the sum type being matched.
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
                        if let Node::Read {
                            collect: _,
                            ref indices,
                        } = function.nodes[user.idx()]
                        {
                            if indices.len() != 1 {
                                Err("Match node's user Read nodes must have a single index.")?;
                            }
                            let index = if let Index::Control(index) = indices[0] {
                                index
                            } else {
                                Err("Match node's user Read node must use a control index.")?
                            };
                            assert!(index < users.len(), "Read child of match node reads from bad index, but ran after typecheck succeeded.");
                            users_covered.set(index, true);
                        }
                    }
                    if users_covered.count_ones() != users.len() {
                        Err(format!("Match node's user Read nodes must reference all {} elements of match node's output product, but they only reference {} of them.", users.len(), users_covered.count_ones()))?;
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
        // completely differently. Reduce nodes need to be handled similarly.
        if let Node::Phi { control, data } = &function.nodes[idx] {
            // Get the control predecessors of a region.
            let region_preds = if let Node::Region { preds } = &function.nodes[control.idx()] {
                preds
            } else {
                panic!("A phi's control input must be a region node.")
            };

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
        } else if let Node::Reduce {
            control: _,
            init: _,
            reduct: _,
        } = &function.nodes[idx]
        {
            // TODO: Properly check dominance relations of reduce nodes.
        } else {
            // Having a control output dependency only matters if this node is a
            // control node, or if this node is a control output of a control node.
            // If this node is a control output, then we want to consider the
            // control node itself. We exclude the case of phi and reduce nodes
            // here, since these nodes can explicitly have non-dominating inputs.
            let this_id = if let Node::ThreadID {
                control: dominated_control,
            } = function.nodes[idx]
            {
                dominated_control
            } else {
                NodeID::new(idx)
            };

            // If the node to be added to the to_check vector isn't even in the
            // dominator tree, don't bother. It doesn't need to be checked for
            // dominance relations.
            if !dom.contains(this_id) {
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
                    // Verify that uses of phis / reduce nodes are dominated
                    // by the corresponding region / join nodes, respectively.
                    Node::Phi { control, data: _ }
                    | Node::Reduce {
                        control,
                        init: _,
                        reduct: _,
                    } => {
                        if dom.contains(this_id) && !dom.does_dom(control, this_id) {
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
                        if dom.contains(this_id) && !dom.does_dom(control, this_id) {
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
                        if postdom.contains(this_id)
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

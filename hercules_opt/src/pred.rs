extern crate bitvec;
extern crate hercules_ir;

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

use self::bitvec::prelude::*;

use self::hercules_ir::def_use::*;
use self::hercules_ir::dom::*;
use self::hercules_ir::ir::*;
use self::hercules_ir::schedule::*;

/*
 * Top level function to convert acyclic control flow in vectorized fork-joins
 * into predicated data flow.
 */
pub fn predication(
    function: &mut Function,
    def_use: &ImmutableDefUseMap,
    reverse_postorder: &Vec<NodeID>,
    dom: &DomTree,
    fork_join_map: &HashMap<NodeID, NodeID>,
    schedules: &Vec<Vec<Schedule>>,
) {
    // Detect forks with vectorize schedules.
    let vector_forks: Vec<_> = function
        .nodes
        .iter()
        .enumerate()
        //.filter(|(idx, n)| n.is_fork() && schedules[*idx].contains(&Schedule::Vectorize))
        .filter(|(_, n)| n.is_fork())
        .map(|(idx, _)| NodeID::new(idx))
        .collect();

    // Filter forks that can't actually be vectorized, and yell at the user if
    // they're being silly.
    let actual_vector_forks: Vec<_> = vector_forks
        .into_iter()
        .filter_map(|fork_id| {
            // Detect cycles in control flow between fork and join. Start at the
            // join, and work backwards.
            let mut visited = bitvec![u8, Lsb0; 0; function.nodes.len()];
            let join_id = fork_join_map[&fork_id];
            let mut stack = vec![join_id];
            while let Some(pop) = stack.pop() {
                // Only detect cycles between fork and join, and don't revisit
                // nodes.
                if visited[pop.idx()] || function.nodes[pop.idx()].is_fork() {
                    continue;
                }

                // Filter if there is a cycle, or if there is a nested fork, or
                // if there is a match node. We know there is a loop if a node
                // dominates one of its predecessors.
                let control_uses: Vec<_> = get_uses(&function.nodes[pop.idx()])
                    .as_ref()
                    .iter()
                    .filter(|id| function.nodes[id.idx()].is_control())
                    .map(|x| *x)
                    .collect();
                if control_uses
                    .iter()
                    .any(|pred_id| dom.does_dom(pop, *pred_id))
                    || (function.nodes[pop.idx()].is_join() && pop != join_id)
                    || function.nodes[pop.idx()].is_match()
                {
                    eprintln!(
                        "WARNING: Vectorize schedule attached to fork that cannot be vectorized."
                    );
                    return None;
                }

                // Recurse up the control subgraph.
                visited.set(pop.idx(), true);
                stack.extend(control_uses);
            }

            Some((fork_id, visited))
        })
        .collect();

    // For each control node, collect which condition values must be true, and
    // which condition values must be false to reach that node. Each phi's
    // corresponding region will have at least one condition value that differs
    // between the predecessors. These differing condition values anded together
    // form the select condition.
    let mut condition_valuations: HashMap<NodeID, (HashSet<NodeID>, HashSet<NodeID>)> =
        HashMap::new();
    for (fork_id, control_in_fork_join) in actual_vector_forks.iter() {
        // Within a fork-join, there are no condition requirements on the fork.
        condition_valuations.insert(*fork_id, (HashSet::new(), HashSet::new()));

        // Iterate the nodes in the fork-join in reverse postorder, top-down.
        let local_reverse_postorder = reverse_postorder
            .iter()
            .filter(|id| control_in_fork_join[id.idx()]);
        for control_id in local_reverse_postorder {
            match function.nodes[control_id.idx()] {
                Node::If { control, cond: _ } | Node::Join { control } => {
                    condition_valuations
                        .insert(*control_id, condition_valuations[&control].clone());
                }
                // Introduce condition variables into sets, as this is where
                // branching occurs.
                Node::Read {
                    collect,
                    ref indices,
                } => {
                    assert_eq!(indices.len(), 1);
                    let truth_value = indices[0].try_control().unwrap();
                    assert!(truth_value < 2);
                    let mut sets = condition_valuations[&collect].clone();
                    let condition = function.nodes[collect.idx()].try_if().unwrap().1;
                    if truth_value == 0 {
                        sets.0.insert(condition);
                    } else {
                        sets.1.insert(condition);
                    }
                    condition_valuations.insert(*control_id, sets);
                }
                // The only required conditions for a region are those required
                // for all predecessors. Thus, the condition sets for a region
                // are the intersections of the predecessor condition sets.
                Node::Region { ref preds } => {
                    let (prev_true_set, prev_false_set) = condition_valuations[&preds[0]].clone();
                    let int_true_set = preds[1..].iter().fold(prev_true_set, |a, b| {
                        a.intersection(&condition_valuations[b].0)
                            .map(|x| *x)
                            .collect::<HashSet<NodeID>>()
                    });
                    let int_false_set = preds[1..].iter().fold(prev_false_set, |a, b| {
                        a.intersection(&condition_valuations[b].0)
                            .map(|x| *x)
                            .collect::<HashSet<NodeID>>()
                    });

                    condition_valuations.insert(*control_id, (int_true_set, int_false_set));
                }
                _ => {
                    panic!()
                }
            }
        }
    }

    // Convert control flow to predicated data flow.
    for (fork_id, control_in_fork_join) in actual_vector_forks.into_iter() {
        // Worklist of control nodes - traverse control backwards breadth-first.
        let mut queue = VecDeque::new();
        let mut visited = bitvec![u8, Lsb0; 0; function.nodes.len()];
        let join_id = fork_join_map[&fork_id];
        queue.push_back(join_id);

        while let Some(pop) = queue.pop_front() {
            // Stop at forks, and don't revisit nodes.
            if visited[pop.idx()] || function.nodes[pop.idx()].is_fork() {
                continue;
            }

            // The only type of node we need to handle at this point are region
            // nodes. Region nodes are what have phi users, and those phis are
            // what need to get converted to select nodes.
            if let Node::Region { preds } = &function.nodes[pop.idx()] {
                // Get the unique true and false conditions per predecessor.
                // These are the conditions attached to the predecessor that
                // aren't attached to this region.
                assert_eq!(preds.len(), 2);
                let (region_true_conds, region_false_conds) = &condition_valuations[&pop];
                let unique_conditions = preds
                    .iter()
                    .map(|pred_id| {
                        let (pred_true_conds, pred_false_conds) = &condition_valuations[pred_id];
                        (
                            pred_true_conds
                                .iter()
                                .filter(|cond_id| !region_true_conds.contains(cond_id))
                                .map(|x| *x)
                                .collect::<HashSet<NodeID>>(),
                            pred_false_conds
                                .iter()
                                .filter(|cond_id| !region_false_conds.contains(cond_id))
                                .map(|x| *x)
                                .collect::<HashSet<NodeID>>(),
                        )
                    })
                    .collect::<Vec<_>>();

                // Currently, we only handle if branching. The unique conditions
                // for a region's predecessors must be exact inverses of each
                // other. Given this is true, we just use unique_conditions[0]
                // to calculate the select condition.
                assert_eq!(unique_conditions[0].0, unique_conditions[1].1);
                assert_eq!(unique_conditions[0].1, unique_conditions[1].0);
                let negated_conditions = unique_conditions[0]
                    .1
                    .iter()
                    .map(|cond_id| {
                        let id = NodeID::new(function.nodes.len());
                        function.nodes.push(Node::Unary {
                            input: *cond_id,
                            op: UnaryOperator::Not,
                        });
                        id
                    })
                    .collect::<Vec<NodeID>>();
                let mut all_conditions = unique_conditions[0]
                    .0
                    .iter()
                    .map(|x| *x)
                    .chain(negated_conditions.into_iter());

                // And together the negated negative and position conditions.
                let first_cond = all_conditions.next().unwrap();
                let reduced_cond = all_conditions.into_iter().fold(first_cond, |a, b| {
                    let id = NodeID::new(function.nodes.len());
                    function.nodes.push(Node::Binary {
                        left: a,
                        right: b,
                        op: BinaryOperator::And,
                    });
                    id
                });

                // Create the select nodes, corresponding to all phi users.
                for phi in def_use.get_users(pop) {
                    if let Node::Phi { control: _, data } = &function.nodes[phi.idx()] {
                        let select_id = NodeID::new(function.nodes.len());
                        function.nodes.push(Node::Ternary {
                            first: reduced_cond,
                            second: data[1],
                            third: data[0],
                            op: TernaryOperator::Select,
                        });
                        for user in def_use.get_users(*phi) {
                            get_uses_mut(&mut function.nodes[user.idx()]).map(*phi, select_id);
                        }
                        function.nodes[phi.idx()] = Node::Start;
                    }
                }
            }

            // Add users of this control node to queue.
            visited.set(pop.idx(), true);
            queue.extend(
                get_uses(&function.nodes[pop.idx()])
                    .as_ref()
                    .iter()
                    .filter(|id| function.nodes[id.idx()].is_control() && !visited[id.idx()]),
            );
        }

        // Now that we've converted all the phis to selects, delete all the
        // control nodes.
        for control_idx in control_in_fork_join.iter_ones() {
            if let Node::Join { control } = function.nodes[control_idx] {
                get_uses_mut(&mut function.nodes[control_idx]).map(control, fork_id);
            } else {
                function.nodes[control_idx] = Node::Start;
            }
        }
    }
}

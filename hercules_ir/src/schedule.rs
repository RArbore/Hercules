use std::collections::HashMap;

use crate::*;

/*
 * An individual schedule is a single "directive" for the compiler to take into
 * consideration at some point during the compilation pipeline. Each schedule is
 * associated with a single node.
 */
#[derive(Debug, Clone)]
pub enum Schedule {
    ParallelReduce,
}

/*
 * A plan is a set of schedules associated with each node, plus partitioning
 * information. Partitioning information is a mapping from node ID to partition
 * ID. A plan's scope is a single function.
 */
#[derive(Debug, Clone)]
pub struct Plan {
    schedules: Vec<Vec<Schedule>>,
    partitions: Vec<PartitionID>,
    num_partitions: usize,
}

define_id_type!(PartitionID);

/*
 * A "default" plan should be available, where few schedules are used and
 * conservative partitioning is enacted. Only schedules that can be proven safe
 * by the compiler should be included.
 */
pub fn default_plan(function: &Function, fork_join_map: &HashMap<NodeID, NodeID>) -> Plan {
    // Step 1: create a completely bare-bones plan doing nothing interesting.
    let mut plan = Plan {
        schedules: vec![vec![]; function.nodes.len()],
        partitions: vec![PartitionID::new(0); function.nodes.len()],
        num_partitions: 0,
    };

    // Step 2: infer parallel reductions consisting of a simple loop between a
    // Reduce node and a Write node, where an index of the Write is a position
    // index using the ThreadID node attached to the corresponding Fork. This
    // procedure also adds the ParallelReduce schedule to Reduce nodes reducing
    // over a parallelized Reduce, as long as the base Write node also has a
    // position index that is the ThreadID of the outer fork. In other words,
    // the complete Reduce chain is annotated with ParallelReduce, as long as
    // each ThreadID appears in the positional indexing of the Write.
    for id in (0..function.nodes.len())
        .map(NodeID::new)
        .filter(|id| function.nodes[id.idx()].is_reduce())
    {
        let mut first_control = None;
        let mut last_reduce = id;
        let mut chain_id = id;

        // Walk down Reduce chain until we reach the Reduce potentially looping
        // with the Write. Note the control node of the first Reduce, since this
        // will tell us which Thread ID to look for in the Write.
        while let Node::Reduce {
            control,
            init: _,
            reduct,
        } = function.nodes[id.idx()]
        {
            if first_control.is_none() {
                first_control = Some(control);
            }

            last_reduce = chain_id;
            chain_id = reduct;
        }

        // Check for a Write-Reduce tight cycle.
        if let Node::Write {
            collect,
            data: _,
            indices,
        } = &function.nodes[chain_id.idx()]
            && *collect == last_reduce
        {
            // If there is a Write-Reduce tight cycle, get the position indices.
            let positions = indices
                .iter()
                .filter_map(|index| {
                    if let Index::Position(indices) = index {
                        Some(indices)
                    } else {
                        None
                    }
                })
                .flat_map(|pos| pos.iter());

            // Get the Forks corresponding to uses of bare ThreadIDs.
            let mut forks = positions.filter_map(|id| {
                if let Node::ThreadID { control } = function.nodes[id.idx()] {
                    Some(control)
                } else {
                    None
                }
            });

            // Check if any of the Forks correspond to the Join associated with
            // the Reduce being considered.
            let is_parallel = forks.any(|id| fork_join_map[&id] == first_control.unwrap());

            if is_parallel {
                plan.schedules[id.idx()].push(Schedule::ParallelReduce);
            }
        }
    }

    plan
}

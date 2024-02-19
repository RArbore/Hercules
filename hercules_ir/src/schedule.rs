use std::collections::HashMap;
use std::iter::zip;

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
pub fn default_plan(
    function: &Function,
    reverse_postorder: &Vec<NodeID>,
    fork_join_map: &HashMap<NodeID, NodeID>,
    bbs: &Vec<NodeID>,
) -> Plan {
    // Start by creating a completely bare-bones plan doing nothing interesting.
    let mut plan = Plan {
        schedules: vec![vec![]; function.nodes.len()],
        partitions: vec![PartitionID::new(0); function.nodes.len()],
        num_partitions: 0,
    };

    // Infer schedules.
    infer_parallel_reduce(function, fork_join_map, &mut plan);

    // Infer a partitioning.
    partition_out_forks(function, reverse_postorder, fork_join_map, bbs, &mut plan);

    plan
}

/*
 * Infer parallel reductions consisting of a simple cycle between a Reduce node
 * and a Write node, where an index of the Write is a position index using the
 * ThreadID node attached to the corresponding Fork. This procedure also adds
 * the ParallelReduce schedule to Reduce nodes reducing over a parallelized
 * Reduce, as long as the base Write node also has a position index that is the
 * ThreadID of the outer fork. In other words, the complete Reduce chain is
 * annotated with ParallelReduce, as long as each ThreadID appears in the
 * positional indexing of the Write.
 */
pub fn infer_parallel_reduce(
    function: &Function,
    fork_join_map: &HashMap<NodeID, NodeID>,
    plan: &mut Plan,
) {
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
        } = function.nodes[chain_id.idx()]
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
}

/*
 * Create partitions corresponding to fork-join nests. Also, split the "top-
 * level" partition into sub-partitions that are connected graphs. Place data
 * nodes using ThreadID or Reduce nodes in the corresponding fork-join nest's
 * partition.
 */
pub fn partition_out_forks(
    function: &Function,
    reverse_postorder: &Vec<NodeID>,
    fork_join_map: &HashMap<NodeID, NodeID>,
    bbs: &Vec<NodeID>,
    plan: &mut Plan,
) {
    impl Semilattice for NodeID {
        fn meet(a: &Self, b: &Self) -> Self {
            if a.idx() < b.idx() {
                *a
            } else {
                *b
            }
        }

        fn bottom() -> Self {
            NodeID::new(0)
        }

        fn top() -> Self {
            NodeID::new(!0)
        }
    }

    // Step 1: do dataflow analysis over control nodes to identify a
    // representative node for each partition. Each fork not taking as input the
    // ID of another fork node introduces its own ID as a partition
    // representative. Each join node propagates the fork ID if it's not the
    // fork pointing to the join in the fork join map - otherwise, it introduces
    // its user as a representative node ID for a partition. A region node taking
    // multiple node IDs as input belongs to the partition with the smaller
    // representative node ID.
    let mut representatives = forward_dataflow(
        function,
        reverse_postorder,
        |inputs: &[&NodeID], node_id: NodeID| match function.nodes[node_id.idx()] {
            Node::Start => NodeID::new(0),
            Node::Fork {
                control: _,
                factor: _,
            } => {
                // Start a partition if the preceding partition isn't a fork
                // partition. Otherwise, be part of the parent fork partition.
                if *inputs[0] != NodeID::top() && function.nodes[inputs[0].idx()].is_fork() {
                    inputs[0].clone()
                } else {
                    node_id
                }
            }
            Node::Join { control: _ } => inputs[0].clone(),
            _ => {
                // If the previous node is a join and terminates a fork's
                // partition, then start a new partition here. Otherwise, just
                // meet over the input lattice values. Set all data nodes to be
                // in the !0 partition.
                if !function.nodes[node_id.idx()].is_control() {
                    NodeID::top()
                } else if zip(inputs, get_uses(&function.nodes[node_id.idx()]).as_ref())
                    .any(|(part_id, pred_id)| fork_join_map.get(part_id) == Some(pred_id))
                {
                    node_id
                } else {
                    inputs
                        .iter()
                        .filter(|id| {
                            ***id != NodeID::top() && function.nodes[id.idx()].is_control()
                        })
                        .fold(NodeID::top(), |a, b| NodeID::meet(&a, b))
                }
            }
        },
    );

    // Step 2: assign data nodes to the partitions of the control nodes they are
    // assigned to by GCM.
    for idx in 0..function.nodes.len() {
        if !function.nodes[idx].is_control() {
            representatives[idx] = representatives[bbs[idx].idx()];
        }
    }

    // Step 3: deduplicate representative node IDs.
    let mut representative_to_partition_ids = HashMap::new();
    for rep in &representatives {
        if !representative_to_partition_ids.contains_key(rep) {
            representative_to_partition_ids
                .insert(rep, PartitionID::new(representative_to_partition_ids.len()));
        }
    }

    // Step 4: update plan.
    plan.num_partitions = representative_to_partition_ids.len();
    for id in (0..function.nodes.len()).map(NodeID::new) {
        plan.partitions[id.idx()] = representative_to_partition_ids[&representatives[id.idx()]];
    }
}

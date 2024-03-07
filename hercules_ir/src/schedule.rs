use std::collections::HashMap;
use std::collections::VecDeque;
use std::iter::zip;

use crate::*;

/*
 * An individual schedule is a single "directive" for the compiler to take into
 * consideration at some point during the compilation pipeline. Each schedule is
 * associated with a single node.
 */
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Schedule {
    ParallelReduce,
    Vectorize,
}

/*
 * The authoritative enumeration of supported devices. Technically, a device
 * refers to a specific backend, so difference "devices" may refer to the same
 * "kind" of hardware.
 */
#[derive(Debug, Clone)]
pub enum Device {
    CPU,
    GPU,
}

/*
 * A plan is a set of schedules associated with each node, plus partitioning
 * information. Partitioning information is a mapping from node ID to partition
 * ID. A plan's scope is a single function.
 */
#[derive(Debug, Clone)]
pub struct Plan {
    pub schedules: Vec<Vec<Schedule>>,
    pub partitions: Vec<PartitionID>,
    pub partition_devices: Vec<Device>,
    pub num_partitions: usize,
}

define_id_type!(PartitionID);

impl Plan {
    /*
     * Invert stored map from node to partition to map from partition to nodes.
     */
    pub fn invert_partition_map(&self) -> Vec<Vec<NodeID>> {
        let mut map = vec![vec![]; self.num_partitions];

        for idx in 0..self.partitions.len() {
            map[self.partitions[idx].idx()].push(NodeID::new(idx));
        }

        map
    }

    /*
     * Plans must be "repairable", in the sense that the IR that's referred to
     * may change after many passes. Since a plan is an explicit side data
     * structure, it must be updated after every change in the IR.
     */
    pub fn repair(self, function: &Function, grave_mapping: &Vec<NodeID>) -> Self {
        // Unpack the plan.
        let old_inverse_partition_map = self.invert_partition_map();
        let Plan {
            mut schedules,
            partitions: _,
            partition_devices,
            num_partitions: _,
        } = self;

        // Schedules of old nodes just get dropped. Since schedules don't hold
        // necessary semantic information, we are free to drop them arbitrarily.
        schedules = schedules.map_gravestones(grave_mapping);
        schedules.resize(function.nodes.len(), vec![]);

        // Delete now empty partitions. First, filter out deleted nodes from the
        // partitions and simultaneously map old node IDs to new node IDs. Then,
        // filter out empty partitions.
        let (new_inverse_partition_map, new_devices): (Vec<Vec<NodeID>>, Vec<Device>) =
            zip(old_inverse_partition_map, partition_devices)
                .into_iter()
                .map(|(contents, device)| {
                    (
                        contents
                            .into_iter()
                            .filter_map(|id| {
                                if id.idx() == 0 || grave_mapping[id.idx()].idx() != 0 {
                                    Some(grave_mapping[id.idx()])
                                } else {
                                    None
                                }
                            })
                            .collect::<Vec<NodeID>>(),
                        device,
                    )
                })
                .filter(|(contents, _)| !contents.is_empty())
                .unzip();

        // Calculate the number of nodes after deletion but before addition. Use
        // this is iterate new nodes later.
        let num_nodes_before_addition = new_inverse_partition_map.iter().flatten().count();
        assert!(new_inverse_partition_map
            .iter()
            .flatten()
            .all(|id| id.idx() < num_nodes_before_addition));

        // Calculate the nodes that need to be assigned to a partition. This
        // starts as just the nodes that have been added by passes.
        let mut new_node_ids: VecDeque<NodeID> = (num_nodes_before_addition..function.nodes.len())
            .map(NodeID::new)
            .collect();

        // Any partition no longer containing at least one control node needs to
        // be liquidated.
        let (new_inverse_partition_map, new_devices): (Vec<Vec<NodeID>>, Vec<Device>) =
            zip(new_inverse_partition_map, new_devices)
                .into_iter()
                .filter_map(|(part, device)| {
                    if part.iter().any(|id| function.nodes[id.idx()].is_control()) {
                        Some((part, device))
                    } else {
                        // Nodes in removed partitions need to be re-partitioned.
                        new_node_ids.extend(part);
                        None
                    }
                })
                .unzip();

        // Assign the node IDs that need to be partitioned to partitions. In the
        // process, construct a map from node ID to partition ID.
        let mut node_id_to_partition_id: HashMap<NodeID, PartitionID> = new_inverse_partition_map
            .into_iter()
            .enumerate()
            .map(|(partition_idx, node_ids)| {
                node_ids
                    .into_iter()
                    .map(|node_id| (node_id, PartitionID::new(partition_idx)))
                    .collect::<Vec<(NodeID, PartitionID)>>()
            })
            .flatten()
            .collect();

        // Make a best effort to assign nodes to the partition of one of their
        // uses. Prioritize earlier uses. TODO: since not all partitions are
        // legal, this is almost certainly not complete. Think more about that.
        'workloop: while let Some(id) = new_node_ids.pop_front() {
            for u in get_uses(&function.nodes[id.idx()]).as_ref() {
                if let Some(partition_id) = node_id_to_partition_id.get(u) {
                    node_id_to_partition_id.insert(id, *partition_id);
                    continue 'workloop;
                }
            }
            new_node_ids.push_back(id);
        }

        // Reconstruct the partitions vector.
        let num_partitions = new_devices.len();
        let mut partitions = vec![PartitionID::new(0); function.nodes.len()];
        for (k, v) in node_id_to_partition_id {
            partitions[k.idx()] = v;
        }

        // Reconstruct the whole plan.
        Plan {
            schedules,
            partitions,
            partition_devices: new_devices,
            num_partitions,
        }
    }
}

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
        partition_devices: vec![Device::CPU; 1],
        num_partitions: 0,
    };

    // Infer schedules.
    infer_parallel_reduce(function, fork_join_map, &mut plan);
    infer_vectorize(function, fork_join_map, &mut plan);

    // Infer a partitioning.
    partition_out_forks(function, reverse_postorder, fork_join_map, bbs, &mut plan);
    place_fork_partitions_on_gpu(function, &mut plan);

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
 * Infer vectorizable fork-joins. Just check that there are no control nodes
 * between a fork and its join.
 */
pub fn infer_vectorize(
    function: &Function,
    fork_join_map: &HashMap<NodeID, NodeID>,
    plan: &mut Plan,
) {
    for id in (0..function.nodes.len())
        .map(NodeID::new)
        .filter(|id| function.nodes[id.idx()].is_join())
    {
        let u = get_uses(&function.nodes[id.idx()]).as_ref()[0];
        if let Some(join) = fork_join_map.get(&u)
            && *join == id
        {
            plan.schedules[u.idx()].push(Schedule::Vectorize);
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

    plan.partition_devices = vec![Device::CPU; plan.num_partitions];
}

/*
 * Set the device for all partitions containing a fork to the GPU.
 */
pub fn place_fork_partitions_on_gpu(function: &Function, plan: &mut Plan) {
    for idx in 0..function.nodes.len() {
        if function.nodes[idx].is_fork() {
            plan.partition_devices[plan.partitions[idx].idx()] = Device::GPU;
        }
    }
}

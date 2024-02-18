use crate::*;

/*
 * An individual schedule is a single "directive" for the compiler to take into
 * consideration at some point during the compilation pipeline. Each schedule is
 * associated with a single node.
 */
#[derive(Debug, Clone)]
pub enum Schedule {}

/*
 * A plan is a set of schedules associated with each node, plus partitioning
 * information. Partitioning information is a mapping from node ID to partition
 * ID.
 */
#[derive(Debug, Clone)]
pub struct Plan {
    schedules: Vec<Vec<Schedule>>,
    partitions: Vec<PartitionID>,
    num_partitions: usize,
}

define_id_type!(PartitionID);

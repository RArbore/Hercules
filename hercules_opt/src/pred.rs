extern crate bitvec;
extern crate hercules_ir;

use std::collections::HashMap;

use self::bitvec::prelude::*;

use self::hercules_ir::def_use::*;
use self::hercules_ir::ir::*;
use self::hercules_ir::schedule::*;

/*
 * Top level function to convert acyclic control flow in vectorized fork-joins
 * into predicated data flow.
 */
pub fn predication(
    function: &mut Function,
    fork_join_map: &HashMap<NodeID, NodeID>,
    schedules: &Vec<Vec<Schedule>>,
) {
    // Detect forks with vectorize schedules.
    let vector_forks: Vec<_> = function
        .nodes
        .iter()
        .enumerate()
        .filter(|(_, n)| n.is_fork()) // && schedules[*idx].contains(&Schedule::Vectorize))
        .map(|(idx, _)| NodeID::new(idx))
        .collect();

    // Filter forks that can't actually be vectorized, and yell at the user if
    // they're being silly.
    let actual_vector_forks: Vec<_> = vector_forks
        .into_iter()
        .filter(|fork_id| {
            // Detect cycles in control flow between fork and join. Start at the
            // join, and work backwards.
            let mut visited = bitvec![u8, Lsb0; 0; function.nodes.len()];
            let join_id = fork_join_map[fork_id];
            let mut stack = vec![join_id];
            while let Some(pop) = stack.pop() {
                // Only detect cycles between fork and join.
                if function.nodes[pop.idx()].is_fork() {
                    continue;
                }

                // Filter if there is a cycle or if there is a nested fork.
                if visited[pop.idx()] || (function.nodes[pop.idx()].is_join() && pop != join_id) {
                    eprintln!(
                        "WARNING: Vectorize schedule attached to fork that cannot be vectorized."
                    );
                    return false;
                }

                // Recurse up the control subgraph.
                visited.set(pop.idx(), true);
                stack.extend(
                    get_uses(&function.nodes[pop.idx()])
                        .as_ref()
                        .iter()
                        .filter(|id| function.nodes[id.idx()].is_control()),
                )
            }

            true
        })
        .collect();

    println!("{:?}", actual_vector_forks);
}

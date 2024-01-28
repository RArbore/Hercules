extern crate hercules_ir;

use self::hercules_ir::def_use::*;
use self::hercules_ir::ir::*;

/*
 * Top level function to assemble anti-dependence edges. Returns a list of pairs
 * of nodes. The first item in the pair is the read node, and the second item is
 * the write node.
 */
pub fn antideps(function: &Function, def_use: &ImmutableDefUseMap) -> Vec<(NodeID, NodeID)> {
    // Collection values are not directly computed on. Thus, there are actually
    // very few nodes that have collection inputs or output. As a result, when
    // forming anti-dependencies for a single collection, we only need to
    // consider immediate users that are read or write nodes - no proper
    // dataflow analysis necessary.
    let mut antideps = vec![];

    for id in (0..function.nodes.len()).map(NodeID::new) {
        // Collect the reads and writes to / from this collection.
        let users = def_use.get_users(id);
        let reads = users.iter().filter(|user| {
            if let Node::Read {
                collect,
                indices: _,
            } = function.nodes[user.idx()]
            {
                collect == id
            } else {
                false
            }
        });
        let mut writes = users.iter().filter(|user| {
            if let Node::Write {
                collect,
                data: _,
                indices: _,
            } = function.nodes[user.idx()]
            {
                collect == id
            } else {
                false
            }
        });

        // If there are any writes, compute the anti dependence edges.
        if let Some(write) = writes.next() {
            for read in reads {
                antideps.push((*read, *write));
            }
        }

        // TODO: Multiple write uses should clone to collection for N - 1 of the writes.
        assert!(writes.next() == None, "Can't form anti-dependencies when there are two independent writes depending on a single collection value.");
    }

    antideps
}

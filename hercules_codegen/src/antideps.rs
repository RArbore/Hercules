extern crate hercules_ir;

use self::hercules_ir::def_use::*;
use self::hercules_ir::ir::*;

/*
 * Top level function to assemble anti-dependence edges. Returns a list of pairs
 * of nodes. The first item in the pair is the read node, and the second item is
 * the write node.
 */
pub fn antideps<I: Iterator<Item = NodeID>>(
    function: &Function,
    def_use: &ImmutableDefUseMap,
    nodes: I,
) -> Vec<(NodeID, NodeID)> {
    // Anti-dependence edges are between a write node and a read node, where
    // each node uses the same array value. The read must be scheduled before
    // the write to avoid incorrect compilation.
    let mut antideps = vec![];

    for id in nodes {
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

        // TODO: Multiple write uses should clone the collection for N - 1 of the writes.
        assert!(writes.next() == None, "Can't form anti-dependencies when there are two independent writes depending on a single collection value.");
    }

    antideps
}

/*
 * Sometimes, we are only interested in anti-dependence edges involving arrays.
 */
pub fn array_antideps(
    function: &Function,
    def_use: &ImmutableDefUseMap,
    types: &Vec<Type>,
    typing: &Vec<TypeID>,
) -> Vec<(NodeID, NodeID)> {
    antideps(
        function,
        def_use,
        (0..function.nodes.len())
            .map(NodeID::new)
            .filter(|id| types[typing[id.idx()].idx()].is_array()),
    )
}

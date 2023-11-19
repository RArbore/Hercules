extern crate hercules_ir;

use self::hercules_ir::def_use::*;
use self::hercules_ir::ir::*;

/*
 * Top level function to assemble anti-dependence edges. Returns a list of pairs
 * of nodes. The first item in the pair is the read node, and the second item is
 * the write node.
 */
pub fn antideps(function: &Function, def_use: &ImmutableDefUseMap) -> Vec<(NodeID, NodeID)> {
    // Array and product typed values are not directly computed on. Thus, there
    // are actually very few nodes that have array or product inputs or output.
    // As a result, when forming anti-dependencies for a single allocation, we
    // only need to consider immediate users that are read or write nodes - no
    // proper dataflow analysis necessary.
    let mut antideps = vec![];

    for id in (0..function.nodes.len()).map(NodeID::new) {
        let users = def_use.get_users(id);

        // First, handle product reads and writes.
        let reads = users.iter().filter(|user| {
            if let Node::ReadProd { prod, index: _ } = function.nodes[user.idx()] {
                prod == id
            } else {
                false
            }
        });
        let mut writes = users.iter().filter(|user| {
            if let Node::WriteProd {
                prod,
                index: _,
                data: _,
            } = function.nodes[user.idx()]
            {
                prod == id
            } else {
                false
            }
        });
        if let Some(write) = writes.next() {
            for read in reads {
                antideps.push((*read, *write));
            }
        }
        assert!(writes.next() == None, "Can't form anti-dependencies when there are two independent writes depending on a single product value.");

        // Second, handle array reads and writes.
        let reads = users.iter().filter(|user| {
            if let Node::ReadArray { array, index: _ } = function.nodes[user.idx()] {
                array == id
            } else {
                false
            }
        });
        let mut writes = users.iter().filter(|user| {
            if let Node::WriteArray {
                array,
                index: _,
                data: _,
            } = function.nodes[user.idx()]
            {
                array == id
            } else {
                false
            }
        });
        if let Some(write) = writes.next() {
            for read in reads {
                antideps.push((*read, *write));
            }
        }
        assert!(writes.next() == None, "Can't form anti-dependencies when there are two independent writes depending on a single array value.");
    }

    antideps
}

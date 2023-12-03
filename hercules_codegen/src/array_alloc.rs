extern crate ena;
extern crate hercules_ir;

use std::collections::HashMap;

use self::ena::unify::*;

use self::hercules_ir::def_use::*;
use self::hercules_ir::ir::*;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
struct UnitKey(u32);

impl UnifyKey for UnitKey {
    type Value = ();

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> UnitKey {
        UnitKey(u)
    }

    fn tag() -> &'static str {
        "UnitKey"
    }
}

/*
 * Top level function to allocate individual arrays for sets of nodes in the IR
 * graph. Doesn't attempt to overlap allocations w/ liveness analysis. Returns
 * a set of nodes per allocation, which are the nodes that use that allocation,
 * along with dimensions specified with dynamic constants.
 */
pub fn logical_array_alloc(
    function: &Function,
    typing: &Vec<TypeID>,
    types: &Vec<Type>,
    fork_join_map: &HashMap<NodeID, NodeID>,
    bbs: &Vec<NodeID>,
    fork_join_nests: &HashMap<NodeID, Vec<NodeID>>,
) -> () {
    // Step 1: filter nodes that operate on arrays, either on their input or
    // their output.
    let id_outputs_array = |id: &NodeID| types[typing[id.idx()].idx()].is_array();
    let array_nodes: Vec<_> = (0..function.nodes.len())
        .map(NodeID::new)
        .filter(|id| {
            id_outputs_array(id)
                || get_uses(&function.nodes[id.idx()])
                    .as_ref()
                    .into_iter()
                    .any(id_outputs_array)
        })
        .collect();
    let array_node_numbers: HashMap<_, _> =
        std::iter::zip(array_nodes.iter().map(|x| *x), 0..array_nodes.len()).collect();

    // Step 2: union find the nodes based on use edges. Every node in each set
    // should use the same array allocation. The representative node for a set
    // will be the node with the smallest ID.
    let mut allocs: UnificationTable<InPlace<UnitKey>> = UnificationTable::new();
    let keys: Vec<_> = (0..array_nodes.len()).map(|_| allocs.new_key(())).collect();
    for node in array_nodes.iter() {
        for array_use in get_uses(&function.nodes[node.idx()])
            .as_ref()
            .into_iter()
            .map(|x| *x)
            .filter(id_outputs_array)
        {
            allocs.union(
                keys[array_node_numbers[&node]],
                keys[array_node_numbers[&array_use]],
            );
        }
    }

    for key in keys {
        println!(
            "{:?}, {:?}",
            array_nodes[key.index() as usize],
            array_nodes[allocs.find(key).index() as usize]
        );
    }
}

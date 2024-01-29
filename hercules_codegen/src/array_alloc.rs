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
 * graph. Doesn't attempt to overlap allocations w/ liveness analysis. Returns a
 * list of allocations, characterized by their size in terms of dynamic
 * constants, and a map from node IDs involving an array value to the allocation
 * it operates on.
 */
pub fn logical_array_alloc(
    function: &Function,
    typing: &Vec<TypeID>,
    types: &Vec<Type>,
) -> (Vec<Vec<DynamicConstantID>>, HashMap<NodeID, usize>) {
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
    // should use the same array allocation.
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

    // Step 3: determine the size of each array allocation. This is the size of
    // the array type operated on.
    let mut key_to_value_size: HashMap<UnitKey, Vec<DynamicConstantID>> = HashMap::new();
    for key in keys.iter() {
        let value_key = allocs.find(*key);
        let id = array_nodes[key.index() as usize];

        let extents = if let Some(extents) = types[typing[id.idx()].idx()].try_extents() {
            extents.iter().map(|x| *x).collect()
        } else {
            continue;
        };

        // This loop iterates over all nodes that interact with array values.
        // Thus, many iterations will unnecessarily compute the extents of the
        // same array extent. This is a good opportunity to check our work and
        // make sure all the extents are consistent.
        if let Some(old_extents) = key_to_value_size.get(&value_key) {
            assert!(extents == *old_extents);
        } else {
            key_to_value_size.insert(value_key, extents);
        }
    }

    // Step 4: collect array allocations as a numbered list. Map from array
    // nodes to the array allocation number they use.
    let mut logical_allocations: Vec<Vec<DynamicConstantID>> = vec![];
    let mut key_to_number: HashMap<UnitKey, usize> = HashMap::new();
    for (key, array_value) in key_to_value_size {
        key_to_number.insert(key, logical_allocations.len());
        logical_allocations.push(array_value);
    }
    let mut node_to_logical_numbers: HashMap<NodeID, usize> = HashMap::new();
    for node in array_nodes.iter() {
        node_to_logical_numbers.insert(
            *node,
            key_to_number[&allocs.find(keys[array_node_numbers[node]])],
        );
    }

    (logical_allocations, node_to_logical_numbers)
}

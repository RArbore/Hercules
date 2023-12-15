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

    // Step 3: determine the size of each array allocation. This is the size of
    // the array type operated on, possibly in addition to dynamic constant
    // factors corresponding to uses inside fork / joins. Each node that can
    // operate on array values, and their corresponding affect on the array
    // value's size, are listed below.
    //
    // Phi: only provides the base array dimensions
    // Collect: provides the base array dimensions, in addition to dimensions
    // corresponding to dominating fork / join nests
    // Return: provides no array dimensions
    // Parameter: only provides the base array dimensions
    // Constant: only provides the base array dimensions
    // Call: TODO
    // ReadArray: only provides the base array dimensions
    // WriteArray: provides the base array dimensions, in addition to dimensions
    // corresponding to each fork / join the node is nested in
    let mut key_to_value_size: HashMap<UnitKey, Vec<DynamicConstantID>> = HashMap::new();
    for key in keys {
        let value_key = allocs.find(key);
        let id = array_nodes[key.index() as usize];

        let extents = match function.nodes[id.idx()] {
            Node::Phi {
                control: _,
                data: _,
            }
            | Node::Parameter { index: _ }
            | Node::Constant { id: _ }
            | Node::ReadArray { array: _, index: _ } => {
                // For nodes that don't write to the array, the required size
                // is just the underlying size of the array.
                type_extents(typing[id.idx()], types)
            }
            Node::Collect {
                control: _,
                data: _,
            }
            | Node::WriteArray {
                array: _,
                data: _,
                index: _,
            } => {
                // For nodes that write to the array, the required size depends
                // on the surrounding fork / join pairs.
                write_dimensionality(
                    function,
                    id,
                    typing,
                    types,
                    fork_join_map,
                    bbs,
                    fork_join_nests,
                )
            }
            Node::Return {
                control: _,
                data: _,
            } => {
                continue;
            }
            _ => todo!(),
        };

        // The largest required size is the correct size. It is assumed that all
        // sizes calculated above form a total order with respect to sub
        // vectoring. That is, no two vectors calculated above for the same
        // array value will contain differing elements at any given position.
        if let Some(old_extents) = key_to_value_size.get(&value_key) {
            if old_extents.len() < extents.len() {
                key_to_value_size.insert(value_key, extents);
            }
        } else {
            key_to_value_size.insert(value_key, extents);
        }
    }
    println!("{:?}", key_to_value_size);
}

/*
 * Get the dimensionality of a write. Checks for the dimensions of the array
 * type, and adds dimensions corresponding to dominating fork / join nests.
 */
pub fn write_dimensionality(
    function: &Function,
    write: NodeID,
    typing: &Vec<TypeID>,
    types: &Vec<Type>,
    fork_join_map: &HashMap<NodeID, NodeID>,
    bbs: &Vec<NodeID>,
    fork_join_nests: &HashMap<NodeID, Vec<NodeID>>,
) -> Vec<DynamicConstantID> {
    let mut extents = type_extents(typing[write.idx()], types);
    assert!(
        extents.len() > 0,
        "Can't call write_dimensionality with a node that doesn't output an array."
    );
    extents.reverse();

    for fork in fork_join_nests[&bbs[write.idx()]].iter() {
        if let Node::Fork { control: _, factor } = function.nodes[fork.idx()] {
            // If this node is a collect, we don't need to add the dimension
            // from the corresponding fork.
            if function.nodes[write.idx()].is_collect() && fork_join_map[&fork] != bbs[write.idx()]
            {
                extents.push(factor);
            }
        } else {
            panic!("Fork join nests map contains a non-fork in the value list.");
        }
    }

    extents.reverse();
    extents
}

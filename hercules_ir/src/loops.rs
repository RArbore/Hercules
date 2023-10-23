extern crate bitvec;

use std::collections::HashMap;

use self::bitvec::prelude::*;

use crate::*;

/*
 * Custom type for storing a loop tree. Each node corresponds to a single loop
 * or a fork join pair in the IR graph. Each node in the tree corresponds to
 * some subset of the overall IR graph. The root node corresponds to the entire
 * IR graph. The children of the root correspond to the top-level loops and fork
 * join pairs, and so on. Each node in the loop tree has a representative
 * "header" node. For normal loops, this is the region node branched to by a
 * dominated if node. For fork join pairs, this is the fork node. The root node
 * is not explicitly stored.
 */
#[derive(Debug, Clone)]
pub struct LoopTree {
    loops: HashMap<NodeID, (BitVec<u8, Lsb0>, NodeID)>,
}

impl LoopTree {}

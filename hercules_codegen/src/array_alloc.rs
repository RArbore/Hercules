extern crate hercules_ir;

use self::hercules_ir::dataflow::*;
use self::hercules_ir::ir::*;

use self::ArrayAllocSemilattice::*;

/*
 * Enum for array allocation semilattice.
 */
#[derive(PartialEq, Eq, Clone, Debug)]
enum ArrayAllocSemilattice {}

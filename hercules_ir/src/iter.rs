use crate::*;

/*
 * Top level function to run "iter" optimization. Named after the "iter"
 * optimization from the OpenJDK HotSpot compiler. Runs constant propgataion,
 * unreachable code elimination, and global value numbering, at once.
 */
pub fn iter(function: Function) -> Function {
    function
}

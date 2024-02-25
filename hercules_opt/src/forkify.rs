extern crate hercules_ir;

use std::iter::zip;

use self::hercules_ir::def_use::*;
use self::hercules_ir::ir::*;
use self::hercules_ir::loops::*;

/*
 * Top level function to convert natural loops with simple induction variables
 * into fork-joins.
 */
pub fn forkify(
    function: &mut Function,
    constants: &Vec<Constant>,
    dynamic_constants: &mut Vec<DynamicConstant>,
    loops: &LoopTree,
) {
    // Ignore loops that are already fork-joins.
    let natural_loops = loops
        .loops()
        .filter(|(k, _)| function.nodes[k.idx()].is_region());

    // Detect loops that have a simple loop induction variable. TODO: proper
    // affine analysis to recognize other cases of linear induction variables.
    let affine_loops = natural_loops.filter_map(|(header, (contents, _))| {
        // Get the single loop contained predecessor of the loop header.
        let header_uses = get_uses(&function.nodes[header.idx()]);
        let mut pred_loop = header_uses.as_ref().iter().filter(|id| contents[id.idx()]);
        let single_pred_loop = pred_loop.next()?;
        if pred_loop.next().is_some() {
            return None;
        }

        // Check for a very particular loop indexing structure.
        let if_ctrl = function.nodes[single_pred_loop.idx()].try_control_read(1)?;
        let (_, if_cond) = function.nodes[if_ctrl.idx()].try_if()?;
        let (idx, bound) = function.nodes[if_cond.idx()].try_binary(BinaryOperator::LT)?;
        let (phi, one) = function.nodes[idx.idx()].try_binary(BinaryOperator::Add)?;
        let (should_be_header, pred_datas) = function.nodes[phi.idx()].try_phi()?;
        let one_c_id = function.nodes[one.idx()].try_constant()?;
        let bound_dc_id = function.nodes[bound.idx()].try_dynamic_constant()?;

        if should_be_header != *header || !constants[one_c_id.idx()].is_one() {
            return None;
        }

        // Check that phi's if predecessor is the add node, and check that the
        // phi's other predecessors are zeros.
        zip(header_uses.as_ref().iter(), pred_datas.iter())
            .position(|(c, d)| *c == *single_pred_loop && *d == idx)?;
        if zip(header_uses.as_ref().iter(), pred_datas.iter())
            .filter(|(c, d)| {
                (**c != *single_pred_loop) && !function.nodes[d.idx()].is_zero_constant(constants)
            })
            .count()
            != 0
        {
            return None;
        }

        Some(header)
    });

    println!("{:?}", affine_loops.collect::<Vec<_>>());
}

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
    def_use: &ImmutableDefUseMap,
    loops: &LoopTree,
) {
    // Ignore loops that are already fork-joins.
    let natural_loops = loops
        .loops()
        .filter(|(k, _)| function.nodes[k.idx()].is_region());

    // Detect loops that have a simple loop induction variable. TODO: proper
    // affine analysis to recognize other cases of linear induction variables.
    let affine_loops: Vec<_> = natural_loops
        .into_iter()
        .filter_map(|(header, (contents, _))| {
            // Get the single loop contained predecessor of the loop header.
            let header_uses = get_uses(&function.nodes[header.idx()]);
            let mut pred_loop = header_uses.as_ref().iter().filter(|id| contents[id.idx()]);
            let single_pred_loop = pred_loop.next()?;
            if pred_loop.next().is_some() || header_uses.as_ref().len() != 2 {
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
                    (**c != *single_pred_loop)
                        && !function.nodes[d.idx()].is_zero_constant(constants)
                })
                .count()
                != 0
            {
                return None;
            }

            Some((header, phi, contents, bound_dc_id))
        })
        .collect();

    // Convert affine loops into fork-joins.
    for (header, idx_phi, contents, dc_id) in affine_loops {
        let header_uses = get_uses(&function.nodes[header.idx()]);
        let header_uses: Vec<_> = header_uses.as_ref().into_iter().map(|x| *x).collect();

        // Get the control portions of the loop that need to be grafted.
        let loop_pred = *header_uses
            .iter()
            .filter(|id| !contents[id.idx()])
            .next()
            .unwrap();
        let loop_true_read = *header_uses
            .iter()
            .filter(|id| contents[id.idx()])
            .next()
            .unwrap();
        let loop_end = function.nodes[loop_true_read.idx()]
            .try_control_read(1)
            .unwrap();
        let loop_false_read = *def_use
            .get_users(loop_end)
            .iter()
            .filter_map(|id| {
                if function.nodes[id.idx()].try_control_read(0).is_some() {
                    Some(id)
                } else {
                    None
                }
            })
            .next()
            .unwrap();
        let loop_dst = def_use.get_users(loop_false_read)[0];

        // Create fork and join nodes.
        let fork = Node::Fork {
            control: loop_pred,
            factor: dc_id,
        };
        let fork_id = NodeID::new(function.nodes.len());
        function.nodes.push(fork);

        let join = Node::Join {
            control: if *header == get_uses(&function.nodes[loop_end.idx()]).as_ref()[0] {
                fork_id
            } else {
                function.nodes[loop_end.idx()].try_if().unwrap().0
            },
        };
        let join_id = NodeID::new(function.nodes.len());
        function.nodes.push(join);

        // Reconnect control nodes.
        get_uses_mut(&mut function.nodes[loop_dst.idx()]).map(loop_false_read, join_id);

        // Convert reducing phi nodes to reduce nodes.
        let reduction_phis: Vec<_> = def_use
            .get_users(*header)
            .iter()
            .filter(|id| **id != idx_phi && function.nodes[id.idx()].is_phi())
            .collect();
        for reduction_phi in reduction_phis {
            // Loop predecessor input to phi is the reduction initializer.
            let init = *zip(
                header_uses.iter(),
                function.nodes[reduction_phi.idx()]
                    .try_phi()
                    .unwrap()
                    .1
                    .iter(),
            )
            .filter(|(c, _)| **c == loop_pred)
            .next()
            .unwrap()
            .1;

            // Loop back edge input to phi is the reduction induction variable.
            let reduct = *zip(
                header_uses.iter(),
                function.nodes[reduction_phi.idx()]
                    .try_phi()
                    .unwrap()
                    .1
                    .iter(),
            )
            .filter(|(c, _)| **c == loop_true_read)
            .next()
            .unwrap()
            .1;

            // Create reduction node.
            let reduce = Node::Reduce {
                control: join_id,
                init,
                reduct,
            };
            let reduce_id = NodeID::new(function.nodes.len());
            function.nodes.push(reduce);

            // Edit users of phis.
            for user in def_use.get_users(*reduction_phi) {
                get_uses_mut(&mut function.nodes[user.idx()]).map(*reduction_phi, reduce_id);
            }

            // Delete reducing phi.
            function.nodes[reduction_phi.idx()] = Node::Start;
        }

        // Convert index phi node to thread ID node.
        let thread_id = Node::ThreadID { control: fork_id };
        let thread_id_id = NodeID::new(function.nodes.len());
        function.nodes.push(thread_id);
        for user in def_use.get_users(idx_phi) {
            get_uses_mut(&mut function.nodes[user.idx()]).map(idx_phi, thread_id_id);
        }
        function.nodes[idx_phi.idx()] = Node::Start;

        // Delete old loop control nodes;
        function.nodes[header.idx()] = Node::Start;
        function.nodes[loop_end.idx()] = Node::Start;
        function.nodes[loop_true_read.idx()] = Node::Start;
        function.nodes[loop_false_read.idx()] = Node::Start;
    }
}

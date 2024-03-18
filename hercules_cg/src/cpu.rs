extern crate bitvec;
extern crate hercules_ir;

use std::collections::HashMap;
use std::collections::VecDeque;

use std::fmt::Write;

use self::bitvec::prelude::*;

use self::hercules_ir::*;

use crate::*;

/*
 * When assembling LLVM basic blocks, we traverse the nodes in a partition in an
 * ad-hoc order. Thus, we cannot assume block terminators will be visited after
 * data nodes, for example. However, textual LLVM IR requires that the
 * terminator instruction is last. So, we emit nodes into separate strings of
 * LLVM IR that will get stichted together when the block is complete.
 */
#[derive(Debug)]
struct LLVMBlock {
    header: String,
    phis: String,
    data: String,
    terminator: String,
}

impl<'a> FunctionContext<'a> {
    /*
     * Top level function to generate code for a partition, targeting the CPU.
     */
    pub(crate) fn codegen_cpu<W: Write>(
        &self,
        top_node: NodeID,
        w: &mut W,
    ) -> Result<(), std::fmt::Error> {
        // Step 1: do some analysis to get a bunch of per-partition information.
        let partition_id = self.plan.partitions[top_node.idx()];
        let partition_context = PartitionContext::new(self, partition_id);

        // Step 2: emit the function signature.
        write!(
            w,
            "define internal {} @{}_part_{}(",
            generate_type_string(&partition_context.return_type, &self.llvm_types),
            self.function.name,
            partition_id.idx(),
        )?;
        if !partition_context.input_types.is_empty() {
            write!(
                w,
                "{} %a.0",
                &self.llvm_types[partition_context.input_types[0].idx()]
            )?;
            for (idx, id) in partition_context.input_types[1..].iter().enumerate() {
                write!(w, ", {} %a.{}", &self.llvm_types[id.idx()], idx + 1)?;
            }
        }
        write!(w, ") {{\n")?;

        // Step 3: set up basic blocks. A node represents a basic block if its
        // entry in the basic blocks vector points to itself.
        let mut llvm_bbs = HashMap::new();
        for id in &self.partitions_inverted_map[partition_id.idx()] {
            if self.bbs[id.idx()] == *id {
                llvm_bbs.insert(
                    id,
                    LLVMBlock {
                        header: format!("bb_{}:\n", id.idx()),
                        phis: "".to_string(),
                        data: "".to_string(),
                        terminator: "".to_string(),
                    },
                );
            }
        }

        // Step 4: emit nodes. Nodes are emitted into basic blocks separately as
        // nodes are not necessarily emitted in order. Assemble worklist of
        // nodes, starting as reverse post order of nodes. For non-phi and non-
        // reduce nodes, only emit once all data uses are emitted. In addition,
        // consider additional anti-dependence edges from read to write nodes.
        let mut visited = bitvec![u8, Lsb0; 0; self.function.nodes.len()];
        let mut worklist = VecDeque::from(partition_context.reverse_postorder.clone());
        while let Some(id) = worklist.pop_front() {
            if !(self.function.nodes[id.idx()].is_phi()
                || self.function.nodes[id.idx()].is_reduce())
                && !get_uses(&self.function.nodes[id.idx()])
                    .as_ref()
                    .into_iter()
                    // If this node isn't a phi or reduce, we need to check that
                    // all uses, as well as all reads we anti-depend with, have
                    // been emitted.
                    .chain(self.antideps.iter().filter_map(|(read, write)| {
                        if id == *write {
                            Some(read)
                        } else {
                            None
                        }
                    }))
                    // Only data dependencies inside this partition need to have
                    // already been visited.
                    .all(|id| {
                        self.plan.partitions[id.idx()] != partition_id
                            || self.function.nodes[id.idx()].is_control()
                            || visited[id.idx()]
                    })
            {
                // Skip emitting node if it's not a phi or reducee node and if
                // its data uses are not emitted yet.
                worklist.push_back(id);
            } else {
                // Once all of the data dependencies for this node are emitted,
                // this node can be emitted.
                partition_context
                    .codegen_cpu_node(id, llvm_bbs.get_mut(&self.bbs[id.idx()]).unwrap())?;
                visited.set(id.idx(), true);
            }
        }

        // Step 5: emit the now completed basic blocks, in order. Make sure to
        // emit the "top" basic block first.
        write!(
            w,
            "{}{}{}{}",
            llvm_bbs[&top_node].header,
            llvm_bbs[&top_node].phis,
            llvm_bbs[&top_node].data,
            llvm_bbs[&top_node].terminator
        )?;
        for id in partition_context.reverse_postorder {
            if self.bbs[id.idx()] == id && id != top_node {
                write!(
                    w,
                    "{}{}{}{}",
                    llvm_bbs[&id].header,
                    llvm_bbs[&id].phis,
                    llvm_bbs[&id].data,
                    llvm_bbs[&id].terminator
                )?;
            }
        }

        // Step 6: close the partition function - we're done.
        write!(w, "}}\n\n")?;

        Ok(())
    }
}

impl<'a> PartitionContext<'a> {
    /*
     * Emit LLVM IR implementing a single node.
     */
    fn codegen_cpu_node(&self, id: NodeID, bb: &mut LLVMBlock) -> std::fmt::Result {
        // Emit the primary IR for each node.
        match self.function.function.nodes[id.idx()] {
            Node::Start => {}
            Node::Region { preds: _ } => {}
            Node::Return {
                control: _,
                data: _,
            } => {}
            Node::Parameter { index: _ } => {}
            Node::Constant { id: _ } => {}
            Node::DynamicConstant { id: _ } => {}
            _ => {}
        }

        // If this node is a control return, we emit a return from this
        // partition function.
        if self.control_returns.contains(&id) {
            write!(
                bb.terminator,
                "  ret {} {{",
                generate_type_string(&self.return_type, &self.function.llvm_types)
            )?;

            if !self.data_outputs.is_empty() {
                self.cpu_emit_value_for_node(self.data_outputs[0], false, &mut bb.terminator)?;
                for id in &self.data_outputs[1..] {
                    write!(bb.terminator, ", ")?;
                    self.cpu_emit_value_for_node(*id, false, &mut bb.terminator)?;
                }
            }

            write!(bb.terminator, "}}\n")?;
        }

        Ok(())
    }

    /*
     * Emit the LLVM value corresponding to a node. Optionally prefix with the
     * LLVM type, which is required by textual LLVM IR in a few places.
     */
    fn cpu_emit_value_for_node<W: Write>(
        &self,
        id: NodeID,
        emit_type: bool,
        w: &mut W,
    ) -> std::fmt::Result {
        // First, emit the type before the value (if applicable).
        if emit_type {
            write!(
                w,
                "{} ",
                self.function.llvm_types[self.function.typing[id.idx()].idx()]
            )?;
        }

        // Emitting the value can be surprisingly complicated, depending on what
        // the node is. For example, partition arguments are emitted specially.
        if let Some(input_idx) = self.data_inputs.iter().position(|inp_id| *inp_id == id) {
            write!(w, "%a.{}", input_idx)?;
        } else {
            assert_eq!(self.partition_id, self.function.plan.partitions[id.idx()]);
            match self.function.function.nodes[id.idx()] {
                Node::Parameter { index } => {
                    write!(w, "%a.{}", self.function_to_partition_parameter(index))?
                }
                Node::Constant { id } => write!(w, "{}", self.function.llvm_constants[id.idx()])?,
                _ => write!(w, "%v.{}", id.idx())?,
            }
        }

        Ok(())
    }
}

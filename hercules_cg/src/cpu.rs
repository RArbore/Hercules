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

        // Step 2: determine the function signature for this partition.
        let input_types = partition_context.partition_input_types();
        let return_type = partition_context.partition_return_type();

        // Step 3: emit the function signature.
        write!(
            w,
            "define internal {} @{}_part_{}(",
            generate_type_string(&return_type, &self.llvm_types),
            self.function.name,
            partition_id.idx(),
        )?;
        if !input_types.is_empty() {
            write!(w, "{}", &self.llvm_types[input_types[0].idx()])?;
            for id in &input_types[1..] {
                write!(w, ", {}", &self.llvm_types[id.idx()])?;
            }
        }
        write!(w, ") {{\n")?;

        // Step 4: set up basic blocks. A node represents a basic block if its
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

        // Step 5: emit nodes. Nodes are emitted into basic blocks separately as
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
                partition_context.codegen_cpu_node(id, w)?;
                visited.set(id.idx(), true);
            }
        }

        // Step 6: emit the now completed basic blocks, in order.
        for id in partition_context.reverse_postorder {
            if self.bbs[id.idx()] == id {
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

        // Step 7: close the partition function - we're done.
        write!(w, "}}\n\n")?;

        Ok(())
    }
}

impl<'a> PartitionContext<'a> {
    fn codegen_cpu_node<W: Write>(&self, id: NodeID, w: &mut W) -> std::fmt::Result {
        todo!()
    }
}

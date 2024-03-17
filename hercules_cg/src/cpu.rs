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
        let data_inputs = self.partition_data_inputs(partition_id);
        let data_outputs = self.partition_data_outputs(partition_id);
        let control_returns = self.partition_control_returns(partition_id);
        let control_successors = self.partition_control_successors(partition_id);
        let function_parameters = self.partition_function_parameters(partition_id);
        let array_constants = self.partition_array_constants(partition_id);
        let dynamic_constants = self.partition_dynamic_constants(partition_id);
        let reverse_postorder = self.partition_reverse_postorder(partition_id);

        // Step 2: determine the function signature for this partition. The
        // arguments are the input data nodes, plus used function parameters,
        // plus used array constants, plus used dynamic constants. The return
        // struct contains all of the data outputs, plus control information if
        // there are multiple successor partitions. The control information is
        // used by the orchestration code to implement control flow between
        // partitions.
        let input_data_types = data_inputs.iter().map(|id| self.typing[id.idx()]);
        let function_parameter_types = function_parameters
            .iter()
            .map(|index| self.function.param_types[*index]);
        let array_constant_types =
            array_constants
                .iter()
                .map(|id| match self.constants[id.idx()] {
                    Constant::Array(ty_id, _) => ty_id,
                    Constant::Zero(ty_id) => ty_id,
                    _ => panic!(),
                });
        let dynamic_constant_types =
            std::iter::repeat(get_type_id(Type::UnsignedInteger64, &self.types).unwrap())
                .take(dynamic_constants.len());
        let input_types: Vec<TypeID> = input_data_types
            .chain(function_parameter_types)
            .chain(array_constant_types)
            .chain(dynamic_constant_types)
            .collect();

        let multiple_control_successors = control_successors.len() > 1;
        let output_data_types = data_outputs.iter().map(|id| self.typing[id.idx()]);
        let return_type = if multiple_control_successors {
            Type::Product(
                output_data_types
                    .chain(std::iter::once(
                        get_type_id(Type::UnsignedInteger64, &self.types).unwrap(),
                    ))
                    .collect(),
            )
        } else {
            Type::Product(output_data_types.collect())
        };

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
        let mut worklist = VecDeque::from(reverse_postorder.clone());
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
                // TODO: emit!
                visited.set(id.idx(), true);
            }
        }

        // Step ?: emit the now completed basic blocks, in order.
        for id in reverse_postorder {
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

        // Step ?: close the partition function - we're done.
        write!(w, "}}\n\n")?;

        Ok(())
    }
}

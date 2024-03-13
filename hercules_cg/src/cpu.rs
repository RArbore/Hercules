extern crate hercules_ir;

use std::fmt::Write;

use self::hercules_ir::*;

use crate::*;

impl<'a> FunctionContext<'a> {
    /*
     * Top level function to generate code for a partition, targeting the CPU.
     */
    pub(crate) fn codegen_cpu<W: Write>(
        &self,
        top_node: NodeID,
        w: &mut W,
    ) -> Result<(), std::fmt::Error> {
        // Step 1: do some analysis to find four things:
        // 1. All of the data inputs to this partition.
        // 2. All of the data outputs of this partition.
        // 3. All of the return locations of this partition.
        // 4. All of the successor partitions of this partition.
        let partition_id = self.plan.partitions[top_node.idx()];
        let data_inputs = self.partition_data_inputs(partition_id);
        let data_outputs = self.partition_data_outputs(partition_id);
        let control_returns = self.partition_control_returns(partition_id);
        let control_successors = self.partition_control_successors(partition_id);
        println!("{:?}", partition_id);
        println!("{:?}", data_inputs);
        println!("{:?}", data_outputs);
        println!("{:?}", control_returns);
        println!("{:?}", control_successors);

        // Step 2: Determine the function signature for this partition. The
        // arguments are the input data nodes, plus dynamic constants, plus
        // array constants. The return struct contains all of the data outputs,
        // plus control information if there are multiple successor partitions.
        // The control information is used by the orchestration code to
        // implement control flow between partitions.
        let multiple_control_successors = control_successors.len() > 1;
        let return_type = Type::Product(
            data_outputs
                .iter()
                .map(|id| self.typing[id.idx()])
                .collect(),
        );

        Ok(())
    }
}

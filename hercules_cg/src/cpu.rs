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
        // Step 1: do some analysis to get a bunch of per-partition information.
        let partition_id = self.plan.partitions[top_node.idx()];
        let data_inputs = self.partition_data_inputs(partition_id);
        let data_outputs = self.partition_data_outputs(partition_id);
        let control_returns = self.partition_control_returns(partition_id);
        let control_successors = self.partition_control_successors(partition_id);
        let function_parameters = self.partition_function_parameters(partition_id);
        let array_constants = self.partition_array_constants(partition_id);
        let dynamic_constants = self.partition_dynamic_constants(partition_id);
        println!("PartitionID: {:?}", partition_id);
        println!("Data Inputs: {:?}", data_inputs);
        println!("Data Outputs: {:?}", data_outputs);
        println!("Control Returns: {:?}", control_returns);
        println!("Control Successors: {:?}", control_successors);
        println!("Function Parameters: {:?}", function_parameters);
        println!("Array Constants: {:?}", array_constants);
        println!("Dynamic Constants: {:?}", dynamic_constants);

        // Step 2: Determine the function signature for this partition. The
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

        println!("Inputs: {:?}", input_types);
        println!("Return: {:?}", return_type);
        println!("");

        Ok(())
    }
}

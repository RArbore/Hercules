extern crate hercules_ir;

use std::fmt::Write;

use self::hercules_ir::*;

use crate::*;

/*
 * Top level function to generate code for a module. Emits LLVM IR text. Calls
 * out to backends to generate code for individual partitions. Generates
 * orchestration code to manage partitions.
 */
pub fn codegen<W: Write>(
    module: &Module,
    def_uses: &Vec<ImmutableDefUseMap>,
    typing: &ModuleTyping,
    control_subgraphs: &Vec<Subgraph>,
    plans: &Vec<Plan>,
    w: &mut W,
) -> std::fmt::Result {
    // Render types, constants, and dynamic constants into LLVM IR.
    let llvm_types = generate_type_strings(module);
    let llvm_constants = generate_constant_strings(module);
    let llvm_dynamic_constants = generate_dynamic_constant_strings(module);

    // Do codegen for each function individually.
    for function_idx in 0..module.functions.len() {
        // There's a bunch of per-function information we use.
        let context = FunctionContext {
            function: &module.functions[function_idx],
            def_use: &def_uses[function_idx],
            typing: &typing[function_idx],
            control_subgraph: &control_subgraphs[function_idx],
            plan: &plans[function_idx],
            llvm_types: &llvm_types,
            llvm_constants: &llvm_constants,
            llvm_dynamic_constants: &llvm_dynamic_constants,
            partitions_inverted_map: plans[function_idx].invert_partition_map(),
        };

        context.codegen_function(w)?;
    }

    Ok(())
}

impl<'a> FunctionContext<'a> {
    /*
     * Each function gets codegened separately.
     */
    fn codegen_function<W: Write>(&self, w: &mut W) -> std::fmt::Result {
        // Find the "top" control node of each partition. One well-formedness
        // condition of partitions is that there is exactly one "top" control
        // node.
        let top_nodes: Vec<NodeID> = self
            .partitions_inverted_map
            .iter()
            .enumerate()
            .map(|(part_idx, part)| {
                // For each partition, find the "top" node.
                *part
                    .iter()
                    .filter(move |id| {
                        // The "top" node is a control node having at least one
                        // control predecessor in another partition, or is a
                        // start node. Every predecessor in the control subgraph
                        // is a control node.
                        self.function.nodes[id.idx()].is_start()
                            || (self.function.nodes[id.idx()].is_control()
                                && self
                                    .control_subgraph
                                    .preds(**id)
                                    .filter(|pred_id| {
                                        self.plan.partitions[pred_id.idx()].idx() != part_idx
                                    })
                                    .count()
                                    > 0)
                    })
                    .next()
                    .unwrap()
            })
            .collect();

        // Generate code for each individual partition. This generates a single
        // LLVM function per partition, which we will use in the orchestration
        // code for the whole function.
        assert_eq!(self.plan.num_partitions, top_nodes.len());
        for part_idx in 0..self.plan.num_partitions {
            match self.plan.partition_devices[part_idx] {
                Device::CPU => self.codegen_cpu(top_nodes[part_idx], w)?,
                Device::GPU => todo!(),
            }
        }

        Ok(())
    }
}

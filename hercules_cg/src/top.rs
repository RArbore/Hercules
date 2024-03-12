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
        let function = &module.functions[function_idx];
        let def_use = &def_uses[function_idx];
        let control_subgraph = &control_subgraphs[function_idx];
        let plan = &plans[function_idx];

        codegen_function(
            function,
            def_use,
            control_subgraph,
            plan,
            &llvm_types,
            &llvm_constants,
            &llvm_dynamic_constants,
            w,
        )?;
    }

    Ok(())
}

/*
 * Each function gets codegened separately.
 */
fn codegen_function<W: Write>(
    function: &Function,
    def_use: &ImmutableDefUseMap,
    control_subgraph: &Subgraph,
    plan: &Plan,
    llvm_types: &Vec<String>,
    llvm_constants: &Vec<String>,
    llvm_dynamic_constants: &Vec<String>,
    w: &mut W,
) -> std::fmt::Result {
    // Find the "top" control node of each partition. One well-formedness
    // condition of partitions is that there is exactly one "top" control node.
    let partitions = plan.invert_partition_map();
    let top_nodes: Vec<NodeID> = partitions
        .iter()
        .enumerate()
        .map(|(part_idx, part)| {
            // For each partition, find the "top" node.
            *part
                .iter()
                .filter(move |id| {
                    // The "top" node is a control node having at least one
                    // control predecessor in another partition, or is a start
                    // node. Every predecessor in the control subgraph is a
                    // control node.
                    function.nodes[id.idx()].is_start()
                        || (function.nodes[id.idx()].is_control()
                            && control_subgraph
                                .preds(**id)
                                .filter(|pred_id| plan.partitions[pred_id.idx()].idx() != part_idx)
                                .count()
                                > 0)
                })
                .next()
                .unwrap()
        })
        .collect();

    // Generate code for each individual partition. This generates a single LLVM
    // function per partition, which we will use in the orchestration code for
    // the whole function.
    assert_eq!(plan.num_partitions, top_nodes.len());
    for part_idx in 0..plan.num_partitions {
        match plan.partition_devices[part_idx] {
            Device::CPU => codegen_cpu(
                function,
                def_use,
                plan,
                top_nodes[part_idx],
                &partitions,
                llvm_types,
                llvm_constants,
                llvm_dynamic_constants,
                w,
            )?,
            Device::GPU => todo!(),
        }
    }

    Ok(())
}

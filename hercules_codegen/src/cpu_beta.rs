extern crate hercules_ir;

use std::collections::HashMap;
use std::fmt::Write;

use self::hercules_ir::def_use::*;
use self::hercules_ir::ir::*;

/*
 * This CPU backend is a rewrite of the original CPU alpha backend. It accounts
 * for many changes in the IR designed to make lowering, optimization, and code
 * generation easier.
 */

/*
 * Top level function to generate code for a module. Emits LLVM IR text.
 */
pub fn cpu_beta_codegen<W: Write>(
    module: &hercules_ir::ir::Module,
    typing: &hercules_ir::typecheck::ModuleTyping,
    reverse_postorders: &Vec<Vec<NodeID>>,
    def_uses: &Vec<ImmutableDefUseMap>,
    bbs: &Vec<Vec<NodeID>>,
    antideps: &Vec<Vec<(NodeID, NodeID)>>,
    array_allocations: &Vec<(Vec<Vec<DynamicConstantID>>, HashMap<NodeID, usize>)>,
    fork_join_nests: &Vec<HashMap<NodeID, Vec<NodeID>>>,
    w: &mut W,
) -> std::fmt::Result {
    let hercules_ir::ir::Module {
        functions,
        types,
        constants,
        dynamic_constants,
    } = module;

    // Step 1: render types into LLVM IR. This requires translating from our
    // interning structures to LLVM types. We can't just blow through the types
    // vector, since a type may reference a type ID ahead of it in the vector.
    // Instead, iterate types in a bottom up order with respect to the type
    // intern DAGs.
    let mut llvm_types = vec!["".to_string(); types.len()];
    for id in module.types_bottom_up() {
        match &types[id.idx()] {
            Type::Control(_) => {
                llvm_types[id.idx()] = "CONTROL_NOT_AN_LLVM_TYPE".to_string();
            }
            Type::Boolean => {
                llvm_types[id.idx()] = "i1".to_string();
            }
            Type::Integer8 | Type::UnsignedInteger8 => {
                llvm_types[id.idx()] = "i8".to_string();
            }
            Type::Integer16 | Type::UnsignedInteger16 => {
                llvm_types[id.idx()] = "i16".to_string();
            }
            Type::Integer32 | Type::UnsignedInteger32 => {
                llvm_types[id.idx()] = "i32".to_string();
            }
            Type::Integer64 | Type::UnsignedInteger64 => {
                llvm_types[id.idx()] = "i64".to_string();
            }
            Type::Float32 => {
                llvm_types[id.idx()] = "f32".to_string();
            }
            Type::Float64 => {
                llvm_types[id.idx()] = "f64".to_string();
            }
            // Because we traverse in bottom-up order, we can assume that the
            // LLVM types for children types are already computed.
            Type::Product(fields) => {
                let mut iter = fields.iter();
                if let Some(first) = iter.next() {
                    llvm_types[id.idx()] =
                        iter.fold("{".to_string() + &llvm_types[first.idx()], |s, f| {
                            s + ", " + &llvm_types[f.idx()]
                        }) + "}";
                } else {
                    llvm_types[id.idx()] = "{}".to_string();
                }
            }
            Type::Array(_, _) => {
                // Array types becomes pointers. The elemtn type and dynamic
                // constant bounds characterize the access code we generate
                // later, not the type itself.
                llvm_types[id.idx()] = "ptr".to_string();
            }
            Type::Summation(_) => todo!(),
        }
    }

    // Step 2: render constants into LLVM IR. This is done in a very similar
    // manner as types.
    let mut llvm_constants = vec!["".to_string(); types.len()];
    for id in module.constants_bottom_up() {
        match &constants[id.idx()] {
            Constant::Boolean(val) => {
                llvm_constants[id.idx()] = if *val {
                    "i1 true".to_string()
                } else {
                    "i1 false".to_string()
                };
            }
            Constant::Integer8(val) => llvm_constants[id.idx()] = format!("i8 {}", val),
            Constant::Integer16(val) => llvm_constants[id.idx()] = format!("i16 {}", val),
            Constant::Integer32(val) => llvm_constants[id.idx()] = format!("i32 {}", val),
            Constant::Integer64(val) => llvm_constants[id.idx()] = format!("i64 {}", val),
            Constant::UnsignedInteger8(val) => llvm_constants[id.idx()] = format!("i8 {}", val),
            Constant::UnsignedInteger16(val) => llvm_constants[id.idx()] = format!("i16 {}", val),
            Constant::UnsignedInteger32(val) => llvm_constants[id.idx()] = format!("i32 {}", val),
            Constant::UnsignedInteger64(val) => llvm_constants[id.idx()] = format!("i64 {}", val),
            Constant::Float32(val) => llvm_constants[id.idx()] = format!("f32 {}", val),
            Constant::Float64(val) => llvm_constants[id.idx()] = format!("f64 {}", val),
            Constant::Product(_, fields) => {
                let mut iter = fields.iter();
                if let Some(first) = iter.next() {
                    llvm_constants[id.idx()] =
                        iter.fold("{".to_string() + &llvm_constants[first.idx()], |s, f| {
                            s + ", " + &llvm_constants[f.idx()]
                        }) + "}";
                } else {
                    llvm_constants[id.idx()] = "{}".to_string();
                }
            }
            Constant::Array(_, _) => todo!(),
            Constant::Summation(_, _, _) => todo!(),
        }
    }

    // Step 3: do codegen for each function.
    for function_idx in 0..functions.len() {
        let function = &functions[function_idx];
        let typing = &typing[function_idx];
        let reverse_postorder = &reverse_postorders[function_idx];
        let def_use = &def_uses[function_idx];
        let bb = &bbs[function_idx];
        let antideps = &antideps[function_idx];
        let fork_join_nest = &fork_join_nests[function_idx];
        let array_allocations = &array_allocations[function_idx];

        // Step 3.1: emit function signature.
        let llvm_ret_type = &llvm_types[function.return_type.idx()];
        let mut llvm_params = function
            .param_types
            .iter()
            .enumerate()
            .map(|(idx, id)| format!("{} %p{}", &llvm_types[id.idx()], idx))
            .chain((0..function.num_dynamic_constants).map(|idx| format!("i64 %dc{}", idx)))
            .chain((0..array_allocations.0.len()).map(|idx| format!("ptr %arr{}", idx)));
        write!(w, "define {} @{}(", llvm_ret_type, function.name)?;
        if let Some(first) = llvm_params.next() {
            write!(w, "{}", first)?;
            for p in llvm_params {
                write!(w, ", {}", p)?;
            }
        }
        write!(w, ") {{\n")?;

        // Step 3.2: emit basic blocks. A node represents a basic block if its
        // entry in the basic blocks vector points to itself.
        let mut llvm_bbs = HashMap::new();
        for id in (0..function.nodes.len()).map(NodeID::new) {
            if bb[id.idx()] == id {
                llvm_bbs.insert(id, format!("bb_{}:\n", id.idx()));
            }
        }

        // Step 3.3: emit nodes. Nodes are emitted into basic blocks separately
        // as nodes are not necessarily emitted in order. Assemble worklist of
        // nodes, starting as reverse post order of nodes. For non-phi and non-
        // reduce nodes, only emit once all data uses are emitted. Wait to emit
        // phi and reduce nodes. In addition, consider additional anti-
        // dependence edges from read to write nodes.
    }

    Ok(())
}

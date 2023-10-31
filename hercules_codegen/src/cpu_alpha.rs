extern crate bitvec;
extern crate hercules_ir;
extern crate inkwell;

use std::iter::zip;

use self::bitvec::prelude::*;

use self::inkwell::builder::*;
use self::inkwell::context::*;
use self::inkwell::module::*;
use self::inkwell::types::*;
use self::inkwell::*;

use self::hercules_ir::def_use::*;
use self::hercules_ir::ir::*;

/*
 * This CPU backend was written to get some Hercules IR running, and to better
 * understand how writing backends for Hercules IR will work. This backend is
 * not meant to be used in the long term. If you are reading this code in a
 * significant amount of time from when this comment was written, you are
 * probably already doing something wrong - Russel.
 */

/*
 * Top level function to generate code for a module. Writes the result object
 * file to the specified path.
 */
pub fn cpu_alpha_codegen(
    module: &hercules_ir::ir::Module,
    reverse_postorders: &Vec<Vec<NodeID>>,
    bbs: &Vec<Vec<NodeID>>,
    path: &std::path::Path,
) {
    // Step 1: partition reverse postorder into control and data nodes, for each
    // function individually.
    let hercules_ir::ir::Module {
        functions,
        types,
        constants,
        dynamic_constants,
    } = module;

    let mut cfgs = vec![];
    let mut dfgs = vec![];
    for function_idx in 0..functions.len() {
        let function = &functions[function_idx];
        let reverse_postorder = &reverse_postorders[function_idx];
        let (cfg, dfg): (Vec<_>, Vec<_>) = reverse_postorder
            .iter()
            .map(|x| *x)
            .partition(|id| function.is_control(*id));
        cfgs.push(cfg);
        dfgs.push(dfg);
    }

    // Step 2: initialize LLVM objects.
    let llvm_context = Context::create();
    let llvm_module = llvm_context.create_module("");
    let llvm_builder = llvm_context.create_builder();

    // Step 3: add all the types. This requires translating from our interning
    // structures to LLVM's. We can't just blow through the types vector, since
    // a type may reference a type ID ahead of it in the vector. Instead,
    // iterate types in a bottom up order with respect to the type intern DAGs.
    let mut llvm_types = vec![llvm_context.i8_type().as_basic_type_enum(); types.len()];
    for id in module.types_bottom_up() {
        match &types[id.idx()] {
            Type::Control(_) => {}
            Type::Boolean => {
                llvm_types[id.idx()] = llvm_context.bool_type().as_basic_type_enum();
            }
            Type::Integer8 | Type::UnsignedInteger8 => {
                llvm_types[id.idx()] = llvm_context.i8_type().as_basic_type_enum();
            }
            Type::Integer16 | Type::UnsignedInteger16 => {
                llvm_types[id.idx()] = llvm_context.i16_type().as_basic_type_enum();
            }
            Type::Integer32 | Type::UnsignedInteger32 => {
                llvm_types[id.idx()] = llvm_context.i32_type().as_basic_type_enum();
            }
            Type::Integer64 | Type::UnsignedInteger64 => {
                llvm_types[id.idx()] = llvm_context.i64_type().as_basic_type_enum();
            }
            Type::Float32 => {
                llvm_types[id.idx()] = llvm_context.f32_type().as_basic_type_enum();
            }
            Type::Float64 => {
                llvm_types[id.idx()] = llvm_context.f64_type().as_basic_type_enum();
            }
            // Because we traverse in bottom-up order, we can assume that the
            // LLVM types for children types are already computed.
            Type::Product(fields) => {
                let field_types = fields
                    .iter()
                    .map(|id| llvm_types[id.idx()])
                    .collect::<Box<[_]>>();
                llvm_types[id.idx()] = llvm_context
                    .struct_type(&field_types, false)
                    .as_basic_type_enum();
            }
            Type::Array(elem, _) => {
                let elem_type = llvm_types[elem.idx()];
                llvm_types[id.idx()] = elem_type
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum();
            }
            _ => todo!(),
        }
    }
    for (id, llvm_type) in zip((0..types.len()).map(TypeID::new), llvm_types) {
        let mut ty_str = String::new();
        module.write_type(id, &mut ty_str).unwrap();
        println!("{} {}", ty_str, llvm_type);
    }

    // Step 4: do codegen for each function.
    for function_idx in 0..functions.len() {
        let function = &functions[function_idx];
        let cfg = &cfgs[function_idx];
        let dfg = &dfgs[function_idx];
    }
}

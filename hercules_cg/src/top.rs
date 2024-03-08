extern crate hercules_ir;

use std::fmt::Write;

use self::hercules_ir::ir::*;

/*
 * Top level function to generate code for a module. Emits LLVM IR text. Calls
 * out to backends to generate code for individual partitions. Generates
 * orchestration code to manage partitions.
 */
pub fn codegen<W: Write>(module: &Module, w: &mut W) -> std::fmt::Result {
    // Render types, constants, and dynamic constants into LLVM IR.
    let llvm_types = generate_type_strings(module);
    let llvm_constants = generate_constant_strings(module);
    let llvm_dynamic_constants = generate_dynamic_constant_strings(module);

    Ok(())
}

fn generate_type_strings(module: &Module) -> Vec<String> {
    // Render types into LLVM IR. This requires translating from our interning
    // structures to LLVM types. We can't just blow through the types vector,
    // since a type may reference a type ID ahead of it in the vector. Instead,
    // iterate types in a bottom up order with respect to the type intern DAGs.
    let mut llvm_types = vec!["".to_string(); module.types.len()];
    for id in module.types_bottom_up() {
        match &module.types[id.idx()] {
            Type::Control(_) => {
                // Later, we create virtual registers corresponding to fork
                // nodes of type i64, so we need the "type" of the fork node
                // to be i64.
                llvm_types[id.idx()] = "i64".to_string();
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
                llvm_types[id.idx()] = "float".to_string();
            }
            Type::Float64 => {
                llvm_types[id.idx()] = "double".to_string();
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
                // Array types becomes pointers. The element type and dynamic
                // constant bounds characterize the access code we generate
                // later, not the type itself.
                llvm_types[id.idx()] = "ptr".to_string();
            }
            Type::Summation(_) => todo!(),
        }
    }

    llvm_types
}

fn generate_constant_strings(module: &Module) -> Vec<String> {
    // Render constants into LLVM IR. This is done in a very similar manner as
    // types.
    let mut llvm_constants = vec!["".to_string(); module.constants.len()];
    for id in module.constants_bottom_up() {
        match &module.constants[id.idx()] {
            Constant::Boolean(val) => {
                llvm_constants[id.idx()] = if *val {
                    "true".to_string()
                } else {
                    "false".to_string()
                };
            }
            Constant::Integer8(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::Integer16(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::Integer32(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::Integer64(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::UnsignedInteger8(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::UnsignedInteger16(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::UnsignedInteger32(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::UnsignedInteger64(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::Float32(val) => {
                llvm_constants[id.idx()] = if val.fract() == 0.0 {
                    format!("{}.0", val)
                } else {
                    format!("{}", val)
                }
            }
            Constant::Float64(val) => {
                llvm_constants[id.idx()] = if val.fract() == 0.0 {
                    format!("{}.0", val)
                } else {
                    format!("{}", val)
                }
            }
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
            Constant::Array(_, _) => llvm_constants[id.idx()] = format!("%arr.{}", id.idx()),
            Constant::Summation(_, _, _) => todo!(),
            Constant::Zero(ty_id) => {
                llvm_constants[id.idx()] = render_zero_constant(id, *ty_id, &module.types)
            }
        }
    }

    llvm_constants
}

fn generate_dynamic_constant_strings(module: &Module) -> Vec<String> {
    // Render dynamic constants into LLVM IR.
    let mut llvm_dynamic_constants = vec!["".to_string(); module.dynamic_constants.len()];
    for id in (0..module.dynamic_constants.len()).map(DynamicConstantID::new) {
        match &module.dynamic_constants[id.idx()] {
            DynamicConstant::Constant(val) => llvm_dynamic_constants[id.idx()] = format!("{}", val),
            DynamicConstant::Parameter(num) => {
                llvm_dynamic_constants[id.idx()] = format!("%dc{}", num)
            }
        }
    }

    llvm_dynamic_constants
}

fn render_zero_constant(cons_id: ConstantID, ty_id: TypeID, types: &Vec<Type>) -> String {
    // Extra logic for zero constants, since arrays are set to 0 by the runtime.
    match &types[ty_id.idx()] {
        Type::Array(_, _) => format!("%arr.{}", cons_id.idx()),
        Type::Summation(_) => todo!(),
        _ => "zeroinitializer".to_string(),
    }
}

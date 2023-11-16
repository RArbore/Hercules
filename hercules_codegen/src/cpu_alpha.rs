extern crate bitvec;
extern crate hercules_ir;
extern crate inkwell;

use std::collections::HashMap;
use std::collections::VecDeque;
use std::iter::zip;

use self::bitvec::prelude::*;

use self::inkwell::basic_block::*;
use self::inkwell::builder::*;
use self::inkwell::context::*;
use self::inkwell::module::*;
use self::inkwell::types::*;
use self::inkwell::values::*;
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
    typing: &hercules_ir::typecheck::ModuleTyping,
    reverse_postorders: &Vec<Vec<NodeID>>,
    def_uses: &Vec<ImmutableDefUseMap>,
    bbs: &Vec<Vec<NodeID>>,
    antideps: &Vec<Vec<(NodeID, NodeID)>>,
    path: &std::path::Path,
) {
    let hercules_ir::ir::Module {
        functions,
        types,
        constants,
        dynamic_constants,
    } = module;

    // Step 1: initialize LLVM objects.
    let llvm_context = Context::create();
    let llvm_module = llvm_context.create_module("");
    let llvm_builder = llvm_context.create_builder();

    // Step 2: convert the types. This requires translating from our interning
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
            Type::Summation(_) => todo!(),
        }
    }

    // Step 3: convert the constants. This is done in a very similar manner as
    // types.
    let mut llvm_constants = vec![
        llvm_context
            .i8_type()
            .const_int(0, false)
            .as_basic_value_enum();
        constants.len()
    ];
    for id in module.constants_bottom_up() {
        match &constants[id.idx()] {
            Constant::Boolean(val) => {
                llvm_constants[id.idx()] = llvm_context
                    .bool_type()
                    .const_int(*val as u64, false)
                    .as_basic_value_enum();
            }
            Constant::Integer8(val) => {
                llvm_constants[id.idx()] = llvm_context
                    .i8_type()
                    .const_int(*val as u64, true)
                    .as_basic_value_enum();
            }
            Constant::Integer16(val) => {
                llvm_constants[id.idx()] = llvm_context
                    .i16_type()
                    .const_int(*val as u64, true)
                    .as_basic_value_enum();
            }
            Constant::Integer32(val) => {
                llvm_constants[id.idx()] = llvm_context
                    .i32_type()
                    .const_int(*val as u64, true)
                    .as_basic_value_enum();
            }
            Constant::Integer64(val) => {
                llvm_constants[id.idx()] = llvm_context
                    .i64_type()
                    .const_int(*val as u64, true)
                    .as_basic_value_enum();
            }
            Constant::UnsignedInteger8(val) => {
                llvm_constants[id.idx()] = llvm_context
                    .i8_type()
                    .const_int(*val as u64, false)
                    .as_basic_value_enum();
            }
            Constant::UnsignedInteger16(val) => {
                llvm_constants[id.idx()] = llvm_context
                    .i16_type()
                    .const_int(*val as u64, false)
                    .as_basic_value_enum();
            }
            Constant::UnsignedInteger32(val) => {
                llvm_constants[id.idx()] = llvm_context
                    .i32_type()
                    .const_int(*val as u64, false)
                    .as_basic_value_enum();
            }
            Constant::UnsignedInteger64(val) => {
                llvm_constants[id.idx()] = llvm_context
                    .i64_type()
                    .const_int(*val, false)
                    .as_basic_value_enum();
            }
            Constant::Float32(val) => {
                llvm_constants[id.idx()] = llvm_context
                    .f32_type()
                    .const_float(val.into_inner() as f64)
                    .as_basic_value_enum();
            }
            Constant::Float64(val) => {
                llvm_constants[id.idx()] = llvm_context
                    .f64_type()
                    .const_float(val.into_inner())
                    .as_basic_value_enum();
            }
            // Because we traverse in bottom-up order, we can assume that the
            // LLVM constants for children constants are already computed.
            Constant::Product(_, fields) => {
                let field_constants = fields
                    .iter()
                    .map(|id| llvm_constants[id.idx()])
                    .collect::<Box<[_]>>();
                llvm_constants[id.idx()] = llvm_context
                    .const_struct(&field_constants, false)
                    .as_basic_value_enum();
            }
            Constant::Array(_, _) => todo!(),
            Constant::Summation(_, _, _) => todo!(),
        }
    }

    // Step 4: do codegen for each function.
    for function_idx in 0..functions.len() {
        let function = &functions[function_idx];
        let typing = &typing[function_idx];
        let reverse_postorder = &reverse_postorders[function_idx];
        let def_use = &def_uses[function_idx];
        let bb = &bbs[function_idx];
        let antideps = &antideps[function_idx];

        // Step 4.1: create LLVM function object.
        let llvm_ret_type = llvm_types[function.return_type.idx()];
        let llvm_param_types = function
            .param_types
            .iter()
            .map(|id| llvm_types[id.idx()].into())
            .collect::<Box<[_]>>();
        let llvm_fn_type = llvm_ret_type.fn_type(&llvm_param_types, false);
        let llvm_fn = llvm_module.add_function(&function.name, llvm_fn_type, None);

        // Step 4.2: create LLVM basic blocks. A node needs a corresponding
        // basic block if its entry in the basic blocks vector points to iself.
        let mut llvm_bbs = HashMap::new();
        for id in (0..function.nodes.len()).map(NodeID::new) {
            if bb[id.idx()] == id {
                llvm_bbs.insert(
                    id,
                    llvm_context.append_basic_block(llvm_fn, &format!("bb_{}", id.idx())),
                );
            }
        }

        // Step 4.3: emit LLVM for each node. Assemble worklist of nodes,
        // starting as reverse post order of nodes. For non-phi nodes, only emit
        // once all data uses are emitted. In addition, consider additional anti
        // dependence edges from read to write nodes.
        let mut values = HashMap::new();
        let mut worklist = VecDeque::from(reverse_postorder.clone());
        while let Some(id) = worklist.pop_front() {
            if !function.nodes[id.idx()].is_phi()
                && !get_uses(&function.nodes[id.idx()])
                    .as_ref()
                    .into_iter()
                    .chain(
                        antideps.iter().filter_map(
                            |(read, write)| if id == *write { Some(read) } else { None },
                        ),
                    )
                    .all(|x| {
                        function.nodes[x.idx()].is_strictly_control() || values.contains_key(x)
                    })
            {
                // Skip emitting node if it's not a phi node and if its data
                // uses are not emitted yet.
                worklist.push_back(id);
            } else {
                // Once all of the data dependencies for this node are emitted,
                // this node can be emitted.
                emit_llvm_for_node(
                    id,
                    &mut values,
                    function,
                    typing,
                    types,
                    bb,
                    def_use,
                    &llvm_builder,
                    llvm_fn,
                    &llvm_bbs,
                );
            }
        }
    }

    // Step 5: write out module to given file path.
    llvm_module.write_bitcode_to_path(path);
}

/*
 * Emit LLVM implementing a single node.
 */
fn emit_llvm_for_node<'ctx>(
    id: NodeID,
    values: &mut HashMap<NodeID, BasicValueEnum<'ctx>>,
    function: &Function,
    typing: &Vec<TypeID>,
    types: &Vec<Type>,
    bb: &Vec<NodeID>,
    def_use: &ImmutableDefUseMap,
    llvm_builder: &'ctx Builder,
    llvm_fn: FunctionValue<'ctx>,
    llvm_bbs: &HashMap<NodeID, BasicBlock<'ctx>>,
) {
    let llvm_bb = llvm_bbs[&bb[id.idx()]];
    llvm_builder.position_at_end(llvm_bb);
    match function.nodes[id.idx()] {
        Node::Start => {
            let successor = def_use
                .get_users(id)
                .iter()
                .filter(|id| function.nodes[id.idx()].is_strictly_control())
                .next()
                .unwrap();
            llvm_builder
                .build_unconditional_branch(llvm_bbs[successor])
                .unwrap();
        }
        Node::Return { control: _, data } => {
            llvm_builder.build_return(Some(&values[&data])).unwrap();
        }
        Node::Parameter { index } => {
            values.insert(
                id,
                llvm_fn
                    .get_nth_param(index as u32)
                    .unwrap()
                    .as_basic_value_enum(),
            );
        }
        Node::Unary { input, op } => {
            let input = values[&input];
            match op {
                UnaryOperator::Not => {
                    values.insert(
                        id,
                        llvm_builder
                            .build_not(input.into_int_value(), "")
                            .unwrap()
                            .as_basic_value_enum(),
                    );
                }
                UnaryOperator::Neg => {
                    if input.get_type().is_float_type() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_float_neg(input.into_float_value(), "")
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_neg(input.into_int_value(), "")
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    }
                }
            }
        }
        Node::Binary { left, right, op } => {
            let left = values[&left];
            let right = values[&right];
            match op {
                BinaryOperator::Add => {
                    if left.get_type().is_float_type() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_float_add(
                                    left.into_float_value(),
                                    right.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_add(left.into_int_value(), right.into_int_value(), "")
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    }
                }
                BinaryOperator::Sub => {
                    if left.get_type().is_float_type() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_float_sub(
                                    left.into_float_value(),
                                    right.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_sub(left.into_int_value(), right.into_int_value(), "")
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    }
                }
                BinaryOperator::Mul => {
                    if left.get_type().is_float_type() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_float_mul(
                                    left.into_float_value(),
                                    right.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_mul(left.into_int_value(), right.into_int_value(), "")
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    }
                }
                BinaryOperator::Div => {
                    if left.get_type().is_float_type() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_float_div(
                                    left.into_float_value(),
                                    right.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else if types[typing[id.idx()].idx()].is_unsigned() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_unsigned_div(
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_signed_div(
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    }
                }
                BinaryOperator::Rem => {
                    if left.get_type().is_float_type() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_float_rem(
                                    left.into_float_value(),
                                    right.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else if types[typing[id.idx()].idx()].is_unsigned() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_unsigned_rem(
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_signed_rem(
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    }
                }
                BinaryOperator::LT => {
                    if left.get_type().is_float_type() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_float_compare(
                                    FloatPredicate::OLT,
                                    left.into_float_value(),
                                    right.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else if types[typing[id.idx()].idx()].is_unsigned() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_compare(
                                    IntPredicate::ULT,
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_compare(
                                    IntPredicate::SLT,
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    }
                }
                BinaryOperator::LTE => {
                    if left.get_type().is_float_type() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_float_compare(
                                    FloatPredicate::OLE,
                                    left.into_float_value(),
                                    right.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else if types[typing[id.idx()].idx()].is_unsigned() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_compare(
                                    IntPredicate::ULE,
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_compare(
                                    IntPredicate::SLE,
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    }
                }
                BinaryOperator::GT => {
                    if left.get_type().is_float_type() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_float_compare(
                                    FloatPredicate::OGT,
                                    left.into_float_value(),
                                    right.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else if types[typing[id.idx()].idx()].is_unsigned() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_compare(
                                    IntPredicate::UGT,
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_compare(
                                    IntPredicate::SGT,
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    }
                }
                BinaryOperator::GTE => {
                    if left.get_type().is_float_type() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_float_compare(
                                    FloatPredicate::OGE,
                                    left.into_float_value(),
                                    right.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else if types[typing[id.idx()].idx()].is_unsigned() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_compare(
                                    IntPredicate::UGE,
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_compare(
                                    IntPredicate::SGE,
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    }
                }
                BinaryOperator::EQ => {
                    if left.get_type().is_float_type() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_float_compare(
                                    FloatPredicate::OEQ,
                                    left.into_float_value(),
                                    right.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_compare(
                                    IntPredicate::EQ,
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    }
                }
                BinaryOperator::NE => {
                    if left.get_type().is_float_type() {
                        values.insert(
                            id,
                            llvm_builder
                                .build_float_compare(
                                    FloatPredicate::ONE,
                                    left.into_float_value(),
                                    right.into_float_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    } else {
                        values.insert(
                            id,
                            llvm_builder
                                .build_int_compare(
                                    IntPredicate::NE,
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "",
                                )
                                .unwrap()
                                .as_basic_value_enum(),
                        );
                    }
                }
                BinaryOperator::Or => {
                    values.insert(
                        id,
                        llvm_builder
                            .build_or(left.into_int_value(), right.into_int_value(), "")
                            .unwrap()
                            .as_basic_value_enum(),
                    );
                }
                BinaryOperator::And => {
                    values.insert(
                        id,
                        llvm_builder
                            .build_and(left.into_int_value(), right.into_int_value(), "")
                            .unwrap()
                            .as_basic_value_enum(),
                    );
                }
                BinaryOperator::Xor => {
                    values.insert(
                        id,
                        llvm_builder
                            .build_xor(left.into_int_value(), right.into_int_value(), "")
                            .unwrap()
                            .as_basic_value_enum(),
                    );
                }
                BinaryOperator::LSh => {
                    values.insert(
                        id,
                        llvm_builder
                            .build_left_shift(left.into_int_value(), right.into_int_value(), "")
                            .unwrap()
                            .as_basic_value_enum(),
                    );
                }
                BinaryOperator::RSh => {
                    values.insert(
                        id,
                        llvm_builder
                            .build_right_shift(
                                left.into_int_value(),
                                right.into_int_value(),
                                !types[typing[id.idx()].idx()].is_unsigned(),
                                "",
                            )
                            .unwrap()
                            .as_basic_value_enum(),
                    );
                }
            }
        }
        _ => todo!(),
    }
}

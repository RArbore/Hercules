extern crate bitvec;
extern crate hercules_ir;

use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt::Write;
use std::iter::zip;

use self::bitvec::prelude::*;

use self::hercules_ir::def_use::*;
use self::hercules_ir::ir::*;

/*
 * This CPU backend is a rewrite of the original CPU alpha backend. It accounts
 * for many changes in the IR designed to make lowering, optimization, and code
 * generation easier.
 */

#[derive(Debug)]
struct LLVMBlock {
    header: String,
    phis: String,
    data: String,
    terminator: String,
}

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
    fork_join_maps: &Vec<HashMap<NodeID, NodeID>>,
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
                // Array types becomes pointers. The element type and dynamic
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
            Constant::Integer8(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::Integer16(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::Integer32(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::Integer64(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::UnsignedInteger8(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::UnsignedInteger16(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::UnsignedInteger32(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::UnsignedInteger64(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::Float32(val) => llvm_constants[id.idx()] = format!("{}", val),
            Constant::Float64(val) => llvm_constants[id.idx()] = format!("{}", val),
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

    // Step 3: render dynamic constants into LLVM IR.
    let mut llvm_dynamic_constants = vec!["".to_string(); dynamic_constants.len()];
    for id in (0..dynamic_constants.len()).map(DynamicConstantID::new) {
        match &dynamic_constants[id.idx()] {
            DynamicConstant::Constant(val) => llvm_dynamic_constants[id.idx()] = format!("{}", val),
            DynamicConstant::Parameter(num) => {
                llvm_dynamic_constants[id.idx()] = format!("%dc{}", num)
            }
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
        let fork_join_map = &fork_join_maps[function_idx];
        let array_allocations = &array_allocations[function_idx];

        // Step 4.1: emit function signature.
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

        // Step 4.2: emit basic blocks. A node represents a basic block if its
        // entry in the basic blocks vector points to itself. Each basic block
        // is created as four strings: the block header, the block's phis, the
        // block's data computations, and the block's terminator instruction.
        let mut llvm_bbs = HashMap::new();
        for id in (0..function.nodes.len()).map(NodeID::new) {
            if bb[id.idx()] == id {
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

        // Step 4.3: emit nodes. Nodes are emitted into basic blocks separately
        // as nodes are not necessarily emitted in order. Assemble worklist of
        // nodes, starting as reverse post order of nodes. For non-phi and non-
        // reduce nodes, only emit once all data uses are emitted. In addition,
        // consider additional anti-dependence edges from read to write nodes.
        let mut visited = bitvec![u8, Lsb0; 0; function.nodes.len()];
        let mut worklist = VecDeque::from(reverse_postorder.clone());
        while let Some(id) = worklist.pop_front() {
            if !(function.nodes[id.idx()].is_phi() || function.nodes[id.idx()].is_reduce())
                && !get_uses(&function.nodes[id.idx()])
                    .as_ref()
                    .into_iter()
                    .chain(
                        antideps.iter().filter_map(
                            |(read, write)| if id == *write { Some(read) } else { None },
                        ),
                    )
                    .all(|x| function.nodes[x.idx()].is_control() || visited[x.idx()])
            {
                // Skip emitting node if it's not a phi or reducee node and if
                // its data uses are not emitted yet.
                worklist.push_back(id);
            } else {
                // Once all of the data dependencies for this node are emitted,
                // this node can be emitted.
                emit_llvm_for_node(
                    id,
                    function,
                    typing,
                    types,
                    dynamic_constants,
                    bb,
                    def_use,
                    fork_join_map,
                    array_allocations,
                    &mut llvm_bbs,
                    &llvm_types,
                    &llvm_constants,
                    &llvm_dynamic_constants,
                    w,
                )?;
                visited.set(id.idx(), true);
            }
        }

        // Step 4.4: put basic blocks in order.
        for node in reverse_postorder {
            if bb[node.idx()] == *node {
                write!(
                    w,
                    "{}{}{}{}",
                    llvm_bbs[node].header,
                    llvm_bbs[node].phis,
                    llvm_bbs[node].data,
                    llvm_bbs[node].terminator
                )?;
            }
        }

        // Step 4.5: close function.
        write!(w, "}}\n")?;
    }

    Ok(())
}

/*
 * Emit LLVM implementing a single node.
 */
fn emit_llvm_for_node<W: Write>(
    id: NodeID,
    function: &Function,
    typing: &Vec<TypeID>,
    types: &Vec<Type>,
    dynamic_constants: &Vec<DynamicConstant>,
    bb: &Vec<NodeID>,
    def_use: &ImmutableDefUseMap,
    fork_join_map: &HashMap<NodeID, NodeID>,
    array_allocations: &(Vec<Vec<DynamicConstantID>>, HashMap<NodeID, usize>),
    llvm_bbs: &mut HashMap<NodeID, LLVMBlock>,
    llvm_types: &Vec<String>,
    llvm_constants: &Vec<String>,
    llvm_dynamic_constants: &Vec<String>,
    w: &mut W,
) -> std::fmt::Result {
    // Helper to get the virtual register corresponding to a node. Overload to
    // also emit constants, dynamic constants, parameters, and thread IDs.
    // Override writes to use previous non-store pointer, since emitting a store
    // doesn't create a new pointer virtual register we should use.
    let virtual_register = |mut id: NodeID| {
        while let Node::Write {
            collect,
            indices: _,
            data: _,
        } = &function.nodes[id.idx()]
        {
            id = *collect;
        }

        match function.nodes[id.idx()] {
            Node::Constant { id } => llvm_constants[id.idx()].clone(),
            Node::DynamicConstant { id } => llvm_dynamic_constants[id.idx()].clone(),
            Node::Parameter { index } => format!("%p{}", index),
            Node::ThreadID { control } => format!("%v{}", control.idx()),
            _ => format!("%v{}", id.idx()),
        }
    };
    let type_of = |id: NodeID| format!("{}", llvm_types[typing[id.idx()].idx()]);
    let normal_value = |id: NodeID| format!("{} {}", type_of(id), virtual_register(id));

    // Helper to emit code to index into an aggregate, and return a pointer to
    // the indexed element. This only works when the first aggregate is an
    // array.
    let mut generate_index_code = |collect: NodeID, indices: &[Index]| {
        let extents = types[typing[collect.idx()].idx()].try_extents().unwrap();
        let position = indices[0].try_position().unwrap();
        for (idx, (extent, index_id)) in zip(extents, position).enumerate() {
            if idx == 0 {
                llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data += &format!(
                    "  %index.{}.acc.add.{} = add {}, {}\n",
                    id.idx(),
                    idx,
                    normal_value(*index_id),
                    0,
                );
            } else {
                llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data += &format!(
                    "  %index.{}.acc.mul.{} = mul {}, %index.acc.add.{}\n",
                    id.idx(),
                    idx,
                    llvm_dynamic_constants[extent.idx()],
                    idx - 1,
                );
                llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data += &format!(
                    "  %index.{}.acc.add.{} = add {}, %index.acc.mul.{}\n",
                    id.idx(),
                    idx,
                    normal_value(*index_id),
                    idx,
                );
            }
        }
        llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data += &format!(
            "  %index.{}.ptr = getelementptr {}, {}, i64 %index.{}.acc.add.{}",
            id.idx(),
            llvm_types[types[typing[collect.idx()].idx()]
                .try_element_type()
                .unwrap()
                .idx()],
            normal_value(collect),
            id.idx(),
            extents.len() - 1
        );
        for index in &indices[1..] {
            llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data +=
                &format!(", i32 {}", index.try_field().unwrap());
        }
        llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data += "\n";
    };

    // Emit code depending on the type of the node.
    match function.nodes[id.idx()] {
        Node::Start | Node::Region { preds: _ } => {
            let successor = def_use
                .get_users(id)
                .iter()
                .filter(|id| function.nodes[id.idx()].is_strictly_control())
                .next()
                .unwrap();
            llvm_bbs.get_mut(&bb[id.idx()]).unwrap().terminator =
                format!("  br label %bb_{}\n", successor.idx());
        }
        Node::If { control: _, cond } => {
            let successors = def_use.get_users(id);
            let rev = if let Node::Read {
                collect: _,
                indices,
            } = &function.nodes[successors[0].idx()]
            {
                indices[0] != Index::Control(0)
            } else {
                panic!()
            };
            llvm_bbs.get_mut(&bb[id.idx()]).unwrap().terminator = format!(
                "  br {}, label %bb_{}, label %bb_{}\n",
                normal_value(cond),
                successors[(!rev) as usize].idx(),
                successors[rev as usize].idx()
            );
        }
        Node::Fork { control, factor: _ } => {
            // Calculate the join and successor.
            let join = fork_join_map[&id];
            let successor = def_use
                .get_users(id)
                .iter()
                .filter(|id| function.nodes[id.idx()].is_strictly_control())
                .next()
                .unwrap();

            // Need to create phi node for the loop index.
            llvm_bbs.get_mut(&bb[id.idx()]).unwrap().phis += &format!(
                "  {} = phi i64 [ 0, %bb_{} ], [ %fork.{}.inc, %bb_{} ]\n",
                virtual_register(id),
                control.idx(),
                id.idx(),
                join.idx(),
            );
            llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data += &format!(
                "  %fork.{}.inc = add i64 1, {}\n",
                id.idx(),
                virtual_register(id),
            );
            llvm_bbs.get_mut(&bb[id.idx()]).unwrap().terminator =
                format!("  br label %bb_{}\n", successor.idx());
        }
        Node::Join { control } => {
            // Get the fork, its factor, and the successor to this join.
            let fork_id = if let Type::Control(factors) = &types[typing[control.idx()].idx()] {
                *factors.last().unwrap()
            } else {
                panic!()
            };
            let factor = if let Node::Fork { control: _, factor } = &function.nodes[fork_id.idx()] {
                *factor
            } else {
                panic!()
            };
            let successor = def_use
                .get_users(id)
                .iter()
                .filter(|id| function.nodes[id.idx()].is_strictly_control())
                .next()
                .unwrap();

            // Form the bottom of the loop. We need to branch between the
            // successor and the fork.
            llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data += &format!(
                "  %join.{}.cond = icmp ult i64 %fork.{}.inc, {}\n",
                id.idx(),
                fork_id.idx(),
                llvm_dynamic_constants[factor.idx()],
            );
            llvm_bbs.get_mut(&bb[id.idx()]).unwrap().terminator = format!(
                "  br i1 %join.{}.cond, label %bb_{}, label %bb_{}\n",
                id.idx(),
                fork_id.idx(),
                successor.idx(),
            );
        }
        Node::Phi {
            control: _,
            ref data,
        } => {
            let pred_ids = get_uses(&function.nodes[bb[id.idx()].idx()]);
            let mut iter = zip(data.iter(), pred_ids.as_ref().iter());
            let (first_data, first_control) = iter.next().unwrap();
            let mut phi = format!(
                "  {} = phi {} [ {}, %bb_{} ]",
                virtual_register(id),
                type_of(id),
                virtual_register(*first_data),
                bb[first_control.idx()].idx()
            );
            for (data, control) in iter {
                phi += &format!(
                    ", [ {}, %bb_{} ]",
                    virtual_register(*data),
                    bb[control.idx()].idx()
                );
            }
            phi += "\n";
            llvm_bbs.get_mut(&bb[id.idx()]).unwrap().phis += &phi;
        }
        // No code needs to get emitted for thread ID nodes - the loop index is
        // emitted in the fork.
        Node::ThreadID { control: _ } => {}
        Node::Reduce {
            control,
            init,
            reduct,
        } => {
            // Figure out the fork corresponding to the associated join.
            let fork_id = if let Node::Join { control } = function.nodes[control.idx()] {
                if let Type::Control(factors) = &types[typing[control.idx()].idx()] {
                    *factors.last().unwrap()
                } else {
                    panic!()
                }
            } else {
                panic!()
            };

            // Figure out the fork's predecessor.
            let pred = if let Node::Fork { control, factor: _ } = function.nodes[fork_id.idx()] {
                control
            } else {
                panic!()
            };

            // Create the phi node for the reduction.
            llvm_bbs.get_mut(&bb[fork_id.idx()]).unwrap().phis += &format!(
                "  {} = phi {} [ {}, %bb_{} ], [ {}, %bb_{} ]\n",
                virtual_register(id),
                type_of(id),
                virtual_register(init),
                pred.idx(),
                virtual_register(reduct),
                control.idx(),
            );
        }
        Node::Return { control: _, data } => {
            llvm_bbs.get_mut(&bb[id.idx()]).unwrap().terminator =
                format!("  ret {}\n", normal_value(data));
        }
        // No code needs to get emitted for parameters, constants, or dynamic
        // constants - these are just specific virtual registers or constant
        // values.
        Node::Parameter { index: _ } => {}
        Node::Constant { id: _ } => {}
        Node::DynamicConstant { id: _ } => {}
        Node::Unary { input, op } => match op {
            UnaryOperator::Not => todo!(),
            UnaryOperator::Neg => {
                if types[typing[input.idx()].idx()].is_float() {
                    llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data += &format!(
                        "  {} = fneg {}\n",
                        virtual_register(id),
                        normal_value(input)
                    );
                } else {
                    todo!()
                }
            }
        },
        Node::Binary { left, right, op } => {
            let opcode = match op {
                BinaryOperator::Add => {
                    if types[typing[left.idx()].idx()].is_float() {
                        "fadd"
                    } else {
                        "add"
                    }
                }
                BinaryOperator::Sub => {
                    if types[typing[left.idx()].idx()].is_float() {
                        "fsub"
                    } else {
                        "sub"
                    }
                }
                BinaryOperator::Mul => {
                    if types[typing[left.idx()].idx()].is_float() {
                        "fmul"
                    } else {
                        "mul"
                    }
                }
                BinaryOperator::Div => {
                    if types[typing[left.idx()].idx()].is_float() {
                        "fdiv"
                    } else if types[typing[left.idx()].idx()].is_unsigned() {
                        "udiv"
                    } else {
                        "sdiv"
                    }
                }
                BinaryOperator::Rem => {
                    if types[typing[left.idx()].idx()].is_float() {
                        "frem"
                    } else if types[typing[left.idx()].idx()].is_unsigned() {
                        "urem"
                    } else {
                        "srem"
                    }
                }
                BinaryOperator::LT => {
                    if types[typing[left.idx()].idx()].is_float() {
                        "fcmp olt"
                    } else if types[typing[left.idx()].idx()].is_unsigned() {
                        "icmp ult"
                    } else {
                        "icmp slt"
                    }
                }
                BinaryOperator::LTE => {
                    if types[typing[left.idx()].idx()].is_float() {
                        "fcmp ole"
                    } else if types[typing[left.idx()].idx()].is_unsigned() {
                        "icmp ule"
                    } else {
                        "icmp sle"
                    }
                }
                BinaryOperator::GT => {
                    if types[typing[left.idx()].idx()].is_float() {
                        "fcmp ogt"
                    } else if types[typing[left.idx()].idx()].is_unsigned() {
                        "icmp ugt"
                    } else {
                        "icmp sgt"
                    }
                }
                BinaryOperator::GTE => {
                    if types[typing[left.idx()].idx()].is_float() {
                        "fcmp oge"
                    } else if types[typing[left.idx()].idx()].is_unsigned() {
                        "icmp uge"
                    } else {
                        "icmp sge"
                    }
                }
                BinaryOperator::EQ => {
                    if types[typing[left.idx()].idx()].is_float() {
                        "fcmp oeq"
                    } else {
                        "icmp eq"
                    }
                }
                BinaryOperator::NE => {
                    if types[typing[left.idx()].idx()].is_float() {
                        "fcmp one"
                    } else {
                        "icmp ne"
                    }
                }
                BinaryOperator::Or => "or",
                BinaryOperator::And => "and",
                BinaryOperator::Xor => "xor",
                BinaryOperator::LSh => "lsh",
                BinaryOperator::RSh => {
                    if types[typing[left.idx()].idx()].is_unsigned() {
                        "lshr"
                    } else {
                        "ashr"
                    }
                }
            };
            llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data += &format!(
                "  {} = {} {}, {}\n",
                virtual_register(id),
                opcode,
                normal_value(left),
                virtual_register(right),
            );
        }
        Node::Read {
            collect,
            ref indices,
        } => {
            // Read nodes may be projection successors of if or match nodes.
            if function.nodes[collect.idx()].is_strictly_control() {
                let successor = def_use.get_users(id)[0];
                llvm_bbs.get_mut(&bb[id.idx()]).unwrap().terminator =
                    format!("  br label %bb_{}\n", successor.idx());
            } else {
                generate_index_code(collect, indices);
                if types[typing[collect.idx()].idx()].is_array() {
                    llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data += &format!(
                        "  {} = load {}, ptr %index.{}.ptr\n",
                        virtual_register(id),
                        llvm_types[typing[id.idx()].idx()],
                        id.idx()
                    );
                } else {
                    llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data += &format!(
                        "  {} = extractvalue {}",
                        virtual_register(id),
                        normal_value(collect)
                    );
                    for index in indices.iter() {
                        llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data +=
                            &format!(", {}", index.try_field().unwrap());
                    }
                    llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data += "\n";
                }
            }
        }
        Node::Write {
            collect,
            ref indices,
            data,
        } => {
            generate_index_code(collect, indices);
            if types[typing[collect.idx()].idx()].is_array() {
                llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data += &format!(
                    "  store {}, ptr %index.{}.ptr\n",
                    normal_value(data),
                    id.idx()
                );
            } else {
                llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data += &format!(
                    "  {} = insertvalue {}, {}",
                    virtual_register(id),
                    normal_value(collect),
                    normal_value(data)
                );
                for index in indices.iter() {
                    llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data +=
                        &format!(", {}", index.try_field().unwrap());
                }
                llvm_bbs.get_mut(&bb[id.idx()]).unwrap().data += "\n";
            }
        }
        _ => todo!(),
    }

    Ok(())
}

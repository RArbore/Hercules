extern crate bitvec;
extern crate hercules_ir;

use std::collections::HashMap;
use std::collections::VecDeque;
use std::fmt::Write;

use self::bitvec::prelude::*;

use self::hercules_ir::def_use::*;
use self::hercules_ir::ir::*;

/*
 * This CPU backend is a rewrite of the original CPU alpha backend. It accounts
 * for many changes in the IR designed to make lowering, optimization, and code
 * generation easier.
 */

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

    // Step 3: render dynamic constants into LLVM IR.
    let mut llvm_dynamic_constants = vec!["".to_string(); dynamic_constants.len()];
    for id in (0..dynamic_constants.len()).map(DynamicConstantID::new) {
        match &dynamic_constants[id.idx()] {
            DynamicConstant::Constant(val) => {
                llvm_dynamic_constants[id.idx()] = format!("i64 {}", val)
            }
            DynamicConstant::Parameter(num) => {
                llvm_dynamic_constants[id.idx()] = format!("i64 %dc{}", num)
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
        let fork_join_nest = &fork_join_nests[function_idx];
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
        let visited = bitvec![u8, Lsb0; 0; function.nodes.len()];
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
                    .all(|x| function.nodes[x.idx()].is_control() || visited[x.idx()])
            {
                // Skip emitting node if it's not a phi node and if its data
                // uses are not emitted yet.
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
                    fork_join_nest,
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

        // Step 4.4: close function.
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
    fork_join_nest: &HashMap<NodeID, Vec<NodeID>>,
    array_allocations: &(Vec<Vec<DynamicConstantID>>, HashMap<NodeID, usize>),
    llvm_bbs: &mut HashMap<NodeID, LLVMBlock>,
    llvm_types: &Vec<String>,
    llvm_constants: &Vec<String>,
    llvm_dynamic_constants: &Vec<String>,
    w: &mut W,
) -> std::fmt::Result {
    // Helper to get the virtual register corresponding to a node.
    let virtual_register =
        |id: NodeID| format!("{} %v{}", llvm_types[typing[id.idx()].idx()], id.idx());

    match function.nodes[id.idx()] {
        Node::Start | Node::Region { preds: _ } => {
            let successor = def_use
                .get_users(id)
                .iter()
                .filter(|id| function.nodes[id.idx()].is_strictly_control())
                .next()
                .unwrap();
            llvm_bbs.get_mut(&id).unwrap().terminator =
                format!("  br label bb_{}", successor.idx());
        }
        Node::If { control: _, cond } => {
            let successors = def_use.get_users(id);
            let rev = if let Node::Read {
                collect: _,
                indices,
            } = &function.nodes[successors[0].idx()]
            {
                if indices[0] == Index::Control(0) {
                    false
                } else {
                    true
                }
            } else {
                panic!()
            };
            if rev {
                llvm_bbs.get_mut(&id).unwrap().terminator = format!(
                    "  br {}, label bb_{}, label bb_{}",
                    virtual_register(cond),
                    successors[0].idx(),
                    successors[1].idx()
                );
            } else {
                llvm_bbs.get_mut(&id).unwrap().terminator = format!(
                    "  br {}, label bb_{}, label bb_{}",
                    virtual_register(cond),
                    successors[1].idx(),
                    successors[0].idx()
                );
            }
        }
        _ => todo!(),
    }

    Ok(())
}

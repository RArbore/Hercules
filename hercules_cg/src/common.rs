extern crate hercules_ir;

use std::collections::BTreeSet;

use self::hercules_ir::*;

/*
 * Pretty much all of the codegen functions need to take in some large subset of
 * IR structures, analysis results, and global pieces of information. Package
 * them all in this struct, and make all the codegen functions members of this
 * struct to cut down on the number of function arguments. This structure
 * shouldn't be modified after creation.
 */
pub(crate) struct FunctionContext<'a> {
    pub(crate) function: &'a Function,
    pub(crate) types: &'a Vec<Type>,
    pub(crate) constants: &'a Vec<Constant>,
    pub(crate) dynamic_constants: &'a Vec<DynamicConstant>,
    pub(crate) def_use: &'a ImmutableDefUseMap,
    pub(crate) reverse_postorder: &'a Vec<NodeID>,
    pub(crate) typing: &'a Vec<TypeID>,
    pub(crate) control_subgraph: &'a Subgraph,
    pub(crate) antideps: &'a Vec<(NodeID, NodeID)>,
    pub(crate) bbs: &'a Vec<NodeID>,
    pub(crate) plan: &'a Plan,
    pub(crate) llvm_types: &'a Vec<String>,
    pub(crate) llvm_constants: &'a Vec<String>,
    pub(crate) llvm_dynamic_constants: &'a Vec<String>,
    pub(crate) partitions_inverted_map: Vec<Vec<NodeID>>,
}

impl<'a> FunctionContext<'a> {
    /*
     * Find data inputs to a partition.
     */
    pub(crate) fn partition_data_inputs(&self, partition_id: PartitionID) -> Vec<NodeID> {
        let partition = &self.partitions_inverted_map[partition_id.idx()];

        let mut data_inputs: Vec<NodeID> = partition
            .iter()
            .map(|id| {
                // For each node in the partition, filter out the uses that are
                // data nodes and are in a different partition.
                get_uses(&self.function.nodes[id.idx()])
                    .as_ref()
                    .into_iter()
                    .filter(|id| {
                        !self.function.nodes[id.idx()].is_control()
                            && self.plan.partitions[id.idx()] != partition_id
                    })
                    .map(|x| *x)
                    .collect::<Vec<NodeID>>()
            })
            // Collect all such uses across the whole partition.
            .flatten()
            .collect();

        // Inputs and outputs of partitions need to be sorted so datams don't
        // get mixed up.
        data_inputs.sort();
        data_inputs
    }

    /*
     * Find data outputs of a partition.
     */
    pub(crate) fn partition_data_outputs(&self, partition_id: PartitionID) -> Vec<NodeID> {
        let partition = &self.partitions_inverted_map[partition_id.idx()];

        let mut data_outputs: Vec<NodeID> = partition
            .iter()
            .filter(|id| {
                // For each data node in the partition, check if it has any uses
                // outside its partition. Users can be control or data nodes.
                !self.function.nodes[id.idx()].is_control()
                    && self
                        .def_use
                        .get_users(**id)
                        .as_ref()
                        .into_iter()
                        .filter(|id| self.plan.partitions[id.idx()] != partition_id)
                        .map(|x| *x)
                        .count()
                        > 0
            })
            .map(|x| *x)
            // If this partition contains a return node, the data input of that
            // node is a data output.
            .chain(partition.iter().filter_map(|id| {
                if let Node::Return { control: _, data } = self.function.nodes[id.idx()] {
                    Some(data)
                } else {
                    None
                }
            }))
            .collect();

        // Inputs and outputs of partitions need to be sorted so datams don't
        // get mixed up.
        data_outputs.sort();
        data_outputs
    }

    /*
     * Find control nodes that might return from a partition.
     */
    pub(crate) fn partition_control_returns(&self, partition_id: PartitionID) -> Vec<NodeID> {
        let partition = &self.partitions_inverted_map[partition_id.idx()];

        partition
            .iter()
            .filter(|id| {
                // For each control node in the partition, check if it has any
                // users outside its partition. Users can be control nodes - if
                // a user in a different partition is a data node, then the
                // partition is malformed. Return nodes are also unconditionally
                // a control return of this partition.
                self.function.nodes[id.idx()].is_control()
                    && (self.function.nodes[id.idx()].is_return()
                        || self
                            .def_use
                            .get_users(**id)
                            .as_ref()
                            .into_iter()
                            .filter(|id| {
                                // Users of control nodes can only be data nodes
                                // if they are in the same partition as the
                                // control node. Only control users may be in a
                                // different partition.
                                assert!(
                                    self.function.nodes[id.idx()].is_control()
                                        || self.plan.partitions[id.idx()] == partition_id
                                );
                                self.plan.partitions[id.idx()] != partition_id
                            })
                            .map(|x| *x)
                            .count()
                            > 0)
            })
            .map(|x| *x)
            .collect()
    }

    /*
     * Find control successors of a given partition. A partition cannot be a
     * control successor of itself, since a self-cycle is represented as control
     * flow within a partiion. In other words, the graph of control flow between
     * partitions is free of self-loops (an edge connecting a partition to
     * itself).
     */
    pub(crate) fn partition_control_successors(
        &self,
        partition_id: PartitionID,
    ) -> Vec<PartitionID> {
        let partition = &self.partitions_inverted_map[partition_id.idx()];

        let mut partitions: Vec<PartitionID> = partition
            .iter()
            // Only consider nodes in other partitions that are successors of
            // control nodes. These are necessarily other control nodes.
            .filter(|id| self.function.nodes[id.idx()].is_control())
            .map(|id| {
                // Get the partitions (that are not this partition) of successor
                // nodes of control nodes.
                self.def_use
                    .get_users(*id)
                    .as_ref()
                    .into_iter()
                    .map(|id| self.plan.partitions[id.idx()])
                    .filter(|id| *id != partition_id)
            })
            // We want a flat list of all such partitions.
            .flatten()
            .collect();

        // We only want one copy of the ID per partition.
        partitions.dedup();
        partitions
    }

    /*
     * Find all uses of function parameters in this partition. Collect as a
     * BTreeSet so that iterating the parameter indices is sorted.
     */
    pub(crate) fn partition_function_parameters(
        &self,
        partition_id: PartitionID,
    ) -> BTreeSet<usize> {
        self.partitions_inverted_map[partition_id.idx()]
            .iter()
            .filter_map(|id| match self.function.nodes[id.idx()] {
                Node::Parameter { index } => Some(index),
                _ => None,
            })
            .collect()
    }

    /*
     * Find all uses of array constants in this partition. Collect as a BTreeSet
     * so that iterating the IDs is sorted.
     */
    pub(crate) fn partition_array_constants(
        &self,
        partition_id: PartitionID,
    ) -> BTreeSet<ConstantID> {
        self.partitions_inverted_map[partition_id.idx()]
            .iter()
            .filter_map(|id| match self.function.nodes[id.idx()] {
                Node::Constant { id } => Some(id),
                _ => None,
            })
            .filter_map(|id| match self.constants[id.idx()] {
                Constant::Array(_, _) => Some(id),
                Constant::Zero(ty_id) => {
                    if self.types[ty_id.idx()].is_array() {
                        Some(id)
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .collect()
    }

    /*
     * Find all uses of dynamic constants in this partition. Collect as a
     * BTreeSet so that iterating the IDs is sorted.
     */
    pub(crate) fn partition_dynamic_constants(
        &self,
        partition_id: PartitionID,
    ) -> BTreeSet<DynamicConstantID> {
        self.partitions_inverted_map[partition_id.idx()]
            .iter()
            .filter_map(|id| match self.function.nodes[id.idx()] {
                Node::Fork { control: _, factor } => Some(factor),
                Node::DynamicConstant { id } => Some(id),
                _ => None,
            })
            .collect()
    }

    /*
     * Calculate the reverse postorder of just this partition.
     */
    pub(crate) fn partition_reverse_postorder(&self, partition_id: PartitionID) -> Vec<NodeID> {
        self.reverse_postorder
            .iter()
            .filter(|id| self.plan.partitions[id.idx()] == partition_id)
            .map(|x| *x)
            .collect()
    }
}

/*
 * When emitting individual nodes in the partition codegen functions, a bunch of
 * partition analysis results are needed. Package them all in this struct, and
 * make all of the subroutines of the top level partition codegen functions
 * members of this struct to cut down on the number of function arguments. This
 * structure shouldn't be modified after creation.
 */
pub(crate) struct PartitionContext<'a> {
    pub(crate) function: &'a FunctionContext<'a>,
    pub(crate) partition_id: PartitionID,
    pub(crate) data_inputs: Vec<NodeID>,
    pub(crate) data_outputs: Vec<NodeID>,
    pub(crate) control_returns: Vec<NodeID>,
    pub(crate) control_successors: Vec<PartitionID>,
    pub(crate) function_parameters: BTreeSet<usize>,
    pub(crate) array_constants: BTreeSet<ConstantID>,
    pub(crate) dynamic_constants: BTreeSet<DynamicConstantID>,
    pub(crate) reverse_postorder: Vec<NodeID>,
    pub(crate) input_types: Vec<TypeID>,
    pub(crate) num_data_inputs: u32,
    pub(crate) num_param_inputs: u32,
    pub(crate) num_array_constant_inputs: u32,
    pub(crate) num_dynamic_constant_inputs: u32,
    pub(crate) return_type: Type,
}

impl<'a> PartitionContext<'a> {
    pub(crate) fn new(function: &'a FunctionContext<'a>, partition_id: PartitionID) -> Self {
        let data_inputs = function.partition_data_inputs(partition_id);
        let data_outputs = function.partition_data_outputs(partition_id);
        let control_returns = function.partition_control_returns(partition_id);
        let control_successors = function.partition_control_successors(partition_id);
        let function_parameters = function.partition_function_parameters(partition_id);
        let array_constants = function.partition_array_constants(partition_id);
        let dynamic_constants = function.partition_dynamic_constants(partition_id);
        let reverse_postorder = function.partition_reverse_postorder(partition_id);

        // The arguments are the input data nodes, plus any used function
        // parameters, plus used array constants, plus used dynamic constants.
        let u64_ty_id = TypeID::new(
            function
                .types
                .iter()
                .position(|ty| *ty == Type::UnsignedInteger64)
                .unwrap(),
        );
        let input_data_types = data_inputs.iter().map(|id| function.typing[id.idx()]);
        let num_data_inputs = data_inputs.len() as u32;
        let function_parameter_types = function_parameters
            .iter()
            .map(|index| function.function.param_types[*index]);
        let num_param_inputs = function_parameters.len() as u32;
        let array_constant_types =
            array_constants
                .iter()
                .map(|id| match function.constants[id.idx()] {
                    Constant::Array(ty_id, _) => ty_id,
                    Constant::Zero(ty_id) => ty_id,
                    _ => panic!(),
                });
        let num_array_constant_inputs = array_constants.len() as u32;
        let dynamic_constant_types = std::iter::repeat(u64_ty_id).take(dynamic_constants.len());
        let num_dynamic_constant_inputs = dynamic_constants.len() as u32;
        let input_types = input_data_types
            .chain(function_parameter_types)
            .chain(array_constant_types)
            .chain(dynamic_constant_types)
            .collect();

        // The return struct contains all of the data outputs, plus control
        // information if there are multiple successor partitions. The control
        // information is used by the orchestration code to implement control
        // flow between partitions.
        let multiple_control_successors = control_successors.len() > 1;
        let output_data_types = data_outputs.iter().map(|id| function.typing[id.idx()]);
        let return_type = if multiple_control_successors {
            Type::Product(
                output_data_types
                    .chain(std::iter::once(u64_ty_id))
                    .collect(),
            )
        } else {
            Type::Product(output_data_types.collect())
        };

        PartitionContext {
            function,
            partition_id,
            data_inputs,
            data_outputs,
            control_returns,
            control_successors,
            function_parameters,
            array_constants,
            dynamic_constants,
            reverse_postorder,
            input_types,
            num_data_inputs,
            num_param_inputs,
            num_array_constant_inputs,
            num_dynamic_constant_inputs,
            return_type,
        }
    }

    pub(crate) fn function_to_partition_parameter(&self, index: usize) -> usize {
        self.num_data_inputs as usize + index
    }
}

/*
 * Types, constants, and dynamic constants are fairly simple to translate into
 * LLVM IR.
 */

pub(crate) fn generate_type_string(ty: &Type, llvm_types: &Vec<String>) -> String {
    match ty {
        Type::Control(_) => {
            // Later, we create virtual registers corresponding to fork nodes of
            // type i64, so we need the "type" of the fork node to be i64.
            "i64".to_string()
        }
        Type::Boolean => "i1".to_string(),
        Type::Integer8 | Type::UnsignedInteger8 => "i8".to_string(),
        Type::Integer16 | Type::UnsignedInteger16 => "i16".to_string(),
        Type::Integer32 | Type::UnsignedInteger32 => "i32".to_string(),
        Type::Integer64 | Type::UnsignedInteger64 => "i64".to_string(),
        Type::Float32 => "float".to_string(),
        Type::Float64 => "double".to_string(),
        // Because we traverse in bottom-up order, we can assume that the LLVM
        // types for children types are already computed.
        Type::Product(fields) => {
            let mut iter = fields.iter();
            if let Some(first) = iter.next() {
                iter.fold("{".to_string() + &llvm_types[first.idx()], |s, f| {
                    s + ", " + &llvm_types[f.idx()]
                }) + "}"
            } else {
                "{}".to_string()
            }
        }
        Type::Array(_, _) => {
            // Array types becomes pointers. The element type and dynamic
            // constant bounds characterize the access code we generate later,
            // not the type itself.
            "ptr".to_string()
        }
        Type::Summation(_) => todo!(),
    }
}

pub(crate) fn generate_type_strings(module: &Module) -> Vec<String> {
    // Render types into LLVM IR. This requires translating from our interning
    // structures to LLVM types. We can't just blow through the types vector,
    // since a type may reference a type ID ahead of it in the vector. Instead,
    // iterate types in a bottom up order with respect to the type intern DAGs.
    let mut llvm_types = vec!["".to_string(); module.types.len()];
    for id in module.types_bottom_up() {
        llvm_types[id.idx()] = generate_type_string(&module.types[id.idx()], &llvm_types);
    }

    llvm_types
}

pub(crate) fn generate_constant_string(
    cons_id: ConstantID,
    cons: &Constant,
    tys: &Vec<Type>,
    llvm_constants: &Vec<String>,
) -> String {
    match cons {
        Constant::Boolean(val) => {
            if *val {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        Constant::Integer8(val) => format!("{}", val),
        Constant::Integer16(val) => format!("{}", val),
        Constant::Integer32(val) => format!("{}", val),
        Constant::Integer64(val) => format!("{}", val),
        Constant::UnsignedInteger8(val) => format!("{}", val),
        Constant::UnsignedInteger16(val) => format!("{}", val),
        Constant::UnsignedInteger32(val) => format!("{}", val),
        Constant::UnsignedInteger64(val) => format!("{}", val),
        Constant::Float32(val) => {
            if val.fract() == 0.0 {
                format!("{}.0", val)
            } else {
                format!("{}", val)
            }
        }
        Constant::Float64(val) => {
            if val.fract() == 0.0 {
                format!("{}.0", val)
            } else {
                format!("{}", val)
            }
        }
        Constant::Product(_, fields) => {
            let mut iter = fields.iter();
            if let Some(first) = iter.next() {
                iter.fold("{".to_string() + &llvm_constants[first.idx()], |s, f| {
                    s + ", " + &llvm_constants[f.idx()]
                }) + "}"
            } else {
                "{}".to_string()
            }
        }
        Constant::Array(_, _) => format!("%arr.{}", cons_id.idx()),
        Constant::Summation(_, _, _) => todo!(),
        Constant::Zero(ty_id) => render_zero_constant(cons_id, &tys[ty_id.idx()]),
    }
}

pub(crate) fn generate_constant_strings(module: &Module) -> Vec<String> {
    // Render constants into LLVM IR. This is done in a very similar manner as
    // types.
    let mut llvm_constants = vec!["".to_string(); module.constants.len()];
    for id in module.constants_bottom_up() {
        llvm_constants[id.idx()] = generate_constant_string(
            id,
            &module.constants[id.idx()],
            &module.types,
            &llvm_constants,
        );
    }

    llvm_constants
}

pub(crate) fn generate_dynamic_constant_strings(module: &Module) -> Vec<String> {
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

fn render_zero_constant(cons_id: ConstantID, ty: &Type) -> String {
    // Extra logic for zero constants, since arrays are set to 0 by the runtime.
    match ty {
        Type::Array(_, _) => format!("%arr.{}", cons_id.idx()),
        Type::Summation(_) => todo!(),
        _ => "zeroinitializer".to_string(),
    }
}

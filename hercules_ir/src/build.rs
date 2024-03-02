use std::collections::HashMap;

use crate::*;

/*
 * The builder provides a clean API for programatically creating IR modules.
 * The main function of the builder is to intern various parts of the IR.
 */
#[derive(Debug, Default)]
pub struct Builder<'a> {
    // Intern function names.
    function_ids: HashMap<&'a str, FunctionID>,

    // Intern types, constants, and dynamic constants on a per-module basis.
    interned_types: HashMap<Type, TypeID>,
    interned_constants: HashMap<Constant, ConstantID>,
    interned_dynamic_constants: HashMap<DynamicConstant, DynamicConstantID>,

    // For product, summation, and array constant creation, it's useful to know
    // the type of each constant.
    constant_types: Vec<TypeID>,

    // The module being built.
    module: Module,
}

/*
 * Since the builder doesn't provide string names for nodes, we need a different
 * mechanism for allowing one to allocate node IDs before actually creating the
 * node. This is required since there may be loops in the flow graph. We achieve
 * this using NodeBuilders. Allocating a NodeBuilder allocates a Node ID, and
 * the NodeBuilder can be later used to actually build an IR node.
 */
#[derive(Debug)]
pub struct NodeBuilder {
    id: NodeID,
    function_id: FunctionID,
    node: Node,
}

/*
 * The IR builder may return errors when used incorrectly.
 */
type BuilderResult<T> = Result<T, String>;

impl<'a> Builder<'a> {
    fn intern_type(&mut self, ty: Type) -> TypeID {
        if let Some(id) = self.interned_types.get(&ty) {
            *id
        } else {
            let id = TypeID::new(self.interned_types.len());
            self.interned_types.insert(ty.clone(), id);
            self.module.types.push(ty);
            id
        }
    }

    fn intern_constant(&mut self, cons: Constant, ty: TypeID) -> ConstantID {
        if let Some(id) = self.interned_constants.get(&cons) {
            *id
        } else {
            let id = ConstantID::new(self.interned_constants.len());
            self.interned_constants.insert(cons.clone(), id);
            self.module.constants.push(cons);
            self.constant_types.push(ty);
            id
        }
    }

    fn intern_dynamic_constant(&mut self, dyn_cons: DynamicConstant) -> DynamicConstantID {
        if let Some(id) = self.interned_dynamic_constants.get(&dyn_cons) {
            *id
        } else {
            let id = DynamicConstantID::new(self.interned_dynamic_constants.len());
            self.interned_dynamic_constants.insert(dyn_cons.clone(), id);
            self.module.dynamic_constants.push(dyn_cons);
            id
        }
    }

    pub fn create() -> Self {
        Self::default()
    }

    pub fn finish(self) -> Module {
        self.module
    }

    pub fn create_type_bool(&mut self) -> TypeID {
        self.intern_type(Type::Boolean)
    }

    pub fn create_type_i8(&mut self) -> TypeID {
        self.intern_type(Type::Integer8)
    }

    pub fn create_type_i16(&mut self) -> TypeID {
        self.intern_type(Type::Integer16)
    }

    pub fn create_type_i32(&mut self) -> TypeID {
        self.intern_type(Type::Integer32)
    }

    pub fn create_type_i64(&mut self) -> TypeID {
        self.intern_type(Type::Integer64)
    }

    pub fn create_type_u8(&mut self) -> TypeID {
        self.intern_type(Type::UnsignedInteger8)
    }

    pub fn create_type_u16(&mut self) -> TypeID {
        self.intern_type(Type::UnsignedInteger16)
    }

    pub fn create_type_u32(&mut self) -> TypeID {
        self.intern_type(Type::UnsignedInteger32)
    }

    pub fn create_type_u64(&mut self) -> TypeID {
        self.intern_type(Type::UnsignedInteger64)
    }

    pub fn create_type_f32(&mut self) -> TypeID {
        self.intern_type(Type::Float32)
    }

    pub fn create_type_f64(&mut self) -> TypeID {
        self.intern_type(Type::Float64)
    }

    pub fn create_type_prod(&mut self, tys: Box<[TypeID]>) -> TypeID {
        self.intern_type(Type::Product(tys))
    }

    pub fn create_type_prod2(&mut self, a: TypeID, b: TypeID) -> TypeID {
        self.create_type_prod(Box::new([a, b]))
    }

    pub fn create_type_prod3(&mut self, a: TypeID, b: TypeID, c: TypeID) -> TypeID {
        self.create_type_prod(Box::new([a, b, c]))
    }

    pub fn create_type_prod4(&mut self, a: TypeID, b: TypeID, c: TypeID, d: TypeID) -> TypeID {
        self.create_type_prod(Box::new([a, b, c, d]))
    }

    pub fn create_type_prod5(
        &mut self,
        a: TypeID,
        b: TypeID,
        c: TypeID,
        d: TypeID,
        e: TypeID,
    ) -> TypeID {
        self.create_type_prod(Box::new([a, b, c, d, e]))
    }

    pub fn create_type_prod6(
        &mut self,
        a: TypeID,
        b: TypeID,
        c: TypeID,
        d: TypeID,
        e: TypeID,
        f: TypeID,
    ) -> TypeID {
        self.create_type_prod(Box::new([a, b, c, d, e, f]))
    }

    pub fn create_type_prod7(
        &mut self,
        a: TypeID,
        b: TypeID,
        c: TypeID,
        d: TypeID,
        e: TypeID,
        f: TypeID,
        g: TypeID,
    ) -> TypeID {
        self.create_type_prod(Box::new([a, b, c, d, e, f, g]))
    }

    pub fn create_type_prod8(
        &mut self,
        a: TypeID,
        b: TypeID,
        c: TypeID,
        d: TypeID,
        e: TypeID,
        f: TypeID,
        g: TypeID,
        h: TypeID,
    ) -> TypeID {
        self.create_type_prod(Box::new([a, b, c, d, e, f, g, h]))
    }

    pub fn create_type_sum(&mut self, tys: Box<[TypeID]>) -> TypeID {
        self.intern_type(Type::Summation(tys))
    }

    pub fn create_type_sum2(&mut self, a: TypeID, b: TypeID) -> TypeID {
        self.create_type_sum(Box::new([a, b]))
    }

    pub fn create_type_sum3(&mut self, a: TypeID, b: TypeID, c: TypeID) -> TypeID {
        self.create_type_sum(Box::new([a, b, c]))
    }

    pub fn create_type_sum4(&mut self, a: TypeID, b: TypeID, c: TypeID, d: TypeID) -> TypeID {
        self.create_type_sum(Box::new([a, b, c, d]))
    }

    pub fn create_type_sum5(
        &mut self,
        a: TypeID,
        b: TypeID,
        c: TypeID,
        d: TypeID,
        e: TypeID,
    ) -> TypeID {
        self.create_type_sum(Box::new([a, b, c, d, e]))
    }

    pub fn create_type_sum6(
        &mut self,
        a: TypeID,
        b: TypeID,
        c: TypeID,
        d: TypeID,
        e: TypeID,
        f: TypeID,
    ) -> TypeID {
        self.create_type_sum(Box::new([a, b, c, d, e, f]))
    }

    pub fn create_type_sum7(
        &mut self,
        a: TypeID,
        b: TypeID,
        c: TypeID,
        d: TypeID,
        e: TypeID,
        f: TypeID,
        g: TypeID,
    ) -> TypeID {
        self.create_type_sum(Box::new([a, b, c, d, e, f, g]))
    }

    pub fn create_type_sum8(
        &mut self,
        a: TypeID,
        b: TypeID,
        c: TypeID,
        d: TypeID,
        e: TypeID,
        f: TypeID,
        g: TypeID,
        h: TypeID,
    ) -> TypeID {
        self.create_type_sum(Box::new([a, b, c, d, e, f, g, h]))
    }

    pub fn create_type_array(&mut self, elem: TypeID, extents: Box<[DynamicConstantID]>) -> TypeID {
        self.intern_type(Type::Array(elem, extents))
    }

    pub fn create_constant_bool(&mut self, val: bool) -> ConstantID {
        let ty = self.intern_type(Type::Boolean);
        self.intern_constant(Constant::Boolean(val), ty)
    }

    pub fn create_constant_i8(&mut self, val: i8) -> ConstantID {
        let ty = self.intern_type(Type::Integer8);
        self.intern_constant(Constant::Integer8(val), ty)
    }

    pub fn create_constant_i16(&mut self, val: i16) -> ConstantID {
        let ty = self.intern_type(Type::Integer16);
        self.intern_constant(Constant::Integer16(val), ty)
    }

    pub fn create_constant_i32(&mut self, val: i32) -> ConstantID {
        let ty = self.intern_type(Type::Integer32);
        self.intern_constant(Constant::Integer32(val), ty)
    }

    pub fn create_constant_i64(&mut self, val: i64) -> ConstantID {
        let ty = self.intern_type(Type::Integer64);
        self.intern_constant(Constant::Integer64(val), ty)
    }

    pub fn create_constant_u8(&mut self, val: u8) -> ConstantID {
        let ty = self.intern_type(Type::UnsignedInteger8);
        self.intern_constant(Constant::UnsignedInteger8(val), ty)
    }

    pub fn create_constant_u16(&mut self, val: u16) -> ConstantID {
        let ty = self.intern_type(Type::UnsignedInteger16);
        self.intern_constant(Constant::UnsignedInteger16(val), ty)
    }

    pub fn create_constant_u32(&mut self, val: u32) -> ConstantID {
        let ty = self.intern_type(Type::UnsignedInteger32);
        self.intern_constant(Constant::UnsignedInteger32(val), ty)
    }

    pub fn create_constant_u64(&mut self, val: u64) -> ConstantID {
        let ty = self.intern_type(Type::UnsignedInteger64);
        self.intern_constant(Constant::UnsignedInteger64(val), ty)
    }

    pub fn create_constant_f32(&mut self, val: f32) -> ConstantID {
        let ty = self.intern_type(Type::Float32);
        self.intern_constant(
            Constant::Float32(ordered_float::OrderedFloat::<f32>(val)),
            ty,
        )
    }

    pub fn create_constant_f64(&mut self, val: f64) -> ConstantID {
        let ty = self.intern_type(Type::Float64);
        self.intern_constant(
            Constant::Float64(ordered_float::OrderedFloat::<f64>(val)),
            ty,
        )
    }

    pub fn create_constant_prod(&mut self, cons: Box<[ConstantID]>) -> ConstantID {
        let ty = self.create_type_prod(cons.iter().map(|x| self.constant_types[x.idx()]).collect());
        self.intern_constant(Constant::Product(ty, cons), ty)
    }

    pub fn create_constant_sum(
        &mut self,
        ty: TypeID,
        variant: u32,
        cons: ConstantID,
    ) -> BuilderResult<ConstantID> {
        if let Type::Summation(variant_tys) = &self.module.types[ty.idx()] {
            if variant as usize >= variant_tys.len() {
                Err("Variant provided to create_constant_sum is too large for provided summation type.")?
            }
            if variant_tys[variant as usize] != self.constant_types[cons.idx()] {
                Err("Constant provided to create_constant_sum doesn't match the summation type provided.")?
            }
            Ok(self.intern_constant(Constant::Summation(ty, variant, cons), ty))
        } else {
            Err("Type provided to create_constant_sum is not a summation type.".to_owned())
        }
    }

    pub fn create_constant_array(
        &mut self,
        elem_ty: TypeID,
        cons: Box<[ConstantID]>,
        extents: Box<[u32]>,
    ) -> BuilderResult<ConstantID> {
        for con in cons.iter() {
            if self.constant_types[con.idx()] != elem_ty {
                Err("Constant provided to create_constant_array has a different type than the provided element type.")?
            }
        }
        let extents = extents
            .iter()
            .map(|extent| self.create_dynamic_constant_constant(*extent as usize))
            .collect();
        let ty = self.create_type_array(elem_ty, extents);
        Ok(self.intern_constant(Constant::Array(ty, cons), ty))
    }

    pub fn create_dynamic_constant_constant(&mut self, val: usize) -> DynamicConstantID {
        self.intern_dynamic_constant(DynamicConstant::Constant(val))
    }

    pub fn create_dynamic_constant_parameter(&mut self, val: usize) -> DynamicConstantID {
        self.intern_dynamic_constant(DynamicConstant::Parameter(val))
    }

    pub fn create_field_index(&self, idx: usize) -> Index {
        Index::Field(idx)
    }

    pub fn create_variant_index(&self, idx: usize) -> Index {
        Index::Variant(idx)
    }

    pub fn create_position_index(&self, idx: Box<[NodeID]>) -> Index {
        Index::Position(idx)
    }

    pub fn create_control_index(&self, idx: usize) -> Index {
        Index::Control(idx)
    }

    pub fn create_function(
        &mut self,
        name: &'a str,
        param_types: Vec<TypeID>,
        return_type: TypeID,
        num_dynamic_constants: u32,
    ) -> BuilderResult<(FunctionID, NodeID)> {
        if let Some(_) = self.function_ids.get(name) {
            Err(format!("Can't create a function with name \"{}\", because a function with the same name has already been created.", name))?
        }

        let id = FunctionID::new(self.module.functions.len());
        self.module.functions.push(Function {
            name: name.to_owned(),
            param_types,
            return_type,
            nodes: vec![Node::Start],
            num_dynamic_constants,
        });
        Ok((id, NodeID::new(0)))
    }

    pub fn allocate_node(&mut self, function: FunctionID) -> NodeBuilder {
        let id = NodeID::new(self.module.functions[function.idx()].nodes.len());
        self.module.functions[function.idx()]
            .nodes
            .push(Node::Start);
        NodeBuilder {
            id,
            function_id: function,
            node: Node::Start,
        }
    }

    pub fn add_node(&mut self, builder: NodeBuilder) -> BuilderResult<()> {
        if let Node::Start = builder.node {
            Err("Can't add node from a NodeBuilder before NodeBuilder has built a node.")?
        }
        self.module.functions[builder.function_id.idx()].nodes[builder.id.idx()] = builder.node;
        Ok(())
    }
}

impl NodeBuilder {
    pub fn id(&self) -> NodeID {
        self.id
    }

    pub fn build_region(&mut self, preds: Box<[NodeID]>) {
        self.node = Node::Region { preds };
    }

    pub fn build_if(&mut self, control: NodeID, cond: NodeID) {
        self.node = Node::If { control, cond };
    }

    pub fn build_fork(&mut self, control: NodeID, factor: DynamicConstantID) {
        self.node = Node::Fork { control, factor };
    }

    pub fn build_join(&mut self, control: NodeID) {
        self.node = Node::Join { control };
    }

    pub fn build_phi(&mut self, control: NodeID, data: Box<[NodeID]>) {
        self.node = Node::Phi { control, data };
    }

    pub fn build_threadid(&mut self, control: NodeID) {
        self.node = Node::ThreadID { control };
    }

    pub fn build_collect(&mut self, control: NodeID, init: NodeID, reduct: NodeID) {
        self.node = Node::Reduce {
            control,
            init,
            reduct,
        };
    }

    pub fn build_return(&mut self, control: NodeID, data: NodeID) {
        self.node = Node::Return { control, data };
    }

    pub fn build_parameter(&mut self, index: usize) {
        self.node = Node::Parameter { index };
    }

    pub fn build_constant(&mut self, id: ConstantID) {
        self.node = Node::Constant { id };
    }

    pub fn build_dynamicconstant(&mut self, id: DynamicConstantID) {
        self.node = Node::DynamicConstant { id };
    }

    pub fn build_unary(&mut self, input: NodeID, op: UnaryOperator) {
        self.node = Node::Unary { input, op };
    }

    pub fn build_binary(&mut self, left: NodeID, right: NodeID, op: BinaryOperator) {
        self.node = Node::Binary { left, right, op };
    }

    pub fn build_ternary(
        &mut self,
        first: NodeID,
        second: NodeID,
        third: NodeID,
        op: TernaryOperator,
    ) {
        self.node = Node::Ternary {
            first,
            second,
            third,
            op,
        };
    }

    pub fn build_call(
        &mut self,
        function: FunctionID,
        dynamic_constants: Box<[DynamicConstantID]>,
        args: Box<[NodeID]>,
    ) {
        self.node = Node::Call {
            function,
            dynamic_constants,
            args,
        };
    }

    pub fn build_read(&mut self, collect: NodeID, indices: Box<[Index]>) {
        self.node = Node::Read { collect, indices };
    }

    pub fn build_write(&mut self, collect: NodeID, data: NodeID, indices: Box<[Index]>) {
        self.node = Node::Write {
            collect,
            data,
            indices,
        };
    }
}

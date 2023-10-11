use std::collections::HashMap;

use crate::*;

/*
 * The builder provides a clean API for programatically creating IR modules.
 * The main function of the builder is to intern various parts of the IR.
 */
#[derive(Debug, Default)]
pub struct Builder<'a> {
    function_ids: HashMap<&'a str, FunctionID>,
    interned_nodes: Vec<HashMap<Node, NodeID>>,
    interned_types: HashMap<Type, TypeID>,
    interned_constants: HashMap<Constant, ConstantID>,
    interned_dynamic_constants: HashMap<DynamicConstant, DynamicConstantID>,
    module: Module,
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

    fn intern_constant(&mut self, cons: Constant) -> ConstantID {
        if let Some(id) = self.interned_constants.get(&cons) {
            *id
        } else {
            let id = ConstantID::new(self.interned_constants.len());
            self.interned_constants.insert(cons.clone(), id);
            self.module.constants.push(cons);
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

    pub fn create_type_array(&mut self, elem: TypeID, dc: DynamicConstantID) -> TypeID {
        self.intern_type(Type::Array(elem, dc))
    }

    pub fn create_constant_bool(&mut self, val: bool) -> ConstantID {
        self.intern_constant(Constant::Boolean(val))
    }

    pub fn create_constant_i8(&mut self, val: i8) -> ConstantID {
        self.intern_constant(Constant::Integer8(val))
    }

    pub fn create_constant_i16(&mut self, val: i16) -> ConstantID {
        self.intern_constant(Constant::Integer16(val))
    }

    pub fn create_constant_i32(&mut self, val: i32) -> ConstantID {
        self.intern_constant(Constant::Integer32(val))
    }

    pub fn create_constant_i64(&mut self, val: i64) -> ConstantID {
        self.intern_constant(Constant::Integer64(val))
    }

    pub fn create_constant_u8(&mut self, val: u8) -> ConstantID {
        self.intern_constant(Constant::UnsignedInteger8(val))
    }

    pub fn create_constant_u16(&mut self, val: u16) -> ConstantID {
        self.intern_constant(Constant::UnsignedInteger16(val))
    }

    pub fn create_constant_u32(&mut self, val: u32) -> ConstantID {
        self.intern_constant(Constant::UnsignedInteger32(val))
    }

    pub fn create_constant_u64(&mut self, val: u64) -> ConstantID {
        self.intern_constant(Constant::UnsignedInteger64(val))
    }

    pub fn create_constant_f32(&mut self, val: f32) -> ConstantID {
        self.intern_constant(Constant::Float32(ordered_float::OrderedFloat::<f32>(val)))
    }

    pub fn create_constant_f64(&mut self, val: f64) -> ConstantID {
        self.intern_constant(Constant::Float64(ordered_float::OrderedFloat::<f64>(val)))
    }
}

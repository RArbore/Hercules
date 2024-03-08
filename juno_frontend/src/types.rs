use std::collections::{HashMap, HashSet, VecDeque};

use crate::hercules_ir::ir::*;
use crate::hercules_ir::build::*;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Either<A, B> {
    Left(A),
    Right(B)
}

#[derive(Copy, Clone, Eq, Debug)]
pub enum DynamicConstant {
    Constant(usize), // constant value
    DynConst(usize, usize) // name and dynamic constant number
}

impl PartialEq for DynamicConstant {
    fn eq(&self, other : &Self) -> bool {
        match (self, other) {
            (DynamicConstant::Constant(x), DynamicConstant::Constant(y)) => x == y,
            (DynamicConstant::DynConst(_, n), DynamicConstant::DynConst(_, m)) => n == m,
            (_, _) => false,
        }
    }
}

impl DynamicConstant {
    fn subst(&self, dyn_consts : &Vec<DynamicConstant>) -> DynamicConstant {
        match self {
            DynamicConstant::Constant(val) => DynamicConstant::Constant(*val),
            DynamicConstant::DynConst(_, idx) => dyn_consts[*idx],
        }
    }
    
    fn to_string(&self, stringtab : &dyn Fn(usize) -> String) -> String {
        match self {
            DynamicConstant::Constant(val) => val.to_string(),
            DynamicConstant::DynConst(nm, _) => stringtab(*nm),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Primitive { Bool, U8, I8, U16, I16, U32, I32, U64, I64, F32, F64, Unit }

impl Primitive {
    fn is_number(&self) -> bool {
        match &self {
            Primitive::U8 | Primitive::I8 | Primitive::U16
                | Primitive::I16 | Primitive::U32 | Primitive::I32
                | Primitive::U64 | Primitive::I64 | Primitive::F32
                | Primitive::F64 => true,
            _ => false,
        }
    }
    
    fn is_integer(&self) -> bool {
        match &self {
            Primitive::U8 | Primitive::I8 | Primitive::U16
                | Primitive::I16 | Primitive::U32 | Primitive::I32
                | Primitive::U64 | Primitive::I64 => true,
            _ => false,
        }
    }
    
    fn is_float(&self) -> bool {
        match &self {
            Primitive::F32 | Primitive::F64 => true,
            _ => false,
        }
    }

    fn is_bool(&self) -> bool {
        match &self {
            Primitive::Bool => true,
            _ => false,
        }
    }

    fn to_string(&self) -> String {
        match self {
            Primitive::Bool  => "bool".to_string(),
            Primitive::I8    => "i8".to_string(),
            Primitive::U8    => "u8".to_string(),
            Primitive::I16   => "i16".to_string(),
            Primitive::U16   => "u16".to_string(),
            Primitive::I32   => "i32".to_string(),
            Primitive::U32   => "u32".to_string(),
            Primitive::I64   => "i64".to_string(),
            Primitive::U64   => "u64".to_string(),
            Primitive::F32   => "f32".to_string(),
            Primitive::F64   => "f64".to_string(),
            Primitive::Unit  => "()".to_string(),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Type { val : usize }

// Type forms, which include both concrete types, as well as unsolved types that may have some
// constraints. Note that constrained types are just primitives (particularly the numeric types)
// because the type system ensures that we always know information about arrays, structs, unions,
// and tuples
#[derive(Clone, Debug)]
enum TypeForm {
    Primitive(Primitive),
    Tuple(Vec<Type>),
    Array(Type, Vec<DynamicConstant>),

    // This type is the same type as another type
    OtherType(Type),

    // For type variables, we record its name, its index (in the list of type variables in this
    // context), and anything we know about it (is it a number, is it an integer)
    TypeVar { name : usize, index : usize, is_num : bool, is_int : bool },

    // For structs and unions we record the name (via its interned representation), a UID, and the
    // types of its fields/constructors in a set order and a map from field/constructor names to
    // its index in the list
    Struct { name : usize, id : usize, fields : Vec<Type>, names : HashMap<usize, usize> },
    Union  { name : usize, id : usize, constr : Vec<Type>, names : HashMap<usize, usize> },

    // Constrained types
    AnyNumber, AnyInteger, AnyFloat,
}

#[derive(Debug)]
pub struct TypeSolver {
    types : Vec<TypeForm>,
}

#[derive(Debug)]
pub struct TypeSolverInst<'a> {
    solver    : &'a TypeSolver,
    // A collection of current values for type variables, and variables that we've solved for in
    // that context
    type_vars : Vec<TypeID>,
    solved    : Vec<Option<TypeID>>,
}

impl TypeSolver {
    pub fn new() -> TypeSolver {
        TypeSolver { types : vec![] }
    }

    pub fn new_number(&mut self) -> Type {
        self.create_type(TypeForm::AnyNumber)
    }

    pub fn new_integer(&mut self) -> Type {
        self.create_type(TypeForm::AnyInteger)
    }

    pub fn new_float(&mut self) -> Type {
        self.create_type(TypeForm::AnyFloat)
    }

    pub fn new_primitive(&mut self, p : Primitive) -> Type {
        self.create_type(TypeForm::Primitive(p))
    }

    pub fn new_tuple(&mut self, fields : Vec<Type>) -> Type {
        self.create_type(TypeForm::Tuple(fields))
    }

    pub fn new_array(&mut self, element : Type, dims : Vec<DynamicConstant>) -> Type {
        self.create_type(TypeForm::Array(element, dims))
    }

    pub fn new_type_var(&mut self, name : usize, index : usize, is_num : bool,
                    is_int : bool) -> Type {
        self.create_type(TypeForm::TypeVar { name, index, is_num, is_int })
    }

    pub fn new_struct(&mut self, name : usize, id : usize, fields : Vec<Type>,
                  names : HashMap<usize, usize>) -> Type {
        self.create_type(TypeForm::Struct { name, id, fields, names })
    }

    pub fn new_union(&mut self, name : usize, id : usize, constr : Vec<Type>,
                  names : HashMap<usize, usize>) -> Type {
        self.create_type(TypeForm::Union { name, id, constr, names })
    }

    fn create_type(&mut self, typ : TypeForm) -> Type {
        let idx = self.types.len();
        self.types.push(typ);
        Type { val : idx }
    }

    pub fn create_instance(&self, type_vars : Vec<TypeID>) -> TypeSolverInst {
        let num_vars = self.types.len();
        TypeSolverInst { solver    : self,
                         type_vars : type_vars,
                         solved    : vec![None; num_vars] }
    }

    pub fn is_u64(&mut self, Type { val } : Type) -> bool {
        match &self.types[val] {
            TypeForm::Primitive(Primitive::U64) => true,
            TypeForm::OtherType(t) => self.is_u64(*t),
            TypeForm::AnyNumber | TypeForm::AnyInteger => {
                self.types[val] = TypeForm::Primitive(Primitive::U64);
                true
            },
            _ => false,
        }
    }

    pub fn is_bool(&mut self, Type { val } : Type) -> bool {
        match &self.types[val] {
            TypeForm::Primitive(Primitive::Bool) => true,
            TypeForm::OtherType(t) => self.is_bool(*t),
            _ => false,
        }
    }

    pub fn is_void(&mut self, Type { val } : Type) -> bool {
        match &self.types[val] {
            TypeForm::Primitive(Primitive::Unit) => true,
            TypeForm::OtherType(t) => self.is_void(*t),
            _ => false,
        }
    }

    pub fn is_number(&mut self, Type { val } : Type) -> bool {
        match &self.types[val] {
            TypeForm::Primitive(p) => p.is_number(),
            TypeForm::OtherType(t) => self.is_number(*t),
            TypeForm::AnyNumber | TypeForm::AnyInteger | TypeForm::AnyFloat => true,
            TypeForm::TypeVar { name : _, index : _, is_num, .. } => *is_num,
            _ => false,
        }
    }

    pub fn is_integer(&mut self, Type { val } : Type) -> bool {
        match &self.types[val] {
            TypeForm::Primitive(p) => p.is_integer(),
            TypeForm::OtherType(t) => self.is_integer(*t),
            TypeForm::TypeVar { name : _, index : _, is_num : _, is_int } => *is_int,
            TypeForm::AnyInteger => true,
            TypeForm::AnyNumber => {
                self.types[val] = TypeForm::AnyInteger;
                true
            },
            _ => false,
        }
    }
   
    pub fn is_float(&mut self, Type { val } : Type) -> bool {
        match &self.types[val] {
            TypeForm::Primitive(p) => p.is_float(),
            TypeForm::OtherType(t) => self.is_float(*t),
            TypeForm::AnyFloat => true,
            TypeForm::AnyNumber => {
                self.types[val] = TypeForm::AnyFloat;
                true
            },
            _ => false,
        }
    }

    pub fn is_tuple(&self, Type { val } : Type) -> bool {
        match &self.types[val] {
            TypeForm::Tuple(_) => true,
            TypeForm::OtherType(t) => self.is_tuple(*t),
            _ => false,
        }
    }

    pub fn get_num_fields(&self, Type { val } : Type) -> Option<usize> {
        match &self.types[val] {
            TypeForm::Tuple(fields) => { Some(fields.len()) },
            TypeForm::OtherType(t) => self.get_num_fields(*t),
            _ => None,
        }
    }

    fn get_fields(&self, Type { val } : Type) -> Vec<Type> {
        match &self.types[val] {
            TypeForm::Tuple(fields) => { fields.clone() },
            TypeForm::OtherType(t) => self.get_fields(*t),
            _ => panic!("Internal function get_fields used on non-tuple"),
        }
    }

    pub fn get_index(&self, Type { val } : Type, idx : usize) -> Option<Type> {
        match &self.types[val] {
            TypeForm::Tuple(fields) => fields.get(idx).copied(),
            TypeForm::OtherType(t) => self.get_index(*t, idx),
            _ => None,
        }
    }

    pub fn is_struct(&self, Type { val } : Type) -> bool {
        match &self.types[val] {
            TypeForm::Struct { .. } => true,
            TypeForm::OtherType(t) => self.is_struct(*t),
            _ => false,
        }
    }

    // Return the number of fields a struct has
    pub fn get_num_struct_fields(&self, Type { val } : Type) -> Option<usize> {
        match &self.types[val] {
            TypeForm::Struct { name : _, id : _, fields, .. } => Some(fields.len()),
            TypeForm::OtherType(t) => self.get_num_struct_fields(*t),
            _ => None,
        }
    }

    // Returns the position and type of a field in a type (if it exists)
    pub fn get_field(&self, Type { val } : Type, name : usize) -> Option<(usize, Type)> {
        match &self.types[val] {
            TypeForm::Struct { name : _, id : _, fields, names } => {
                names.get(&name).map(|idx| (*idx, fields[*idx]))
            },
            TypeForm::OtherType(t) => self.get_field(*t, name),
            _ => None,
        }
    }

    // Returns the type of the field at a certain index in a struct
    pub fn get_struct_field_type(&self, Type { val } : Type, idx : usize) -> Option<Type> {
        match &self.types[val] {
            TypeForm::Struct { name : _, id : _, fields, .. } =>
                fields.get(idx).copied(),
            TypeForm::OtherType(t) => self.get_struct_field_type(*t, idx),
            _ => None,
        }
    }

    pub fn get_field_names(&self, Type { val } : Type) -> Option<Vec<usize>> {
        match &self.types[val] {
            TypeForm::Struct { name : _, id : _, fields : _, names } => {
                Some(names.keys().map(|i| *i).collect::<Vec<_>>())
            },
            TypeForm::OtherType(t) => self.get_field_names(*t),
            _ => None,
        }
    }

    pub fn is_array(&self, Type { val } : Type) -> bool {
        match &self.types[val] {
            TypeForm::Array(_, _) => true,
            TypeForm::OtherType(t) => self.is_array(*t),
            _ => false,
        }
    }

    pub fn get_element_type(&self, Type { val } : Type) -> Option<Type> {
        match &self.types[val] {
            TypeForm::Array(elem, _) => Some(*elem),
            TypeForm::OtherType(t) => self.get_element_type(*t),
            _ => None,
        }
    }

    pub fn get_dimensions(&self, Type { val } : Type) -> Option<Vec<DynamicConstant>> {
        match &self.types[val] {
            TypeForm::Array(_, dims) => Some(dims.to_vec()),
            TypeForm::OtherType(t) => self.get_dimensions(*t),
            _ => None,
        }
    }

    pub fn get_num_dimensions(&self, Type { val } : Type) -> Option<usize> {
        match &self.types[val] {
            TypeForm::Array(_, dims) => Some(dims.len()),
            TypeForm::OtherType(t) => self.get_num_dimensions(*t),
            _ => None,
        }
    }

    pub fn is_union(&self, Type { val } : Type) -> bool {
        match &self.types[val] {
            TypeForm::Union { .. } => true,
            TypeForm::OtherType(t) => self.is_union(*t),
            _ => false,
        }
    }
    
    pub fn get_constructor_list(&self, Type { val } : Type) -> Option<Vec<usize>> {
        match &self.types[val] {
            TypeForm::Union { name : _, id : _, constr : _, names } => {
                Some(names.keys().map(|i| *i).collect::<Vec<_>>())
            },
            TypeForm::OtherType(t) => self.get_constructor_list(*t),
            _ => None,
        }
    }

    pub fn get_constructor_info(&self, Type { val } : Type, name : usize) 
        -> Option<(usize, Type)> {
        match &self.types[val] {
            TypeForm::Union { name : _, id : _, constr, names} => {
                names.get(&name).map(|idx| (*idx, constr[*idx]))
            },
            TypeForm::OtherType(t) => self.get_constructor_info(*t, name),
            _ => None,
        }
    }

    fn is_type_var_num(&self, num : usize, Type { val } : Type) -> bool {
        match &self.types[val] {
            TypeForm::TypeVar { name : _, index, .. } => *index == num,
            TypeForm::OtherType(t) => self.is_type_var_num(num, *t),
            _ => false,
        }
    }

    pub fn equal(&mut self, Type { val : ty1 } : Type, Type { val : ty2 } : Type) -> bool {
        if let TypeForm::OtherType(ty) = self.types[ty1] {
            return self.equal(ty, Type { val : ty2 });
        }
        if let TypeForm::OtherType(ty) = self.types[ty2] {
            return self.equal(Type { val : ty1 }, ty);
        }

        match (self.types[ty1].clone(), self.types[ty2].clone()) {
            (TypeForm::Primitive(p1), TypeForm::Primitive(p2)) => p1 == p2,

            (TypeForm::Primitive(p), TypeForm::AnyNumber)  if p.is_number() => {
                self.types[ty2] = TypeForm::OtherType(Type { val : ty1 });
                true
            },
            (TypeForm::Primitive(p), TypeForm::AnyInteger) if p.is_integer() => {
                self.types[ty2] = TypeForm::OtherType(Type { val : ty1 });
                true
            },
            (TypeForm::Primitive(p), TypeForm::AnyFloat)   if p.is_float() => {
                self.types[ty2] = TypeForm::OtherType(Type { val : ty1 });
                true
            },

            (TypeForm::AnyNumber,  TypeForm::Primitive(p)) if p.is_number() => {
                self.types[ty1] = TypeForm::OtherType(Type { val : ty2 });
                true
            },
            (TypeForm::AnyInteger, TypeForm::Primitive(p)) if p.is_number() => {
                self.types[ty1] = TypeForm::OtherType(Type { val : ty2 });
                true
            },
            (TypeForm::AnyFloat,   TypeForm::Primitive(p)) if p.is_float() => {
                self.types[ty1] = TypeForm::OtherType(Type { val : ty2 });
                true
            },

            (TypeForm::Tuple(f1), TypeForm::Tuple(f2)) if f1.len() == f2.len() => {
                for (t1, t2) in f1.iter().zip(f2.iter()) {
                    if !self.equal(*t1, *t2) { return false; }
                }
                true
            },

            (TypeForm::Array(t1, dm1), TypeForm::Array(t2, dm2)) =>
                self.equal(t1, t2) && dm1 == dm2,

            (TypeForm::TypeVar { name : _, index : idx1, .. },
             TypeForm::TypeVar { name : _, index : idx2, .. }) => idx1 == idx2,

            (TypeForm::Struct { name : _, id : id1, fields : fs1, .. },
             TypeForm::Struct { name : _, id : id2, fields : fs2, .. })
            | (TypeForm::Union {name : _, id : id1, constr : fs1, .. },
               TypeForm::Union {name : _, id : id2, constr : fs2, .. })
              if id1 == id2 && fs1.len() == fs2.len() => {
                for (t1, t2) in fs1.iter().zip(fs2.iter()) {
                    if !self.equal(*t1, *t2) { return false; }
                }
                true
            },

            (TypeForm::AnyNumber | TypeForm::AnyInteger | TypeForm::AnyFloat, 
             TypeForm::AnyNumber)
            | (TypeForm::AnyInteger, TypeForm::AnyInteger)
            | (TypeForm::AnyFloat,   TypeForm::AnyFloat) => {
                self.types[ty2] = TypeForm::OtherType(Type { val : ty1 });
                true
            },
            (TypeForm::AnyNumber, TypeForm::AnyInteger)
            | (TypeForm::AnyNumber, TypeForm::AnyFloat) => {
                self.types[ty1] = TypeForm::OtherType(Type { val : ty2 });
                true
            },

            _ => false,
        }
    }

    pub fn to_string(&self, Type { val } : Type, stringtab : &dyn Fn(usize) -> String) 
        -> String {
        match &self.types[val] {
            TypeForm::Primitive(p) => p.to_string(),
            TypeForm::Tuple(fields) => {
                "(" .to_string()
                + &fields.iter().map(|t| self.to_string(*t, stringtab)).collect::<Vec<_>>().join(", ")
                + ")"
            },
            TypeForm::Array(elem, dims) => {
                self.to_string(*elem, stringtab)
                + "["
                + &dims.iter().map(|d| d.to_string(stringtab)).collect::<Vec<_>>().join(", ")
                + "]"
            },
            TypeForm::OtherType(typ) => {
                self.to_string(*typ, stringtab)
            },
            TypeForm::TypeVar { name, .. } | TypeForm::Struct { name, .. }
                | TypeForm::Union { name, .. } => {
                stringtab(*name)
            },
            TypeForm::AnyNumber => "number".to_string(),
            TypeForm::AnyInteger => "integer".to_string(),
            TypeForm::AnyFloat => "float".to_string(),
        }
    }

    // Instantiate a type using the provided list of type variables and dynamic constants
    // This is useful for instantiating the return type of a function and parametric types
    pub fn instantiate(&mut self, Type { val } : Type, type_vars : &Vec<Type>,
                       dynamic_constants : &Vec<DynamicConstant>) -> Type {
        match self.types[val].clone() {
            TypeForm::Primitive(_) => Type { val },
            TypeForm::AnyNumber | TypeForm::AnyInteger | TypeForm::AnyFloat => {
                self.create_type(self.types[val].clone())
            },
            TypeForm::OtherType(t) =>
                self.instantiate(t, type_vars, dynamic_constants),
            TypeForm::Tuple(fields) => {
                let mut types = vec![];
                let mut changed = false;
                for typ in fields {
                    let inst = self.instantiate(typ, type_vars, dynamic_constants);
                    changed = changed || typ.val != inst.val;
                    types.push(inst);
                }
                if changed { self.create_type(TypeForm::Tuple(types)) }
                else { Type { val } }
            },
            TypeForm::Array(elem, dims) => {
                let elem_typ = self.instantiate(elem, type_vars, dynamic_constants);
                let subst_dims =
                    dims.iter().map(|c| c.subst(dynamic_constants)).collect::<Vec<_>>();

                self.create_type(TypeForm::Array(elem_typ, subst_dims))
            },
            TypeForm::TypeVar { name : _, index, is_num, is_int } => {
                let typ = type_vars[index];
                assert!(!is_num || self.is_number(typ));
                assert!(!is_int || self.is_integer(typ));
                typ
            },
            TypeForm::Struct { name, id, fields, names } => {
                let mut new_fields = vec![];
                let mut changed = false;
                for typ in fields {
                    let inst = self.instantiate(typ, type_vars, dynamic_constants);
                    changed = changed || typ.val != inst.val;
                    new_fields.push(inst);
                }

                if changed { self.create_type(TypeForm::Struct {
                                                name : name, id : id,
                                                fields : new_fields,
                                                names : names.clone() }) }
                else { Type { val } }
            },
            TypeForm::Union { name, id, constr, names } => {
                let mut new_constr = vec![];
                let mut changed = false;
                for typ in constr {
                    let inst = self.instantiate(typ, type_vars, dynamic_constants);
                    changed = changed || typ.val != inst.val;
                    new_constr.push(inst);
                }

                if changed { self.create_type(TypeForm::Union {
                                                name : name, id : id,
                                                constr : new_constr,
                                                names : names.clone() }) }
                else { Type { val } }
            },
        }
    }
}

impl TypeSolverInst<'_> {
    pub fn lower_type(&mut self, builder : &mut Builder, Type { val } : Type) -> TypeID {
        if self.solved[val].is_some() {
            return self.solved[val].unwrap();
        }

        let mut worklist = VecDeque::from([val]);
        let mut depends : HashMap<usize, HashSet<usize>> = HashMap::new();

        while !worklist.is_empty() {
            let typ = worklist.pop_front().unwrap();

            // If this type is already solved, just continue.
            // Since we don't depend on something unless its unsolved we only need to drain the set
            // of dependences once
            if self.solved[typ].is_some() { continue; }

            let solution : Either<TypeID, usize> =
                match &self.solver.types[typ] {
                    TypeForm::Primitive(p) => Either::Left(Self::build_primitive(builder, *p)),
                    TypeForm::Tuple(fields) => {
                        let mut needs = None;
                        let mut i_fields = vec![];

                        for Type { val } in fields {
                            match &self.solved[*val] {
                                Some(ty) => i_fields.push(*ty),
                                None => { needs = Some(*val); break; },
                            }
                        }

                        if let Some(t) = needs {
                            Either::Right(t)
                        } else {
                            Either::Left(Self::build_product(builder, i_fields))
                        }
                    },
                    TypeForm::Array(Type { val }, dims) => {
                        match &self.solved[*val] {
                            Some(ty) =>
                                Either::Left(Self::build_array(builder, *ty, dims)),
                            None => Either::Right(*val),
                        }
                    },
                    TypeForm::OtherType(Type { val }) => {
                        match &self.solved[*val] {
                            Some(ty) => Either::Left(*ty), 
                            None => Either::Right(*val),
                        }
                    },
                    TypeForm::TypeVar { name : _, index, .. } => {
                        Either::Left(self.type_vars[*index])
                    },
                    TypeForm::Struct { name : _, id : _, fields, .. } => {
                        let mut needs = None;
                        let mut i_fields = vec![];

                        for Type { val } in fields {
                            match &self.solved[*val] {
                                Some(ty) => i_fields.push(*ty),
                                None => { needs = Some(*val); break; },
                            }
                        }

                        if let Some(t) = needs {
                            Either::Right(t)
                        } else {
                            Either::Left(Self::build_product(builder, i_fields))
                        }
                    },
                    TypeForm::Union { name : _, id : _, constr, .. } => {
                        let mut needs = None;
                        let mut i_constr = vec![];

                        for Type { val } in constr {
                            match &self.solved[*val] {
                                Some(ty) => i_constr.push(*ty),
                                None => { needs = Some(*val); break; },
                            }
                        }

                        if let Some(t) = needs {
                            Either::Right(t)
                        } else {
                            Either::Left(Self::build_union(builder, i_constr))
                        }
                    },
                    TypeForm::AnyNumber | TypeForm::AnyInteger =>
                        Either::Left(Self::build_primitive(builder, Primitive::I64)),
                    TypeForm::AnyFloat =>
                        Either::Left(Self::build_primitive(builder, Primitive::F64)),
                };

            match solution {
                Either::Left(solution) => {
                    self.solved[typ] = Some(solution);
                    match depends.get_mut(&typ) {
                        None => {},
                        Some(set) => {
                            for idx in set.drain() {
                                worklist.push_back(idx);
                            }
                        },
                    }
                },
                Either::Right(needs) => {
                    match depends.get_mut(&needs) {
                        None => {
                            depends.insert(needs, HashSet::from([typ]));
                        },
                        Some(set) => {
                            set.insert(typ);
                        },
                    }
                    worklist.push_back(needs);
                },
            }
        }

        self.solved[val].expect("Failure to solve type constraints")
    }
    
    pub fn as_numeric_type(&mut self, builder : &mut Builder, ty : Type) -> Primitive {
        let type_id = self.lower_type(builder, ty);
        if type_id == builder.create_type_i8() { Primitive::I8 }
        else if type_id == builder.create_type_i16() { Primitive::I16 }
        else if type_id == builder.create_type_i32() { Primitive::I32 }
        else if type_id == builder.create_type_i64() { Primitive::I64 }
        else if type_id == builder.create_type_u8()  { Primitive::U8 }
        else if type_id == builder.create_type_u16() { Primitive::U16 }
        else if type_id == builder.create_type_u32() { Primitive::U32 }
        else if type_id == builder.create_type_u64() { Primitive::U64 }
        else if type_id == builder.create_type_f32() { Primitive::F32 }
        else if type_id == builder.create_type_f64() { Primitive::F64 }
        else { panic!("as_numeric_type() called on non-numeric type") }
    }
    
    fn build_primitive(builder : &mut Builder, p : Primitive) -> TypeID {
        match p {
            Primitive::Bool => builder.create_type_bool(),
            Primitive::I8   => builder.create_type_i8(),
            Primitive::I16  => builder.create_type_i16(),
            Primitive::I32  => builder.create_type_i32(),
            Primitive::I64  => builder.create_type_i64(),
            Primitive::U8   => builder.create_type_u8(),
            Primitive::U16  => builder.create_type_u16(),
            Primitive::U32  => builder.create_type_u32(),
            Primitive::U64  => builder.create_type_u64(),
            Primitive::F32  => builder.create_type_f32(),
            Primitive::F64  => builder.create_type_f64(),
            Primitive::Unit => builder.create_type_prod(vec![].into()),
        }
    }

    fn build_product(builder : &mut Builder, tys : Vec<TypeID>) -> TypeID {
        builder.create_type_prod(tys.into())
    }

    fn build_union(builder : &mut Builder, tys : Vec<TypeID>) -> TypeID {
        builder.create_type_sum(tys.into())
    }

    fn build_array(builder : &mut Builder, elem : TypeID, dims : &Vec<DynamicConstant>) -> TypeID {
        let extents = Self::build_dyn_consts(builder, dims);
        builder.create_type_array(elem, extents.into())
    }

    pub fn build_dyn_consts(builder : &mut Builder, vals : &Vec<DynamicConstant>) -> Vec<DynamicConstantID> {
        let mut res = vec![];
        for val in vals {
            res.push(Self::build_dyn_const(builder, val));
        }
        res
    }

    pub fn build_dyn_const(builder : &mut Builder, val : &DynamicConstant) -> DynamicConstantID {
        match val {
            DynamicConstant::Constant(val) => builder.create_dynamic_constant_constant(*val),
            DynamicConstant::DynConst(_, num) => builder.create_dynamic_constant_parameter(*num),
        }
    }
}

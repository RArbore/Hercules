use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Either<A, B> {
    Left(A),
    Right(B)
}

#[derive(Copy, Clone, Eq)]
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

#[derive(Copy, Clone, PartialEq, Eq)]
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

    fn is_unsigned(&self) -> bool {
        match &self {
            Primitive::U8 | Primitive::U16 | Primitive::U32 | Primitive::U64 => true,
            _ => false,
        }
    }
    
    fn is_float(&self) -> bool {
        match &self {
            Primitive::F32 | Primitive::F64 => true,
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

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Type { val : usize }

// Type forms, which include both concrete types, as well as unsolved types that may have some
// constraints. Note that constrained types are just primitives (particularly the numeric types)
// because the type system ensures that we always know information about arrays, structs, unions,
// and tuples
#[derive(Clone)]
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
    // Types that can hold a value of the given primitive type (i.e. the primitive can be
    // implicitly coerced into the ultimate type)
    AtLeast(Primitive),
    // Types that can be coerced into the given primitive type
    AtMost(Primitive),
}

#[derive(Clone, PartialEq, Eq)]
pub enum IType {
    Primitive(Primitive),
    Array(Box<IType>, Vec<DynamicConstant>),
    Prod(Vec<IType>),
    Sum(Vec<IType>),
}

pub struct TypeSolver {
    types : Vec<TypeForm>,

    // A collection of current values for type variables, and variables that we've solved for in
    // that context
    type_vars : Vec<IType>,
    solved : Vec<Option<IType>>,
}

impl TypeSolver {
    pub fn new() -> TypeSolver {
        TypeSolver { types : vec![], type_vars : vec![], solved : vec![] }
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
        assert!(fields.len() > 1);
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

    pub fn set_type_vars(&mut self, type_vars : Vec<IType>) {
        self.type_vars = type_vars;
        self.solved.clear();
        self.solved.resize(self.types.len(), None);
    }

    pub fn lower_type(&mut self, Type { val } : Type) -> IType {
        if self.solved[val].is_some() {
            return self.solved[val].as_ref().unwrap().clone();
        }

        let mut worklist = VecDeque::from([val]);
        let mut depends : HashMap<usize, HashSet<usize>> = HashMap::new();

        while !worklist.is_empty() {
            let typ = worklist.pop_front().unwrap();

            // If this type is already solved, just continue.
            // Since we don't depend on something unless its unsolved we only need to drain the set
            // of dependences once
            if self.solved[typ].is_some() { continue; }

            let solution : Either<IType, usize> =
                match &self.types[typ] {
                    TypeForm::Primitive(p) | TypeForm::AtLeast(p)
                        | TypeForm::AtMost(p) => Either::Left(IType::Primitive(*p)),
                    TypeForm::Tuple(fields) => {
                        let mut needs = None;
                        let mut i_fields = vec![];

                        for Type { val } in fields {
                            match &self.solved[*val] {
                                Some(ty) => i_fields.push(ty.clone()),
                                None => { needs = Some(*val); break; },
                            }
                        }

                        if let Some(t) = needs {
                            Either::Right(t)
                        } else {
                            Either::Left(IType::Prod(i_fields))
                        }
                    },
                    TypeForm::Array(Type { val }, dims) => {
                        match &self.solved[*val] {
                            Some(ty) =>
                                Either::Left(IType::Array(
                                        Box::new(ty.clone()), dims.clone())),
                            None => Either::Right(*val),
                        }
                    },
                    TypeForm::OtherType(Type { val }) => {
                        match &self.solved[*val] {
                            Some(ty) => Either::Left(ty.clone()), 
                            None => Either::Right(*val),
                        }
                    },
                    TypeForm::TypeVar { name : _, index, .. } => {
                        Either::Left(self.type_vars[*index].clone())
                    },
                    TypeForm::Struct { name : _, id : _, fields, .. } => {
                        let mut needs = None;
                        let mut i_fields = vec![];

                        for Type { val } in fields {
                            match &self.solved[*val] {
                                Some(ty) => i_fields.push(ty.clone()),
                                None => { needs = Some(*val); break; },
                            }
                        }

                        if let Some(t) = needs {
                            Either::Right(t)
                        } else {
                            Either::Left(IType::Prod(i_fields))
                        }
                    },
                    TypeForm::Union { name : _, id : _, constr, .. } => {
                        let mut needs = None;
                        let mut i_constr = vec![];

                        for Type { val } in constr {
                            match &self.solved[*val] {
                                Some(ty) => i_constr.push(ty.clone()),
                                None => { needs = Some(*val); break; },
                            }
                        }

                        if let Some(t) = needs {
                            Either::Right(t)
                        } else {
                            Either::Left(IType::Sum(i_constr))
                        }
                    },
                    TypeForm::AnyNumber | TypeForm::AnyInteger => {
                        Either::Left(IType::Primitive(Primitive::I64))
                    },
                    TypeForm::AnyFloat => {
                        Either::Left(IType::Primitive(Primitive::F64))
                    },
                };

            match solution {
                Either::Left(solution) => {
                    self.solved.insert(typ, Some(solution));
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
                },
            }
        }

        self.solved[val].as_ref().expect("Failure to solve type constraints").clone()
    }

    pub fn is_void(&mut self, Type { val } : Type) -> bool {
        match &self.types[val] {
            TypeForm::Primitive(Primitive::Unit) => true,
            _ => false,
        }
    }

    pub fn is_number(&mut self, Type { val } : Type) -> bool {
        match &self.types[val] {
            TypeForm::Primitive(p) | TypeForm::AtLeast(p) | TypeForm::AtMost(p)
                => p.is_number(),
            TypeForm::OtherType(t) => self.is_number(*t),
            TypeForm::AnyNumber | TypeForm::AnyInteger | TypeForm::AnyFloat => true,
            TypeForm::TypeVar { name : _, index : _, is_num, .. } => *is_num,
            _ => false,
        }
    }

    pub fn is_integer(&mut self, Type { val } : Type) -> bool {
        match &self.types[val] {
            TypeForm::Primitive(p) | TypeForm::AtLeast(p) | TypeForm::AtMost(p)
                => p.is_integer(),
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
            TypeForm::Primitive(p) | TypeForm::AtLeast(p) | TypeForm::AtMost(p)
                => p.is_float(),
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

    pub fn get_field(&self, Type { val } : Type, name : usize) -> Option<Type> {
        match &self.types[val] {
            TypeForm::Struct { name : _, id : _, fields, names } => {
                names.get(&name).map(|idx| fields[*idx])
            },
            TypeForm::OtherType(t) => self.get_field(*t, name),
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

    pub fn get_constructor_index(&self, Type { val } : Type, name : usize) -> Option<usize> {
        match &self.types[val] {
            TypeForm::Union { name : _, id : _, constr : _, names} => {
                names.get(&name).copied()
            },
            TypeForm::OtherType(t) => self.get_constructor_index(*t, name),
            _ => None,
        }
    }

    pub fn get_constructor_type(&self, Type { val } : Type, name : usize) -> Option<Type> {
        match &self.types[val] {
            TypeForm::Union { name : _, id : _, constr, names } => {
                names.get(&name).map(|idx| constr[*idx])
            },
            TypeForm::OtherType(t) => self.get_constructor_type(*t, name),
            _ => None,
        }
    }

    fn can_coerce_primitive(&mut self, goal : Primitive, Type { val : have } : Type) -> bool {
        match &self.types[have] {
            TypeForm::Primitive(have) => { todo!() },
            TypeForm::OtherType(t) => self.can_coerce_primitive(goal, *t),
            TypeForm::AnyNumber => { todo!() },
            TypeForm::AnyInteger => { todo!() },
            TypeForm::AnyFloat => { todo!() },
            TypeForm::AtLeast(have) => { todo!() },
            TypeForm::AtMost(have) => { todo!() },
            _ => false,
        }
    }

    fn is_type_var_num(&self, num : usize, Type { val } : Type) -> bool {
        match &self.types[val] {
            TypeForm::TypeVar { name : _, index, .. } => *index == num,
            TypeForm::OtherType(t) => self.is_type_var_num(num, *t),
            _ => false,
        }
    }

    pub fn can_coerce(&mut self, Type { val : goal } : Type, have : Type) -> bool {
        match self.types[goal].clone() {
            TypeForm::Primitive(p) => {
                self.can_coerce_primitive(p, have)
            },
            TypeForm::Tuple(fs) => {
                if !self.is_tuple(have) { return false; }
                let have_fields = self.get_fields(have);
                if fs.len() != have_fields.len() { return false; }
                for (g_ty, h_ty) in fs.iter().zip(have_fields.iter()) {
                    if !self.can_coerce(*g_ty, *h_ty) { return false; }
                }
                true
            },
            TypeForm::Array(elem, dims) => {
                if !self.is_array(have) { return false; }
                let have_elem = self.get_element_type(have).unwrap();
                let have_dims = self.get_dimensions(have).unwrap();

                self.can_coerce(elem, have_elem) && dims == have_dims
            },
            TypeForm::OtherType(other) => self.can_coerce(other, have),
            TypeForm::TypeVar { name : _, index, .. } => {
                self.is_type_var_num(index, have)
            },
            TypeForm::Struct { name : _, id, fields, .. } => {
                todo!()
            },
            TypeForm::Union { name : _, id, constr, .. } => {
                todo!()
            },

            TypeForm::AnyNumber => { todo!() },
            TypeForm::AnyInteger => { todo!() },
            TypeForm::AnyFloat => { todo!() },
            TypeForm::AtLeast(p) => { todo!() },
            TypeForm::AtMost(p) => { todo!() },
        }
    }

    pub fn to_string(&self, Type { val } : Type, stringtab : &dyn Fn(usize) -> String) 
        -> String {
        match &self.types[val] {
            TypeForm::Primitive(p) | TypeForm::AtLeast(p) | TypeForm::AtMost(p)
                => p.to_string(),
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
            TypeForm::AnyNumber | TypeForm::AnyInteger | TypeForm::AnyFloat
                | TypeForm::AtLeast(_) | TypeForm::AtMost(_) => {
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
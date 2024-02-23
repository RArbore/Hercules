extern crate hercules_ir;

use std::collections::{HashMap, LinkedList};
use std::fs::File;
use std::io::Read;
use std::iter;

use lrlex::{lrlex_mod, DefaultLexerTypes};
use lrpar::{lrpar_mod, NonStreamingLexer};
use cfgrammar::Span;

use self::hercules_ir::ir::*;
use self::hercules_ir::build::*;

lrlex_mod!("lang.l");
lrpar_mod!("lang.y");

use lang_y::*;
use crate::env::Env;
use crate::ssa::SSA;

#[derive(Clone, PartialEq, Eq)]
enum DynamicConstant {
    Constant(usize), // constant value
    DynConst(usize, usize), // name and dynamic constant number
}

impl DynamicConstant {
    fn to_string(&self, stringtab : &StringTable) -> String {
        match self {
            DynamicConstant::Constant(val) => val.to_string(),
            DynamicConstant::DynConst(nm, _) => stringtab.lookup_id(*nm).unwrap(),
        }
    }
}

#[derive(Clone, Eq)]
enum Type {
    Primitive(Primitive),
    Tuple(Vec<Type>),
    Array(Box<Type>, Vec<DynamicConstant>),
    // The first usize is the struct's name, the second is a "struct identifier" that indicates
    // what struct this is so we don't have to compare fields, the vector of types is the types of
    // the fields in a determined order, and the map is from field names to field positions in the
    // order
    Struct { name : usize, id : usize, fields : Vec<Type>, names : HashMap<usize, usize> },
}

impl PartialEq for Type {
    fn eq(&self, other : &Type) -> bool {
        match (self, other) {
            (Type::Primitive(p), Type::Primitive(q)) => p == q,
            (Type::Tuple(ts), Type::Tuple(ss)) => ts == ss,
            (Type::Array(t, ds), Type::Array(s, ms)) => t == s && ds == ms,
            (Type::Struct { name : _, id : mid, .. },
             Type::Struct { name : _, id : oid, .. })
                => mid == oid,
            (_, _) => false,
        }
    }
}

impl Type {
    fn to_string(&self, stringtab : &StringTable) -> String {
        match self {
            Type::Primitive(Primitive::Bool)  => "bool".to_string(),
            Type::Primitive(Primitive::I8)    => "i8".to_string(),
            Type::Primitive(Primitive::U8)    => "u8".to_string(),
            Type::Primitive(Primitive::I16)   => "i16".to_string(),
            Type::Primitive(Primitive::U16)   => "u16".to_string(),
            Type::Primitive(Primitive::I32)   => "i32".to_string(),
            Type::Primitive(Primitive::U32)   => "u32".to_string(),
            Type::Primitive(Primitive::I64)   => "i64".to_string(),
            Type::Primitive(Primitive::U64)   => "u64".to_string(),
            Type::Primitive(Primitive::USize) => "usize".to_string(),
            Type::Primitive(Primitive::F32)   => "f32".to_string(),
            Type::Primitive(Primitive::F64)   => "f64".to_string(),
            Type::Primitive(Primitive::Void)  => "void".to_string(),
            Type::Tuple(fields) => {
                "(" .to_string()
                + &fields.iter().map(|t| t.to_string(stringtab)).collect::<Vec<_>>().join(", ")
                + ")"
            },
            Type::Array(elem, dims) => {
                elem.to_string(stringtab)
                + "["
                + &dims.iter().map(|d| d.to_string(stringtab)).collect::<Vec<_>>().join(", ")
                + "]"
            },
            Type::Struct { name, .. } => {
                stringtab.lookup_id(*name).unwrap()
            },
        }
    }
    
    fn is_numeric(&self) -> bool {
        match self {
              Type::Primitive(Primitive::I8)  | Type::Primitive(Primitive::U8)
            | Type::Primitive(Primitive::I16) | Type::Primitive(Primitive::U16)
            | Type::Primitive(Primitive::I32) | Type::Primitive(Primitive::U32)
            | Type::Primitive(Primitive::I64) | Type::Primitive(Primitive::U64)
            | Type::Primitive(Primitive::F32) | Type::Primitive(Primitive::F64)
            | Type::Primitive(Primitive::USize) => true,
            _ => false,
        }
    }

    fn is_integer(&self) -> bool {
        match self {
              Type::Primitive(Primitive::I8)  | Type::Primitive(Primitive::U8)
            | Type::Primitive(Primitive::I16) | Type::Primitive(Primitive::U16)
            | Type::Primitive(Primitive::I32) | Type::Primitive(Primitive::U32)
            | Type::Primitive(Primitive::I64) | Type::Primitive(Primitive::U64)
            | Type::Primitive(Primitive::USize) => true,
            _ => false,
        }
    }

    fn is_float(&self) -> bool {
        match self {
            Type::Primitive(Primitive::F32) | Type::Primitive(Primitive::F64)
                => true,
            _ => false,
        }
    }

    fn is_boolean(&self) -> bool {
        match self {
            Type::Primitive(Primitive::Bool) => true,
            _ => false,
        }
    }

    fn is_void(&self) -> bool {
        match self {
            Type::Primitive(Primitive::Void) => true,
            _ => false,
        }
    }
}

#[derive(Clone)]
enum GoalType {
    KnownType(Type),
    StructType { field : usize, field_type : Box<GoalType> },
    TupleType  { index : usize, index_type : Box<GoalType> },
    ArrayType  { element_type : Box<GoalType>, num_dims : usize },
    
    AnyType, AnyNumeric, AnyInteger
}

impl GoalType {
    fn to_string(&self, stringtab : &StringTable) -> String {
        match self {
            GoalType::KnownType(ty) => ty.to_string(stringtab),

            GoalType::StructType { field, field_type } => {
                format!("{{ {} : {}, ... }}",
                        stringtab.lookup_id(*field).unwrap(),
                        field_type.to_string(stringtab))
            },
            GoalType::TupleType { index, index_type } => {
                format!("({}{}, ...)",
                        "*, ".repeat(*index),
                        index_type.to_string(stringtab))
            },
            GoalType::ArrayType { element_type, num_dims } => {
                format!("{}{}",
                        element_type.to_string(stringtab),
                        "[*]".repeat(*num_dims))
            },

            GoalType::AnyType => format!("*"),
            GoalType::AnyNumeric => format!("numeric"),
            GoalType::AnyInteger => format!("integer"),
        }
    }

    fn is_numeric(&self) -> bool {
        match self {
            GoalType::KnownType(ty) => ty.is_integer(),
            GoalType::AnyType | GoalType::AnyNumeric | GoalType::AnyInteger 
                => true,
            _ => false,
        }
    }

    fn as_numeric(&self) -> &GoalType {
        match self {
            GoalType::KnownType(ty) => { assert!(ty.is_numeric()); self },
            GoalType::AnyType | GoalType::AnyNumeric => &GoalType::AnyNumeric,
            GoalType::AnyInteger => &GoalType::AnyInteger,
            _ => panic!("Call to GoalType::as_numeric() on non numeric type"),
        }
    }

    fn is_integer(&self) -> bool {
        match self {
            GoalType::KnownType(ty) => ty.is_integer(),
            GoalType::AnyType | GoalType::AnyNumeric | GoalType::AnyInteger 
                => true,
            _ => false,
        }
    }

    fn get_integer(&self) -> &Type {
        match self {
            GoalType::KnownType(ty) => { assert!(ty.is_integer()); ty },
            GoalType::AnyType | GoalType::AnyNumeric | GoalType::AnyInteger
                => &Type::Primitive(Primitive::I64),
            _ => panic!("Call to GoalType::get_integer() on non integer type"),
        }
    }

    fn as_integer(&self) -> &GoalType {
        match self {
            GoalType::KnownType(ty) => { assert!(ty.is_integer()); self },
            GoalType::AnyType | GoalType::AnyNumeric | GoalType::AnyInteger
                => &GoalType::AnyInteger,
            _ => panic!("Call to GoalType::as_integer() no non integer type"),
        }
    }

    fn is_float(&self) -> bool {
        match self {
            GoalType::KnownType(ty) => ty.is_float(),
            GoalType::AnyType | GoalType::AnyNumeric => true,
            _ => false,
        }
    }

    fn get_float(&self) -> &Type {
        match self {
            GoalType::KnownType(ty) => { assert!(ty.is_float()); ty },
            GoalType::AnyType | GoalType::AnyNumeric => &Type::Primitive(Primitive::F64),
            _ => panic!("Call to GoalType::get_float() on non float type"),
        }
    }

    fn is_boolean(&self) -> bool {
        match self {
            GoalType::KnownType(ty) => ty.is_boolean(),
            GoalType::AnyType => true,
            _ => false,
        }
    }

    fn get_boolean(&self) -> &Type {
        match self {
            GoalType::KnownType(ty) => { assert!(ty.is_boolean()); ty },
            GoalType::AnyType => &Type::Primitive(Primitive::Bool),
            _ => panic!("Call to GoalType::get_boolean() on non bool type"),
        }
    }

    fn matches(&self, typ : &Type) -> bool {
        match self {
            // TODO: Upcasts?
            GoalType::KnownType(ty) => ty == typ,

            GoalType::StructType { field, field_type } => {
                match typ {
                    Type::Struct { name : _, id : _, fields, names } => {
                        match names.get(field) {
                            None => false,
                            Some(idx) => field_type.matches(&fields[*idx]),
                        }
                    },
                    _ => false,
                }
            },
            GoalType::TupleType { index, index_type } => {
                match typ {
                    Type::Tuple(fields) => index_type.matches(&fields[*index]),
                    _ => false,
                }
            },
            GoalType::ArrayType { element_type, num_dims } => {
                match typ {
                    Type::Array(elem_typ, dims) => {
                        *num_dims == dims.len() && element_type.matches(elem_typ)
                    },
                    _ => false,
                }
            },

            GoalType::AnyType => true,
            GoalType::AnyNumeric => typ.is_numeric(),
            GoalType::AnyInteger => typ.is_integer(),
        }
    }
}

enum Entity {
    // A variable has a variable number to distinguish shadowing
    Variable { variable : usize, typ : Type, is_const : bool },
    Type     { value : Type },
    DynConst { value : usize }, // dynamic constant number
}

// Map strings to unique identifiers and counts uids
struct StringTable {
    count : usize,
    string_to_index : HashMap<String, usize>,
    index_to_string : HashMap<usize, String>,
}
impl StringTable {
    fn new() -> StringTable {
        StringTable { count : 0,
                      string_to_index : HashMap::new(),
                      index_to_string : HashMap::new(), }
    }

    fn lookup_string(&mut self, s : String) -> usize {
        match self.string_to_index.get(&s) {
            Some(n) => *n,
            None => {
                let n = self.count;
                self.count += 1;
                self.string_to_index.insert(s.clone(), n);
                self.index_to_string.insert(n, s);
                n
            },
        }
    }

    fn lookup_id(&self, n : usize) -> Option<String> {
        self.index_to_string.get(&n).cloned()
    }
}

// Start line and column, end line and column
pub type Location = ((usize, usize), (usize, usize));
#[derive(Debug, Clone)]
pub enum ErrorMessage {
    NotImplemented(Location, String),
    SyntaxError(String),
    SemanticError(Location, String), // Other errors, with a location and description
    // Undefined variable at location, variable name
    UndefinedVariable(Location, String),
    // Kind error at location, expected, actual)
    KindError(Location, String, String),
    // Type error at location, expected type, actual type)
    TypeError(Location, String, String),
}

pub type ErrorMessages = LinkedList<ErrorMessage>;

fn singleton_error(err : ErrorMessage) -> ErrorMessages {
    LinkedList::from([err])
}

fn get_errors<A>(x : &Result<A, ErrorMessages>) -> ErrorMessages {
    match x {
        Err(errs) => errs.clone(),
        Ok(_) => LinkedList::new(),
    }
}

fn append_errors2<A, B>(x : Result<A, ErrorMessages>, y : Result<B, ErrorMessages>)
    -> Result<(A, B), ErrorMessages> {
    let mut errs_x = get_errors(&x);
    let mut errs_y = get_errors(&y);

    errs_x.append(&mut errs_y);

    if !errs_x.is_empty() {
        Err(errs_x)
    } else {
        Ok((x.expect("Above"), y.expect("Above")))
    }
}

fn append_errors3<A, B, C>(x : Result<A, ErrorMessages>, y : Result<B, ErrorMessages>,
                           z : Result<C, ErrorMessages>) -> Result<(A, B, C), ErrorMessages> {
    let mut errs_x = get_errors(&x);
    let mut errs_y = get_errors(&y);
    let mut errs_z = get_errors(&z);

    errs_x.append(&mut errs_y);
    errs_x.append(&mut errs_z);

    if !errs_x.is_empty() {
        Err(errs_x)
    } else {
        Ok((x.expect("Above"), y.expect("Above"), z.expect("Above")))
    }
}

fn span_to_loc(span : Span, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>)
    -> Location {
    lexer.line_col(span)
}

// Loop info is a stack of the loop levels, recording the latch and exit block of each
type LoopInfo = Vec<(NodeID, NodeID)>;

pub fn process_program(src_file : String) -> Result<Module, ErrorMessages> {
    let mut stringtab = StringTable::new();
    let mut file = File::open(src_file.clone()).expect("PANIC: Unable to open input file.");
    parse_file(&mut file, &mut stringtab)
}

fn intern_id(n : &Span, lex : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
             stringtab : &mut StringTable) -> usize {
    stringtab.lookup_string(lex.span_str(*n).to_string())
}

fn intern_package_name(
    n : &PackageName, lex : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
    stringtab : &mut StringTable) -> Vec<usize> {

    let mut res = vec![];
    for s in n {
        res.push(intern_id(s, lex, stringtab));
    }
    res
}

fn parse_file(file : &mut File, stringtab : &mut StringTable)
    -> Result<Module, ErrorMessages> {
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("PANIC: Unable to read input file contents.");

    let lexerdef = lang_l::lexerdef();
    let lexer = lexerdef.lexer(&contents);
    let (res, errs) = lang_y::parse(&lexer);
    if errs.is_empty() {
        match res {
            Some(Ok(r))    => prepare_program(r, &lexer, stringtab),
            None           => Err(singleton_error(
                    ErrorMessage::SyntaxError("Parser did not return program".to_string()))),
            Some(Err(())) => Err(singleton_error(
                    ErrorMessage::SyntaxError("Unspecified parse error".to_string()))),
        }
    } else {
        let intern_errs = errs.iter()
                              .map(|e| ErrorMessage::SyntaxError(e.pp(&lexer, &lang_y::token_epp)))
                              .collect::<LinkedList<_>>();
        Err(intern_errs)
    }
}

fn prepare_program(
    prg : lang_y::Prg, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
    stringtab : &mut StringTable) -> Result<Module, ErrorMessages> {

    let mut env : Env<usize, Entity> = Env::new();
    let mut builder = Builder::create();

    env.open_scope();

    for top in prg {
        match top {
            lang_y::Top::Import { span, name: _ } => {
                return Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "imports".to_string())));
            },
            lang_y::Top::TypeDecl { span, public: _, name, ty_vars, body } => {
                // TODO: Handle public
                if ty_vars.len() > 0 {
                    return Err(singleton_error(
                            ErrorMessage::NotImplemented(
                                span_to_loc(span, lexer),
                                "type parameters".to_string())));
                }

                let nm = intern_id(&name, lexer, stringtab);
                let typ = process_type_def(body, nm, lexer, stringtab, &mut env)?;
                env.insert(nm, Entity::Type { value : typ });
            },
            lang_y::Top::ConstDecl { span, public: _, name: _, ty: _, body: _ } => {
                return Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "constants".to_string())));
            },
            lang_y::Top::FuncDecl { span, public: _, attr: _, name, ty_vars, args, ty, body } => {
                // TODO: Handle public, attributes
                env.open_scope();

                let mut num_dyn_const : u32 = 0;
                for TypeVar { span, name, kind } in ty_vars {
                    match kind {
                        Kind::USize => {
                            let num = num_dyn_const as usize;
                            num_dyn_const += 1;

                            let nm = intern_id(&name, lexer, stringtab);
                            env.insert(nm, Entity::DynConst { value : num });
                        },
                        _ => {
                            return Err(singleton_error(
                                    ErrorMessage::NotImplemented(
                                        span_to_loc(span, lexer),
                                        "type parameters".to_string())));
                        },
                    }
                }

                let mut arg_types : Vec<(usize, Type)> = vec![]; // list of pairs of name and type
                let mut inout_args = vec![]; // list of indices into args
                let mut errors = LinkedList::new();

                for (inout, VarBind { span, pattern, typ }) in args {
                    if !typ.is_some() {
                        errors.push_back(
                            ErrorMessage::NotImplemented(
                                span_to_loc(span, lexer),
                                "argument type inference".to_string()));
                        continue;
                    }

                    match pattern {
                        SPattern::Variable { span, name } => {
                            if name.len() != 1 {
                                errors.push_back(
                                    ErrorMessage::SemanticError(
                                        span_to_loc(span, lexer),
                                        "Bound variables must be local names, without a package separator".to_string()));
                                continue;
                            }

                            let nm = intern_package_name(&name, lexer, stringtab)[0];
                            match process_type(typ.expect("FROM ABOVE"), lexer, stringtab, &env) {
                                Ok(ty) => {
                                    if inout.is_some() {
                                        inout_args.push(arg_types.len());
                                    }
                                    arg_types.push((nm, ty));
                                },
                                Err(mut errs) => { errors.append(&mut errs); },
                            }
                        },
                        _ => {
                            errors.push_back(
                                ErrorMessage::NotImplemented(
                                    span_to_loc(span, lexer),
                                    "patterns in arguments".to_string()));
                        },
                    }
                }

                let return_type =
                    match ty {
                        None => {
                            errors.push_back(
                                ErrorMessage::NotImplemented(
                                    span_to_loc(span, lexer),
                                    "function return type inference".to_string()));
                            Type::Primitive(Primitive::Void)
                        },
                        Some(ty) => {
                            match process_type(ty, lexer, stringtab, &env) {
                                Ok(ty) => ty,
                                Err(mut errs) => {
                                    errors.append(&mut errs);
                                    Type::Primitive(Primitive::Void)
                                },
                            }
                        },
                    };

                if !errors.is_empty() {
                    return Err(errors);
                }

                let mut param_types = vec![];
                for (_, ty) in &arg_types {
                    param_types.push(build_type(ty, &mut builder));
                }

                let mut inout_types = vec![];
                for arg_idx in &inout_args {
                    inout_types.push(arg_types[*arg_idx].1.clone());
                }
                
                let pure_return_type
                    = Type::Tuple(vec![return_type.clone(),
                                  Type::Tuple(inout_types.clone())]);
                let return_type_built =
                    build_type(&pure_return_type, &mut builder);

                // TODO: Gracefully handle a possible failure here
                let (func, entry) 
                    = builder.create_function(lexer.span_str(name),
                                              param_types,
                                              return_type_built,
                                              num_dyn_const).unwrap();
                
                let mut ssa : SSA = SSA::new(func, entry);
                
                let mut arg_variables = vec![];

                let mut idx = 0;
                for (nm, ty) in arg_types {
                    let mut node_builder = builder.allocate_node(func);
                    let node = node_builder.id();
                    node_builder.build_parameter(idx);
                    let _ = builder.add_node(node_builder);

                    let variable = env.uniq();
                    env.insert(nm,
                               Entity::Variable {
                                   variable : variable,
                                   typ : ty,
                                   is_const : false });
                    arg_variables.push(variable);

                    ssa.write_variable(variable, entry, node);

                    idx += 1;
                }

                let inout_variables = inout_args.iter().map(|i| arg_variables[*i]).collect::<Vec<_>>();

                // Finally, we have a properly built environment and we can
                // start processing the body
                match process_stmt(body, lexer, stringtab, &mut env, &mut builder, func, &mut ssa,
                                   entry, &mut vec![], &return_type, &inout_types, &inout_variables)? {
                    None => {},
                    Some(block) => {
                        // The fact that some block is returned indicates that there is possibly
                        // some path that does not reach a return. This is an error unless the
                        // return type is "void"
                        if return_type.is_void() {
                            // Insert return at the end
                            // ( (), (...return_variables...) )
                            let unit_const = builder.create_constant_prod(vec![].into());
                            let mut unit_build = builder.allocate_node(func);
                            let unit_value = unit_build.id();
                            unit_build.build_constant(unit_const);
                            let _ = builder.add_node(unit_build);

                            build_return(&return_type, unit_value, block,
                                         &inout_types, &inout_variables,
                                         &mut builder, func, &mut ssa);
                        } else {
                            return Err(singleton_error(
                                    ErrorMessage::SemanticError(
                                        span_to_loc(span, lexer),
                                        "May reach end of control without return".to_string())));
                        }
                    },
                }

                env.close_scope();
            },
            lang_y::Top::ModDecl { span, public: _, name: _, body: _ } => {
                return Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "modules".to_string())));
            },
        }
    }

    Ok(builder.finish())
}

fn process_type_def(
    def : lang_y::TyDef, name : usize,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
    stringtab : &mut StringTable, env : &mut Env<usize, Entity>)
    -> Result<Type, ErrorMessages> {
    
    match def {
        lang_y::TyDef::TypeAlias { span: _, body } => {
            process_type(body, lexer, stringtab, env)
        },
        lang_y::TyDef::Struct { span: _, public: _, fields } => {
            // TODO: handle public correctly (and field public)
            
            let mut field_list = vec![];
            let mut field_map = HashMap::new();
            let mut errors = LinkedList::new();

            for ObjField { span, public: _, name, typ } in fields {
                let nm = intern_id(&name, lexer, stringtab);
                match typ {
                    None => {
                        errors.push_back(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                "struct fields must have a type".to_string()));
                    },
                    Some(ty) => {
                        match process_type(ty, lexer, stringtab, env) {
                            Ok(typ) => {
                                let idx = field_list.len();
                                field_list.push(typ);
                                field_map.insert(nm, idx);
                            },
                            Err(mut errs) => errors.append(&mut errs),
                        }
                    },
                }
            }

            if !errors.is_empty() {
                Err(errors)
            } else {
                Ok(Type::Struct { name : name,
                                  id : env.uniq(),
                                  fields : field_list,
                                  names : field_map })
            }
        },
        lang_y::TyDef::Union { span, public: _, fields: _ } => {
            Err(singleton_error(
                    ErrorMessage::NotImplemented(
                        span_to_loc(span, lexer),
                        "unions".to_string())))
        },
    }
}

fn process_type(
    typ : lang_y::Type, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
    stringtab : &mut StringTable, env : &Env<usize, Entity>)
    -> Result<Type, ErrorMessages> {

    match typ {
        lang_y::Type::PrimType { span: _, typ } => {
            Ok(Type::Primitive(typ))
        },
        lang_y::Type::TupleType { span: _, tys } => {
            let mut fields = vec![];
            let mut errors = LinkedList::new();

            for ty in tys {
                match process_type(ty, lexer, stringtab, env) {
                    Ok(t) => fields.push(t),
                    Err(mut errs) => errors.append(&mut errs),
                }
            }

            if !errors.is_empty() {
                Err(errors)
            } else {
                if fields.len() == 1 {
                    Ok(fields.pop().expect("Length"))
                } else {
                    Ok(Type::Tuple(fields))
                }
            }
        },
        lang_y::Type::NamedType { span, name, args } => {
            if args.len() > 0 {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "type parameters".to_string())))
            } else if name.len() != 1 {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "packages".to_string())))
            } else {
                let id = intern_package_name(&name, lexer, stringtab);
                let nm = id[0];
                match env.lookup(&nm) {
                    Some(Entity::Type { value }) => Ok(value.clone()),
                    Some(_) =>
                        Err(singleton_error(
                                ErrorMessage::KindError(
                                    span_to_loc(span, lexer),
                                    "type".to_string(),
                                    "value".to_string()))),
                    None =>
                        Err(singleton_error(
                                ErrorMessage::UndefinedVariable(
                                    span_to_loc(span, lexer),
                                    stringtab.lookup_id(nm).unwrap()))),
                }
            }
        },
        lang_y::Type::ArrayType { span: _, elem, dims } => {
            let mut dimensions = vec![];
            let mut errors = LinkedList::new();

            let element = process_type(*elem, lexer, stringtab, env);

            for dim in dims {
                match process_type_expr_as_expr(dim, lexer, stringtab, env) {
                    Err(mut errs) => errors.append(&mut errs),
                    Ok(ex) => dimensions.push(ex),
                }
            }

            match element {
                Err(mut errs) => {
                    errs.append(&mut errors);
                    Err(errs)
                },
                Ok(element_type) => {
                    if !errors.is_empty() {
                        Err(errors)
                    } else {
                        match element_type {
                            Type::Array(inner_elem, mut inner_dims) => {
                                dimensions.append(&mut inner_dims);
                                Ok(Type::Array(inner_elem, dimensions))
                            },
                            elem_type => {
                                Ok(Type::Array(Box::new(elem_type), dimensions))
                            },
                        }
                    }
                }
            }
        },
    }
}

fn process_type_expr_as_expr(
    exp : lang_y::TypeExpr, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
    stringtab : &mut StringTable, env : &Env<usize, Entity>)
    -> Result<DynamicConstant, ErrorMessages> {

    match exp {
        lang_y::TypeExpr::PrimType { span, .. }
        | lang_y::TypeExpr::TupleType { span, .. }
        | lang_y::TypeExpr::ArrayTypeExpr { span, .. } =>
            Err(singleton_error(
                    ErrorMessage::KindError(
                        span_to_loc(span, lexer),
                        "dynamic constant expression".to_string(),
                        "type".to_string()))),

        lang_y::TypeExpr::NamedTypeExpr { span, name, args } => {
            if args.len() > 0 {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "type parameters".to_string())))
            } else if name.len() != 1 {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "packages".to_string())))
            } else {
                let id = intern_package_name(&name, lexer, stringtab);
                let nm = id[0];
                match env.lookup(&nm) {
                    Some(Entity::DynConst { value }) => {
                        Ok(DynamicConstant::DynConst(nm, *value))
                    },
                    Some(Entity::Variable { .. }) =>
                        Err(singleton_error(
                                ErrorMessage::KindError(
                                    span_to_loc(span, lexer),
                                    "dynamic constant expression".to_string(),
                                    "runtime variable".to_string()))),
                    Some(Entity::Type { .. }) =>
                        Err(singleton_error(
                                ErrorMessage::KindError(
                                    span_to_loc(span, lexer),
                                    "dynamic constant expression".to_string(),
                                    "type".to_string()))),
                    None =>
                        Err(singleton_error(
                                ErrorMessage::UndefinedVariable(
                                    span_to_loc(span, lexer),
                                    stringtab.lookup_id(nm).unwrap()))),
                }
            }
        },
        lang_y::TypeExpr::IntLiteral { span, base } => {
            let res = usize::from_str_radix(lexer.span_str(span), base.base());
            assert!(res.is_ok(), "Internal Error: Int literal is not an integer");
            Ok(DynamicConstant::Constant(res.unwrap()))
        },

        lang_y::TypeExpr::Negative { span, .. }
        | lang_y::TypeExpr::Add { span, .. }
        | lang_y::TypeExpr::Sub { span, .. }
        | lang_y::TypeExpr::Mul { span, .. } =>
            Err(singleton_error(
                    ErrorMessage::NotImplemented(
                        span_to_loc(span, lexer),
                        "expressions of dynamic constants".to_string()))),
    }
}

// The return is the block that a statement after this statement should go in,
// it may be None in the case of a return
fn process_stmt<'a>(
    stmt : lang_y::Stmt, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
    stringtab : &mut StringTable, env : &mut Env<usize, Entity>,
    builder : &mut Builder<'a>, func : FunctionID, ssa : &mut SSA,
    pred : NodeID, loops : &mut LoopInfo, return_type : &Type,
    inout_types : &Vec<Type>, inout_vars : &Vec<usize>) -> Result<Option<NodeID>, ErrorMessages> {

    match stmt {
        lang_y::Stmt::LetStmt { span: _, var : VarBind { span : v_span, pattern, typ }, init } => {
            match pattern {
                SPattern::Variable { span, name } => {
                    if typ.is_none() {
                        return Err(singleton_error(
                                ErrorMessage::NotImplemented(
                                    span_to_loc(v_span, lexer),
                                    "variable type inference".to_string())));
                    }

                    if name.len() != 1 {
                        return Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(span, lexer),
                                    "Bound variables must be local names, without a package separator".to_string())));
                    }

                    let nm = intern_package_name(&name, lexer, stringtab)[0];
                    let ty = process_type(typ.expect("FROM ABOVE"), lexer, stringtab, env)?;

                    let var = env.uniq();

                    let val =
                        match init {
                            Some(exp) => {
                                process_expr(exp, lexer, stringtab, env, builder, func, ssa,
                                             pred, &GoalType::KnownType(ty.clone()))?.0
                            },
                            None => {
                                let init = build_default(&ty, builder);
                                let mut node_builder = builder.allocate_node(func);
                                let node = node_builder.id();
                                node_builder.build_constant(init);
                                let _ = builder.add_node(node_builder);
                                node
                            },
                        };

                    env.insert(nm,
                               Entity::Variable { variable : var, typ : ty, is_const : false });
                    ssa.write_variable(var, pred, val);

                    Ok(Some(pred))
                },
                _ => {
                    Err(singleton_error(
                            ErrorMessage::NotImplemented(
                                span_to_loc(v_span, lexer),
                                "non-variable bindings".to_string())))
                },
            }
        },
        lang_y::Stmt::ConstStmt { span, var: _, init: _ } => {
            Err(singleton_error(
                    ErrorMessage::NotImplemented(
                        span_to_loc(span, lexer),
                        "constant bindings".to_string())))
        },
        lang_y::Stmt::AssignStmt { span: _, lhs, assign, assign_span, rhs } => {
            let (var, typ, index) = process_lexpr(lhs, lexer, stringtab, env, builder,
                                                  func, ssa, pred)?;

            let val = process_expr(rhs, lexer, stringtab, env, builder, func,
                                   ssa, pred, &GoalType::KnownType(typ.clone()))?.0;

            if assign == AssignOp::None {
                if index.is_empty() {
                    ssa.write_variable(var, pred, val);
                } else {
                    let init_value = ssa.read_variable(var, pred, builder);

                    let mut update_builder = builder.allocate_node(func);
                    ssa.write_variable(var, pred, update_builder.id());

                    update_builder.build_write(init_value, val, index.into());
                    let _ = builder.add_node(update_builder);
                }
                
                Ok(Some(pred))
            } else {
                let op =
                    match assign {
                        AssignOp::None => panic!("Impossible"),
                        AssignOp::Add => {
                            if !typ.is_numeric() {
                                return Err(singleton_error(
                                        ErrorMessage::SemanticError(
                                            span_to_loc(assign_span, lexer),
                                            format!("Operator += cannot be applied to type {}",
                                                    typ.to_string(stringtab)))));
                            }
                            BinaryOperator::Add
                        },
                        AssignOp::Sub => {
                            if !typ.is_numeric() {
                                return Err(singleton_error(
                                        ErrorMessage::SemanticError(
                                            span_to_loc(assign_span, lexer),
                                            format!("Operator -= cannot be applied to type {}",
                                                    typ.to_string(stringtab)))));
                            }
                            BinaryOperator::Sub
                        },
                        AssignOp::Mul => {
                            if !typ.is_numeric() {
                                return Err(singleton_error(
                                        ErrorMessage::SemanticError(
                                            span_to_loc(assign_span, lexer),
                                            format!("Operator *= cannot be applied to type {}",
                                                    typ.to_string(stringtab)))));
                            }
                            BinaryOperator::Mul
                        },
                        AssignOp::Div => {
                            if !typ.is_numeric() {
                                return Err(singleton_error(
                                        ErrorMessage::SemanticError(
                                            span_to_loc(assign_span, lexer),
                                            format!("Operator /= cannot be applied to type {}",
                                                    typ.to_string(stringtab)))));
                            }
                            BinaryOperator::Div
                        },
                        AssignOp::Mod => {
                            if !typ.is_integer() {
                                return Err(singleton_error(
                                        ErrorMessage::SemanticError(
                                            span_to_loc(assign_span, lexer),
                                            format!("Operator %= cannot be applied to type {}",
                                                    typ.to_string(stringtab)))));
                            }
                            BinaryOperator::Rem
                        },
                        AssignOp::BitAnd => {
                            if !typ.is_integer() {
                                return Err(singleton_error(
                                        ErrorMessage::SemanticError(
                                            span_to_loc(assign_span, lexer),
                                            format!("Operator &= cannot be applied to type {}",
                                                    typ.to_string(stringtab)))));
                            }
                            BinaryOperator::And
                        },
                        AssignOp::BitOr => {
                            if !typ.is_integer() {
                                return Err(singleton_error(
                                        ErrorMessage::SemanticError(
                                            span_to_loc(assign_span, lexer),
                                            format!("Operator |= cannot be applied to type {}",
                                                    typ.to_string(stringtab)))));
                            }
                            BinaryOperator::Or
                        },
                        AssignOp::Xor => {
                            if !typ.is_integer() {
                                return Err(singleton_error(
                                        ErrorMessage::SemanticError(
                                            span_to_loc(assign_span, lexer),
                                            format!("Operator ^= cannot be applied to type {}",
                                                    typ.to_string(stringtab)))));
                            }
                            BinaryOperator::Xor
                        },
                        AssignOp::LogAnd => {
                            if !typ.is_boolean() {
                                return Err(singleton_error(
                                        ErrorMessage::SemanticError(
                                            span_to_loc(assign_span, lexer),
                                            format!("Operator &&= cannot be applied to type {}",
                                                    typ.to_string(stringtab)))));
                            }
                            return Err(singleton_error(
                                    ErrorMessage::NotImplemented(
                                        span_to_loc(assign_span, lexer),
                                        "&&= operator".to_string())));
                        },
                        AssignOp::LogOr => {
                            if !typ.is_boolean() {
                                return Err(singleton_error(
                                        ErrorMessage::SemanticError(
                                            span_to_loc(assign_span, lexer),
                                            format!("Operator ||= cannot be applied to type {}",
                                                    typ.to_string(stringtab)))));
                            }
                            return Err(singleton_error(
                                    ErrorMessage::NotImplemented(
                                        span_to_loc(assign_span, lexer),
                                        "||= operator".to_string())));
                        },
                        AssignOp::LShift => {
                            if !typ.is_integer() {
                                return Err(singleton_error(
                                        ErrorMessage::SemanticError(
                                            span_to_loc(assign_span, lexer),
                                            format!("Operator <<= cannot be applied to type {}",
                                                    typ.to_string(stringtab)))));
                            }
                            BinaryOperator::LSh
                        },
                        AssignOp::RShift => {
                            if !typ.is_integer() {
                                return Err(singleton_error(
                                        ErrorMessage::SemanticError(
                                            span_to_loc(assign_span, lexer),
                                            format!("Operator >>= cannot be applied to type {}",
                                                    typ.to_string(stringtab)))));
                            }
                            BinaryOperator::RSh
                        },
                    };
                
                let var_value = ssa.read_variable(var, pred, builder);
                let init_value = 
                    if index.is_empty() {
                        var_value
                    } else {
                        let mut extract_builder = builder.allocate_node(func);
                        let node = extract_builder.id();
                        extract_builder.build_read(var_value, index.clone().into());
                        let _ = builder.add_node(extract_builder);
                        node
                    };

                let mut compute_builder = builder.allocate_node(func);
                let compute_node = compute_builder.id();
                compute_builder.build_binary(init_value, val, op);
                let _ = builder.add_node(compute_builder);
                
                if index.is_empty() {
                    ssa.write_variable(var, pred, compute_node);
                } else {
                    let mut update_builder  = builder.allocate_node(func);
                    ssa.write_variable(var, pred, update_builder.id());
                    update_builder.build_write(var_value, compute_node, index.into());
                    let _ = builder.add_node(update_builder);
                }
                
                Ok(Some(pred))
            }
        },
        lang_y::Stmt::IfStmt { span: _, cond, thn, els } => {
            // Setup control flow that we need
            let (mut if_node, then_block, else_block) = ssa.create_cond(builder, pred);

            let cond_val = process_expr(cond, lexer, stringtab, env, builder,
                                        func, ssa, pred,
                                        &GoalType::KnownType(Type::Primitive(Primitive::Bool)));

            env.open_scope();
            let then_end = process_stmt(*thn, lexer, stringtab, env, builder,
                                        func, ssa, then_block, loops, return_type,
                                        inout_types, inout_vars);
            env.close_scope();

            let else_end =
                match els {
                    None => Ok(Some(else_block)),
                    Some(els_stmt) => {
                        env.open_scope();
                        let res = process_stmt(*els_stmt, lexer, stringtab, env, builder,
                                               func, ssa, else_block, loops, return_type,
                                               inout_types, inout_vars);
                        env.close_scope();
                        res
                    },
                };

            let ((cond_val, _), then_term, else_term) = append_errors3(cond_val, then_end, else_end)?;

            if_node.build_if(pred, cond_val);
            let _ = builder.add_node(if_node);

            match (then_term, else_term) {
                (None, els) => Ok(els),
                (thn, None) => Ok(thn),
                (Some(then_term), Some(else_term)) => {
                    let join_node = ssa.create_block(builder);
                    ssa.add_pred(join_node, then_term);
                    ssa.add_pred(join_node, else_term);
                    ssa.seal_block(join_node, builder);
                    Ok(Some(join_node))
                },
            }
        },
        lang_y::Stmt::MatchStmt { span, expr: _, body: _ } => {
            Err(singleton_error(
                    ErrorMessage::NotImplemented(
                        span_to_loc(span, lexer),
                        "match statements".to_string())))
        },
        lang_y::Stmt::ForStmt { span: _, var : VarBind { span : v_span, pattern, typ },
                                init, bound, step, body } => {
            let latch = ssa.create_block(builder);
            let update = ssa.create_block(builder);

            ssa.add_pred(latch, pred);
            ssa.add_pred(latch, update);
            ssa.seal_block(latch, builder);

            let (mut if_node, body_node, false_proj) = ssa.create_cond(builder, latch);
            let exit = ssa.create_block(builder);

            ssa.add_pred(exit, false_proj);

            let (var, var_name, var_type) =
                match pattern {
                    SPattern::Variable { span, name } => {
                        if name.len() != 1 {
                            return Err(singleton_error(
                                    ErrorMessage::SemanticError(
                                        span_to_loc(span, lexer),
                                        "Bound variables must be local names, without a package separator".to_string())));
                        }

                        let nm = intern_package_name(&name, lexer, stringtab)[0];
                        let var_type =
                            match typ {
                                None => Type::Primitive(Primitive::U64),
                                Some(t) => {
                                    let ty = process_type(t, lexer, stringtab, env)?;
                                    if !ty.is_integer() {
                                        return Err(singleton_error(
                                                ErrorMessage::SemanticError(
                                                    span_to_loc(v_span, lexer),
                                                    "For loop variables must be integers".to_string())));
                                    }
                                    ty
                                },
                            };

                        let var = env.uniq();
                        (var, nm, var_type)
                    },
                    _ => {
                        return Err(singleton_error(
                                ErrorMessage::NotImplemented(
                                    span_to_loc(v_span, lexer),
                                    "patterns in for loop arguments".to_string())));
                    },
                };
            
            // Evaluate the initial value, bound, and step in the predecessor
            let init_res = process_expr(init, lexer, stringtab, env, builder,
                                        func, ssa, pred,
                                        &GoalType::KnownType(var_type.clone()));

            let bound_res = process_expr(bound, lexer, stringtab, env, builder,
                                         func, ssa, pred,
                                         &GoalType::KnownType(var_type.clone()));
            let (step_val, step_pos) =
                match step {
                    None => {
                        (build_int_const(&var_type, 1, builder, func), true)
                    },
                    Some((negative, span, base)) => {
                        let val = u64::from_str_radix(lexer.span_str(span), base.base());
                        assert!(val.is_ok(), "Internal Error: Int literal is not an integer");
                        let num = val.unwrap();
                        if num == 0 {
                            return Err(singleton_error(
                                    ErrorMessage::SemanticError(
                                        span_to_loc(span, lexer),
                                        "For loop step cannot be 0".to_string())));
                        }

                        (build_int_const(&var_type, num, builder, func), !negative)
                    },
                };

            let ((init_val, _), (bound_val, _)) = append_errors2(init_res, bound_res)?;

            // Setup initial value
            ssa.write_variable(var, pred, init_val);
           
            // Build the update
            let mut update_node = builder.allocate_node(func);
            let updated = update_node.id();
            let loop_var = ssa.read_variable(var, update, builder);
            update_node.build_binary(loop_var, step_val,
                                     if step_pos { BinaryOperator::Add }
                                     else { BinaryOperator::Sub });
            let _ = builder.add_node(update_node);
            ssa.write_variable(var, update, updated);

            // Build the condition
            let condition = {
                let mut compare_node = builder.allocate_node(func);
                let cond = compare_node.id();
                let loop_var = ssa.read_variable(var, latch, builder);
                
                compare_node.build_binary(loop_var, bound_val,
                                          if step_pos { BinaryOperator::LT }
                                          else { BinaryOperator::GT });
                
                let _ = builder.add_node(compare_node);
                cond
            };

            env.open_scope();
            loops.push((update, exit));
            env.insert(var_name, Entity::Variable {
                                    variable : var, typ : var_type, is_const : true });

            let body_term = process_stmt(*body, lexer, stringtab, env, builder,
                                         func, ssa, body_node, loops, return_type,
                                         inout_types, inout_vars)?;

            env.close_scope();
            loops.pop();

            if_node.build_if(latch, condition);
            let _ = builder.add_node(if_node);

            match body_term { 
                None => {},
                Some(block) => { ssa.add_pred(update, block); },
            }

            ssa.seal_block(update, builder);
            ssa.seal_block(exit, builder);

            Ok(Some(exit))
        },
        lang_y::Stmt::WhileStmt { span: _, cond, body } => {
            let latch = ssa.create_block(builder);
            ssa.add_pred(latch, pred);
            
            let (mut if_node, body_node, false_proj) = ssa.create_cond(builder, latch);
            let exit = ssa.create_block(builder);

            ssa.add_pred(exit, false_proj);

            let cond_val = process_expr(cond, lexer, stringtab, env, builder,
                                        func, ssa, latch,
                                        &GoalType::KnownType(Type::Primitive(Primitive::Bool)));

            env.open_scope();
            loops.push((latch, exit));

            let body_end = process_stmt(*body, lexer, stringtab, env, builder,
                                        func, ssa, body_node, loops, return_type,
                                        inout_types, inout_vars);

            env.close_scope();
            loops.pop();

            let ((condition, _), body_term) = append_errors2(cond_val, body_end)?;

            if_node.build_if(latch, condition);
            let _ = builder.add_node(if_node);

            match body_term {
                None => {},
                Some(block) => { ssa.add_pred(latch, block); },
            }

            ssa.seal_block(latch, builder);
            ssa.seal_block(exit, builder);

            Ok(Some(exit))
        },
        lang_y::Stmt::ReturnStmt { span, expr } => {
            let val =
                if expr.is_none() && return_type.is_void() {
                    let unit_const = builder.create_constant_prod(vec![].into());
                    let mut unit_node = builder.allocate_node(func);
                    let unit_val = unit_node.id();
                    unit_node.build_constant(unit_const);
                    let _ = builder.add_node(unit_node);
                    unit_val
                } else if expr.is_none() {
                    return Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("Expected return of type {} found no return value",
                                        return_type.to_string(stringtab)))));
                } else {
                    process_expr(expr.unwrap(), lexer, stringtab, env,
                                 builder, func, ssa, pred,
                                 &GoalType::KnownType(return_type.clone()))?.0
                };

            build_return(return_type, val, pred, inout_types, inout_vars,
                         builder, func, ssa);

            Ok(None)
        },
        lang_y::Stmt::BreakStmt { span } => {
            if loops.len() <= 0 {
                return Err(singleton_error(
                        ErrorMessage::SemanticError(
                            span_to_loc(span, lexer),
                            "Break not contained within loop".to_string())));
            }

            let last_loop = loops.len() - 1;
            let (_latch, exit) = loops[last_loop];
            ssa.add_pred(exit, pred); // The block that contains this break now leads to the exit
            
            Ok(None) // Code after the break is unreachable
        },
        lang_y::Stmt::ContinueStmt { span } => {
            if loops.len() <= 0 {
                return Err(singleton_error(
                        ErrorMessage::SemanticError(
                            span_to_loc(span, lexer),
                            "Continue not contained within loop".to_string())));
            }

            let last_loop = loops.len() - 1;
            let (latch, _exit) = loops[last_loop];
            ssa.add_pred(latch, pred); // The block that contains this break now leads to the latch
            
            Ok(None) // Code after the continue is unreachable
        },
        lang_y::Stmt::BlockStmt { span: _, body } => {
            env.open_scope();

            let mut next = Some(pred);
            let mut errors = LinkedList::new();
            for stmt in body {
                if next.is_none() {
                    return Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(stmt.span(), lexer),
                                "Unreachable statement".to_string())));
                }

                match process_stmt(stmt, lexer, stringtab, env, builder, func,
                                   ssa, next.expect("From above"), loops,
                                   return_type, inout_types, inout_vars) {
                    Err(mut errs) => { errors.append(&mut errs); },
                    Ok(block) => { next = block; },
                }
            }

            env.close_scope();

            if !errors.is_empty() {
                Err(errors)
            } else {
                Ok(next)
            }
        },
        lang_y::Stmt::CallStmt { span, name: _, ty_args: _, args: _ } => {
            Err(singleton_error(
                    ErrorMessage::NotImplemented(
                        span_to_loc(span, lexer),
                        "function calls".to_string())))
        },
    }
}

fn process_expr<'a>(
    expr : lang_y::Expr, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
    stringtab : &mut StringTable, env : &mut Env<usize, Entity>,
    builder : &mut Builder<'a>, func : FunctionID, ssa : &mut SSA,
    block : NodeID, goal_type : &GoalType) -> Result<(NodeID, Type), ErrorMessages> {

    match expr {
        lang_y::Expr::Variable { span, name } => {
            if name.len() != 1 {
                return Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "packages".to_string())));
            }
            let nm = intern_package_name(&name, lexer, stringtab)[0];
            match env.lookup(&nm) {
                Some(Entity::Variable { variable, typ, is_const: _ }) => {
                    if goal_type.matches(typ) {
                        Ok((ssa.read_variable(*variable, block, builder), typ.clone()))
                    } else {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(span, lexer),
                                    goal_type.to_string(stringtab),
                                    typ.to_string(stringtab))))
                    }
                },
                // NOTE: Should we handle cases of this being a type/dyn const
                _ => {
                    Err(singleton_error(
                            ErrorMessage::UndefinedVariable(
                                span_to_loc(span, lexer),
                                lexer.span_str(span).to_string())))
                },
            }
        },
        
        lang_y::Expr::Field { span: _, lhs, rhs } => {
            let field_name = intern_id(&rhs, lexer, stringtab);
            let (res, typ) = process_expr(*lhs, lexer, stringtab, env, builder, func,
                                          ssa, block,
                                          &GoalType::StructType {
                                              field : field_name,
                                              field_type : Box::new(goal_type.clone()) })?;
            match typ {
                Type::Struct { name : _, id : _, mut fields, names } => {
                    let index = names.get(&field_name).unwrap();
                    let mut node_builder = builder.allocate_node(func);
                    let node = node_builder.id();
                    node_builder.build_read(res, vec![builder.create_field_index(*index)].into());
                    let _ = builder.add_node(node_builder);
                    Ok((node, fields.swap_remove(*index)))
                },
                _ => {
                    panic!("Internal Error: Expected struct type")
                },
            }
        },
        lang_y::Expr::NumField { span: _, lhs, rhs } => {
            let field_num = lexer.span_str(rhs)[1..].parse::<usize>()
                                                    .expect("From lexical analysis");

            let (res, typ) = process_expr(*lhs, lexer, stringtab, env, builder, func,
                                          ssa, block,
                                          &GoalType::TupleType {
                                              index : field_num,
                                              index_type : Box::new(goal_type.clone()) })?;

            match typ {
                Type::Tuple(mut fields) => {
                    let mut node_builder = builder.allocate_node(func);
                    let node = node_builder.id();
                    node_builder.build_read(res, vec![builder.create_field_index(field_num)].into());
                    let _ = builder.add_node(node_builder);
                    Ok((node, fields.swap_remove(field_num)))
                },
                _ => {
                    panic!("Internal Error: Expected tuple type")
                },
            }
        },
        lang_y::Expr::ArrIndex { span: _, lhs, index } => {
            let array = process_expr(*lhs, lexer, stringtab, env, builder, func,
                                     ssa, block,
                                     &GoalType::ArrayType {
                                         element_type : Box::new(goal_type.clone()),
                                         num_dims : index.len() });

            let mut indices = vec![];
            let mut errors = LinkedList::new();
            for idx in index {
                match process_expr(idx, lexer, stringtab, env, builder, func, ssa, block,
                                   &GoalType::KnownType(Type::Primitive(Primitive::USize))) {
                    Err(mut errs) => errors.append(&mut errs),
                    Ok((val, _)) => indices.push(val),
                }
            }

            match array {
                Err(mut errs) => {
                    errs.append(&mut errors);
                    Err(errs)
                },
                Ok((array_val, Type::Array(element_type, dims))) => {
                    let mut node_builder = builder.allocate_node(func);
                    let res = node_builder.id();
                    node_builder.build_read(array_val, vec![builder.create_position_index(indices.clone().into())].into());
                    let _ = builder.add_node(node_builder);

                    assert!(indices.len() == dims.len(), "Array has wrong number of dimensions");
                    Ok((res, *element_type))
                },
                _ => { panic!("Internal Error: Expected array type") },
            }
        },
        
        lang_y::Expr::Tuple { span, mut exprs } => {
            if exprs.len() == 1 {
                process_expr(exprs.pop().unwrap(), lexer, stringtab, env, builder, func, ssa,
                             block, goal_type)
            } else {
                let field_tys =
                    match goal_type {
                        GoalType::KnownType(Type::Tuple(fields)) => {
                            if fields.len() != exprs.len() {
                                return Err(singleton_error(
                                        ErrorMessage::TypeError(
                                            span_to_loc(span, lexer),
                                            goal_type.to_string(stringtab),
                                            format!("(_{})", ", _".repeat(exprs.len()-1)))));
                            }
                            fields.iter().map(|t| GoalType::KnownType(t.clone())).collect::<Vec<_>>()
                        },
                        GoalType::TupleType { index, index_type } => {
                            if *index >= exprs.len() {
                                return Err(singleton_error(
                                        ErrorMessage::TypeError(
                                            span_to_loc(span, lexer),
                                            goal_type.to_string(stringtab),
                                            format!("(_{})", ", _".repeat(exprs.len()-1)))));
                            }

                            let mut tys = vec![];
                            for i in 0..exprs.len() {
                                tys.push(
                                    if i == *index { *index_type.clone() }
                                    else { GoalType::AnyType });
                            }
                            tys
                        },
                        GoalType::AnyType => {
                            iter::repeat(GoalType::AnyType).take(exprs.len()).collect::<Vec<_>>()
                        },
                        _ => {
                            return Err(singleton_error(
                                    ErrorMessage::TypeError(
                                        span_to_loc(span, lexer),
                                        goal_type.to_string(stringtab),
                                        "tuple".to_string())));
                        },
                    };

                let mut vals = vec![];
                let mut typs = vec![];
                let mut errors = LinkedList::new();

                for (exp, typ) in exprs.into_iter().zip(field_tys) {
                    match process_expr(exp, lexer, stringtab, env, builder,
                                       func, ssa, block, &typ) {
                        Ok((val, ty)) => {
                            vals.push(val);
                            typs.push(ty);
                        },
                        Err(mut errs) => {
                            errors.append(&mut errs);
                        },
                    }
                }

                if !errors.is_empty() {
                    Err(errors)
                } else {
                    Ok((build_tuple(&typs, &vals, builder, func),
                        Type::Tuple(typs)))
                }
            }
        },
        lang_y::Expr::Struct { span : _, name : _, ty_args : _, exprs : _ } => { todo!() },

        lang_y::Expr::BoolLit { span, value } => {
            if !goal_type.is_boolean() {
                Err(singleton_error(
                        ErrorMessage::TypeError(
                            span_to_loc(span, lexer),
                            goal_type.to_string(stringtab),
                            "bool".to_string())))
            } else {
                let mut node = builder.allocate_node(func);
                let const_val = builder.create_constant_bool(value);
                let res = node.id();
                node.build_constant(const_val);
                let _ = builder.add_node(node);
                Ok((res, goal_type.get_boolean().clone()))
            }
        },
        lang_y::Expr::IntLit { span, base } => {
            if !goal_type.is_integer() {
                Err(singleton_error(
                        ErrorMessage::TypeError(
                            span_to_loc(span, lexer),
                            goal_type.to_string(stringtab),
                            "integer".to_string())))
            } else {
                let res = u64::from_str_radix(lexer.span_str(span), base.base());
                assert!(res.is_ok(), "Internal Error: Int literal is not an integer");
                Ok((build_int_const(goal_type.get_integer(), res.unwrap(), builder, func),
                    goal_type.get_integer().clone()))
            }
        },
        lang_y::Expr::FloatLit { span } => {
            if !goal_type.is_float() {
                Err(singleton_error(
                        ErrorMessage::TypeError(
                            span_to_loc(span, lexer),
                            goal_type.to_string(stringtab),
                            "floating point".to_string())))
            } else {
                Ok((build_float_const(goal_type.get_float(), lexer.span_str(span), builder, func),
                    goal_type.get_float(). clone()))
            }
        },

        lang_y::Expr::UnaryExpr { span, op, expr } => {
            // All unary operators preserve the type of their argument
            let (inner_goal, ir_op) =
                match op {
                    UnaryOp::Negation => {
                        if !goal_type.is_numeric() {
                            return Err(singleton_error(
                                    ErrorMessage::TypeError(
                                        span_to_loc(span, lexer),
                                        goal_type.to_string(stringtab),
                                        "numeric".to_string())))
                        }

                        (goal_type.as_numeric(), UnaryOperator::Neg)
                    },
                    UnaryOp::BitwiseNot => {
                        if !goal_type.is_integer() {
                            return Err(singleton_error(
                                    ErrorMessage::TypeError(
                                        span_to_loc(span, lexer),
                                        goal_type.to_string(stringtab),
                                        "integer".to_string())));
                        }

                        (goal_type.as_integer(), UnaryOperator::Not)
                    },
                    UnaryOp::LogicalNot => {
                        if !goal_type.is_boolean() {
                            return Err(singleton_error(
                                    ErrorMessage::TypeError(
                                        span_to_loc(span, lexer),
                                        goal_type.to_string(stringtab),
                                        "bool".to_string())));
                        }

                        return Err(singleton_error(
                                ErrorMessage::NotImplemented(
                                    span_to_loc(span, lexer),
                                    "! operator".to_string())));
                    },
                };

            let (ex, typ) = process_expr(*expr, lexer, stringtab, env, builder,
                                         func, ssa, block, inner_goal)?;
            let mut node = builder.allocate_node(func);
            let res = node.id();

            node.build_unary(ex, ir_op);
            let _ = builder.add_node(node);

            Ok((res, typ))
        },
        lang_y::Expr::BinaryExpr { span: _, op: _, lhs: _, rhs: _ } => { todo!() },
        
        lang_y::Expr::CastExpr { span: _, expr: _, typ: _ } => { todo!() },
        lang_y::Expr::CondExpr { span: _, cond: _, thn: _, els: _ } => { todo!() },
        lang_y::Expr::CallExpr { span, name: _, ty_args: _, args: _ } => {
            Err(singleton_error(
                    ErrorMessage::NotImplemented(
                        span_to_loc(span, lexer),
                        "function calls".to_string())))
        },
    }
}

fn process_lexpr<'a>(
    expr : lang_y::LExpr, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
    stringtab : &mut StringTable, env : &mut Env<usize, Entity>,
    builder : &mut Builder<'a>, func : FunctionID, ssa : &mut SSA,
    block : NodeID) -> Result<(usize, Type, Vec<Index>), ErrorMessages> {

    match expr {
        lang_y::LExpr::VariableLExpr { span } => {
            let nm = intern_id(&span, lexer, stringtab);
            match env.lookup(&nm) {
                Some(Entity::Variable { variable, typ, is_const }) => {
                    if *is_const {
                        return Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(span, lexer),
                                    format!("Variable {} is const, cannot assign to it",
                                            lexer.span_str(span)))));
                    }

                    Ok((*variable, typ.clone(), vec![]))
                },
                // NOTE: Should we handle cases of this being a type/dyn const
                _ => {
                    Err(singleton_error(
                            ErrorMessage::UndefinedVariable(
                                span_to_loc(span, lexer),
                                lexer.span_str(span).to_string())))
                },
            }
        },
        lang_y::LExpr::FieldLExpr { span, lhs, rhs } => {
            let (var, typ, mut idx) = process_lexpr(*lhs, lexer, stringtab, env,
                                                    builder, func, ssa, block)?;
            let field_str = lexer.span_str(rhs).to_string();
            let field_nm = stringtab.lookup_string(field_str.clone());

            match typ {
                Type::Struct { name: _, id: _, ref fields, ref names } => {
                    match names.get(&field_nm) {
                        Some(index) => {
                            idx.push(builder.create_field_index(*index));
                            Ok((var, fields[*index].clone(), idx))
                        },
                        None => {
                        Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(span, lexer),
                                    format!("Operation .{} does not apply to type {}, does not contain field",
                                            field_str, typ.to_string(stringtab)))))
                        },
                    }
                },
                _ => {
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("Operation .{} does not apply to type {}, expected a struct",
                                        field_str, typ.to_string(stringtab)))))
                },
            }
        },
        lang_y::LExpr::NumFieldLExpr { span, lhs, rhs } => {
            let (var, typ, mut idx) = process_lexpr(*lhs, lexer, stringtab, env,
                                                    builder, func, ssa, block)?;
            let num = lexer.span_str(rhs)[1..].parse::<usize>()
                                              .expect("From lexical analysis");

            match typ {
                Type::Tuple(ref fields) => {
                    if num < fields.len() {
                        idx.push(builder.create_field_index(num));
                        Ok((var, fields[num].clone(), idx))
                    } else {
                        Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(span, lexer),
                                    format!("Operation .{} does not apply to type {}, too few fields",
                                            num, typ.to_string(stringtab)))))
                    }
                }
                _ => {
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("Operation .{} does not apply to type {}, expected a tuple",
                                        num, typ.to_string(stringtab)))))
                },
            }
        },
        lang_y::LExpr::IndexLExpr { span, lhs, index } => {
            let (var, typ, mut idx) = process_lexpr(*lhs, lexer, stringtab, env,
                                                    builder, func, ssa, block)?;
            
            let mut indices = vec![];
            let mut errors = LinkedList::new();
            for idx in index {
                match process_expr(idx, lexer, stringtab, env, builder,
                                   func, ssa, block,
                                   &GoalType::KnownType(Type::Primitive(Primitive::USize))) {
                    Ok((exp, _)) => indices.push(exp),
                    Err(mut errs) => errors.append(&mut errs),
                }
            }

            if !errors.is_empty() {
                return Err(errors);
            }

            match typ {
                Type::Array(element, dimensions) => {
                    if indices.len() < dimensions.len() {
                        // Too few dimensions are accessed
                        Err(singleton_error(
                                ErrorMessage::NotImplemented(
                                    span_to_loc(span, lexer),
                                    format!("fewer array indices that dimensions, value has {} dimensions but using {} indices",
                                            dimensions.len(), indices.len()))))
                    } else if indices.len() == dimensions.len() {
                        // All dimenensions are accessed, left with the element type
                        idx.push(builder.create_position_index(indices.into()));
                        Ok((var, *element, idx))
                    } else {
                        // Too many dimensions are accessed
                        Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(span, lexer),
                                    format!("Too many array indices, value has {} dimensions but using {} indices",
                                            dimensions.len(), indices.len()))))
                    }
                },
                _ => {
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("Array index does not apply to type {}",
                                        typ.to_string(stringtab)))))
                },
            }
        },
    }
}

fn build_dynamic_constant<'a>(c : &DynamicConstant, builder : &mut Builder<'a>) -> DynamicConstantID {
    match c {
        DynamicConstant::Constant(val) => builder.create_dynamic_constant_constant(*val),
        DynamicConstant::DynConst(_, num) => builder.create_dynamic_constant_parameter(*num),
    }
}

fn build_type<'a>(ty : &Type, builder : &mut Builder<'a>) -> TypeID {
    match ty {
        Type::Primitive(Primitive::Bool)  => builder.create_type_bool(),
        Type::Primitive(Primitive::I8)    => builder.create_type_i8(),
        Type::Primitive(Primitive::U8)    => builder.create_type_u8(),
        Type::Primitive(Primitive::I16)   => builder.create_type_i16(),
        Type::Primitive(Primitive::U16)   => builder.create_type_u16(),
        Type::Primitive(Primitive::I32)   => builder.create_type_i32(),
        Type::Primitive(Primitive::U32)   => builder.create_type_u32(),
        Type::Primitive(Primitive::I64)   => builder.create_type_i64(),
        Type::Primitive(Primitive::U64)   => builder.create_type_u64(),
        Type::Primitive(Primitive::USize) => builder.create_type_u64(),
        Type::Primitive(Primitive::F32)   => builder.create_type_f32(),
        Type::Primitive(Primitive::F64)   => builder.create_type_f64(),
        Type::Primitive(Primitive::Void)  => builder.create_type_prod(vec![].into()),

        Type::Tuple(fields)
        | Type::Struct { name : _, id : _, fields, names : _ } => {
            let mut built = vec![];
            for ty in fields {
                built.push(build_type(ty, builder));
            }
            builder.create_type_prod(built.into())
        },
        Type::Array(elem, dims) => {
            let elem_type = build_type(&*elem, builder);
            let mut dims_built = vec![];
            for dim in dims {
                dims_built.push(build_dynamic_constant(dim, builder));
            }
            builder.create_type_array(elem_type, dims_built.into())
        },
    }
}

fn build_tuple<'a>(types : &Vec<Type>, vals : &Vec<NodeID>,
                   builder : &mut Builder<'a>, func : FunctionID) -> NodeID {
    assert!(types.len() == vals.len(), "Types and values must be same length to construct a tuple");

    // Create constant
    let mut inits = vec![];
    for ty in types {
        inits.push(build_default(ty, builder));
    }

    let init_const = builder.create_constant_prod(inits.into());
    let mut init_val_builder = builder.allocate_node(func);
    let init_val = init_val_builder.id();

    init_val_builder.build_constant(init_const);
    let _ = builder.add_node(init_val_builder);

    // Write each field
    let mut cur_value = init_val;
    for (idx, val) in vals.iter().enumerate() {
        let mut write_builder = builder.allocate_node(func);
        let write_node = write_builder.id();

        let index = builder.create_field_index(idx);

        write_builder.build_write(cur_value, *val, vec![index].into());
        let _ = builder.add_node(write_builder);

        cur_value = write_node;
    }

    cur_value
}

fn build_default<'a>(ty : &Type, builder : &mut Builder<'a>) -> ConstantID {
    match ty {
        Type::Primitive(Primitive::Bool)  => builder.create_constant_bool(false),
        Type::Primitive(Primitive::I8)    => builder.create_constant_i8(0),
        Type::Primitive(Primitive::U8)    => builder.create_constant_u8(0),
        Type::Primitive(Primitive::I16)   => builder.create_constant_i16(0),
        Type::Primitive(Primitive::U16)   => builder.create_constant_u16(0),
        Type::Primitive(Primitive::I32)   => builder.create_constant_i32(0),
        Type::Primitive(Primitive::U32)   => builder.create_constant_u32(0),
        Type::Primitive(Primitive::I64)   => builder.create_constant_i64(0),
        Type::Primitive(Primitive::U64)   => builder.create_constant_u64(0),
        Type::Primitive(Primitive::USize) => builder.create_constant_u64(0),
        Type::Primitive(Primitive::F32)   => builder.create_constant_f32(0.0),
        Type::Primitive(Primitive::F64)   => builder.create_constant_f64(0.0),
        Type::Primitive(Primitive::Void)  => builder.create_constant_prod(vec![].into()),
        
        Type::Tuple(fields) 
        | Type::Struct { name : _, id : _, fields, .. } => {
            let mut defaults = vec![];
            for ty in fields {
                defaults.push(build_default(ty, builder));
            }
            builder.create_constant_prod(defaults.into())
        },

        Type::Array(_elem, _dims) => {
            todo!("Cannot build default value for arrays") // FIXME
        },
    }
}

fn build_int_const<'a>(ty : &Type, val : u64, builder : &mut Builder<'a>,
                       func : FunctionID) -> NodeID {
    let const_val =
        match ty {
            Type::Primitive(Primitive::I8)    => builder.create_constant_i8(val as i8),
            Type::Primitive(Primitive::U8)    => builder.create_constant_u8(val as u8),
            Type::Primitive(Primitive::I16)   => builder.create_constant_i16(val as i16),
            Type::Primitive(Primitive::U16)   => builder.create_constant_u16(val as u16),
            Type::Primitive(Primitive::I32)   => builder.create_constant_i32(val as i32),
            Type::Primitive(Primitive::U32)   => builder.create_constant_u32(val as u32),
            Type::Primitive(Primitive::I64)   => builder.create_constant_i64(val as i64),
            Type::Primitive(Primitive::U64)   => builder.create_constant_u64(val as u64),
            Type::Primitive(Primitive::USize) => builder.create_constant_u64(val as u64),
            _ => panic!("Internal Error: build_int_const should not be used on non-integer types"),
        };

    let mut node = builder.allocate_node(func);
    let res = node.id();

    node.build_constant(const_val);
    let _ = builder.add_node(node);
    res
}

fn build_float_const<'a>(ty : &Type, text : &str, builder : &mut Builder<'a>,
                         func : FunctionID) -> NodeID {
    let const_val =
        match ty {
            Type::Primitive(Primitive::F32) => {
                builder.create_constant_f32(text.parse::<f32>().unwrap())
            },
            Type::Primitive(Primitive::F64) => {
                builder.create_constant_f64(text.parse::<f64>().unwrap())
            },
            _ => panic!("Internal Error: build_float_const should not be used on non-floating point types"),
        };

    let mut node = builder.allocate_node(func);
    let res = node.id();

    node.build_constant(const_val);
    let _ = builder.add_node(node);
    res
}

fn build_return<'a>(return_type : &Type, return_value : NodeID, block : NodeID,
                    inout_types : &Vec<Type>, inout_vars : &Vec<usize>,
                    builder : &mut Builder<'a>, func : FunctionID, ssa : &mut SSA) {
    let mut inout_vals = vec![];
    for var in inout_vars {
        let value = ssa.read_variable(*var, block, builder);
        inout_vals.push(value);
    }

    let inout_values = build_tuple(inout_types, &inout_vals, builder, func);
    let return_value = build_tuple(&vec![return_type.clone(), Type::Tuple(inout_types.clone())],
                                   &vec![return_value, inout_values],
                                   builder, func);

    let mut return_builder = builder.allocate_node(func);
    return_builder.build_return(block, return_value);
    let _ = builder.add_node(return_builder);
}

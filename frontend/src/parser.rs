extern crate hercules_ir;

use std::collections::{HashMap, LinkedList};
use std::fs::File;
use std::io::Read;

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
pub enum DynamicConstant {
    Constant(usize), // constant value
    DynConst(usize, usize), // name and dynamic constant number
}

impl DynamicConstant {
    fn to_string(&self, stringtab : &StringTable) -> String {
        match self {
            DynamicConstant::Constant(val) => val.to_string(),
            DynamicConstant::DynConst(nm, _) => stringtab.lookupId(*nm).unwrap(),
        }
    }
}

#[derive(Clone, Eq)]
pub enum Type {
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
                stringtab.lookupId(*name).unwrap()
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

    fn is_boolean(&self) -> bool {
        match self {
            Type::Primitive(Primitive::Bool) => true,
            _ => false,
        }
    }

    fn is_bitwise(&self) -> bool {
        match self {
              Type::Primitive(Primitive::I8)  | Type::Primitive(Primitive::U8)
            | Type::Primitive(Primitive::I16) | Type::Primitive(Primitive::U16)
            | Type::Primitive(Primitive::I32) | Type::Primitive(Primitive::U32)
            | Type::Primitive(Primitive::I64) | Type::Primitive(Primitive::U64)
              => true,
            _ => false,
        }
    }
}

pub enum Entity {
    // A variable has a binding number to distinguish shadowing
    Variable { value : NodeID, binding : usize, typ : Type, is_const : bool },
    Type     { value : Type },
    DynConst { value : usize }, // dynamic constant number
}

// Map strings to unique identifiers and counts uids
pub struct StringTable {
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

    fn lookupString(&mut self, s : String) -> usize {
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

    fn lookupId(&self, n : usize) -> Option<String> {
        self.index_to_string.get(&n).cloned()
    }
}

// Start line and column, end line and column
pub type Location = ((usize, usize), (usize, usize));
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

fn span_to_loc(span : Span, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>)
    -> Location {
    lexer.line_col(span)
}

pub fn parse_program(src_file : String) -> Result<Module, ErrorMessages> {
    let mut stringtab = StringTable::new();
    let mut file = File::open(src_file.clone()).expect("PANIC: Unable to open input file.");
    parse_file(&mut file, &mut stringtab)
}

fn intern_id(n : &Span, lex : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
             stringtab : &mut StringTable) -> usize {
    stringtab.lookupString(lex.span_str(*n).to_string())
}

fn intern_packageName(
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
            Some(Err(err)) => Err(singleton_error(
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

    env.openScope();

    for top in prg {
        match top {
            lang_y::Top::Import { span, name } => {
                return Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "imports".to_string())));
            },
            lang_y::Top::TypeDecl { span, public, name, ty_vars, body } => {
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
            lang_y::Top::ConstDecl { span, public, name, ty, body } => {
                return Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "constants".to_string())));
            },
            lang_y::Top::FuncDecl { span, public, attr, name, ty_vars, args, ty, body } => {
                // TODO: Handle public, attributes
                env.openScope();

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

                            let nm = intern_packageName(&name, lexer, stringtab)[0];
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
                let mut inout_names = vec![];
                for arg_idx in inout_args {
                    inout_types.push(arg_types[arg_idx].1.clone());
                    inout_names.push(arg_types[arg_idx].0);
                }
                
                let pure_return_type
                    = Type::Tuple(vec![return_type,
                                  Type::Tuple(inout_types)]);
                let return_type_built =
                    build_type(&pure_return_type, &mut builder);

                // TODO: Gracefully handle a possible failure here
                let (func, entry) 
                    = builder.create_function(lexer.span_str(name),
                                              param_types,
                                              return_type_built,
                                              num_dyn_const).unwrap();
                
                let mut idx = 0;
                for (nm, ty) in arg_types {
                    let mut node_builder = builder.allocate_node(func);
                    let node = node_builder.id();
                    node_builder.build_parameter(idx);
                    let _ = builder.add_node(node_builder);

                    let binding = env.uniq();
                    env.insert(nm,
                               Entity::Variable {
                                   value : node,
                                   binding : binding,
                                   typ : ty,
                                   is_const : false });

                    idx += 1;
                }

                // Finally, we have a properly built environment and we can
                // start processing the body

                env.closeScope();
            },
            lang_y::Top::ModDecl { span, public, name, body } => {
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
        lang_y::TyDef::TypeAlias { span, body } => {
            process_type(body, lexer, stringtab, env)
        },
        lang_y::TyDef::Struct { span, public, fields } => {
            // TODO: handle public correctly (and field public)
            
            let mut field_list = vec![];
            let mut field_map = HashMap::new();
            let mut errors = LinkedList::new();

            for ObjField { span, public, name, typ } in fields {
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
        lang_y::TyDef::Union { span, public, fields } => {
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
        lang_y::Type::PrimType { span, typ } => {
            Ok(Type::Primitive(typ))
        },
        lang_y::Type::TupleType { span, tys } => {
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
                Ok(Type::Tuple(fields))
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
                let id = intern_packageName(&name, lexer, stringtab);
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
                                    stringtab.lookupId(nm).unwrap()))),
                }
            }
        },
        lang_y::Type::ArrayType { span, elem, dims } => {
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
                        Ok(Type::Array(Box::new(element_type), dimensions))
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
                let id = intern_packageName(&name, lexer, stringtab);
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
                                    stringtab.lookupId(nm).unwrap()))),
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

fn process_stmt<'a>(
    stmt : lang_y::Stmt, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
    stringtab : &mut StringTable, env : &mut Env<usize, Entity>,
    builder : &mut Builder<'a>, pred : NodeID, return_type : Type,
    inout_returns : Vec<usize>, /**/) -> Result<NodeID, ErrorMessages> {

    todo!()
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
/*

    let mut bindNum : usize = 0;
    let mut env : Env<usize, Entity> = Env::new();
    env.openScope();

    match &prg[0] {
        lang_y::Top::FuncDecl { span, public, attr, name, ty_vars, args, ty, body } => {
            // TODO: Don't just ignore things like public/attr
            let funcName = intern_id(&name, lexer, stringtab);
            assert!(ty_vars.len() == 0,
                    "Type variables and dynamic constants not supported yet");
            
            let mut err = false;

            // Arguments: binding number and type for each
            let mut rArgs : Vec<(usize, IType)> = vec![];
            // In out arguments (position in the argument list)
            let mut inouts : Vec<usize> = vec![];

            for (isInout, VarBind{span, pattern, typ}) in args {
                match typ {
                    None => todo!("Function argument type inference not supported"),
                    Some(t) => {
                        match pattern {
                            lang_y::SPattern::Variable { span, name } => {
                                if name.len() != 1 {
                                    error_message(span, lexer,
                                                  format!("Argument name cannot contain package separators"));
                                    return None;
                                }

                                let argName = intern_id(&name[0], lexer, stringtab);
                                let binding = bindNum;
                                bindNum += 1;

                                match prepare_type(t) {
                                    None => err = true,
                                    Some((bType, iType)) => {
                                        env.insert(argName,
                                                   Entity::Variable { 
                                                       binding : binding,
                                                       iTy : iType.clone(),
                                                       bTy : bType,
                                                       is_const : false });
                                        rArgs.push((binding, iType));
                                        if isInout.is_some() {
                                            inouts.push(rArgs.len() - 1);
                                        }
                                    },
                                }
                            },
                            _ => todo!("Non-variable patterns in function arguments not implemented"),
                        }
                    }
                }
            }

            // Compute the output type of the generated function
            // Note that this includes any in/out arguments, which are just
            // lifted to additional return values
            let inoutType : IType = IType::Tuple(
                inouts.iter().map(|i| rArgs[*i].1.clone()).collect());
            let inoutBinds : Vec<(usize, IType)> =
                inouts.iter().map(|i| rArgs[*i].clone()).collect();

            let (rTy, rTyB) = match ty {
                None => (IType::Tuple(
                            vec![IType::Primitive(Primitive::Void),
                                 inoutType]),
                         (IType::Primitive(Primitive::Void),
                          BType::Primitive(Primitive::Void))),
                Some(t) => match prepare_type(t) {
                    None => { err = true;
                              (IType::Tuple(
                                  vec![IType::Primitive(Primitive::Void),
                                       inoutType]),
                               (IType::Primitive(Primitive::Void),
                                BType::Primitive(Primitive::Void))) },
                    Some((bTy, rTy)) => {
                        (IType::Tuple(vec![rTy.clone(), inoutType]),
                         (rTy, bTy))
                    },
                }
            };

            if err { None }
            else {
                match process_stmt(body, lexer, stringtab, &mut env,
                                   &mut bindNum, &inoutBinds, false, &rTyB) {
                    None => None,
                    Some(res) =>
                        Some(Prg { span : *span, name : funcName,
                                   args : rArgs, ty   : rTy,
                                   body : res }),
                }
            }
        },
        _ => {
            todo!("Currently, only programs with a single function are supported for compilation");
        }
    }
*/

/*
fn prepare_type(typ : &lang_y::Type) -> Option<(BType, IType)> {
    match typ {
        lang_y::Type::PrimType { span, typ } => {
            Some((BType::Primitive(*typ), IType::Primitive(*typ)))
        },
        lang_y::Type::TupleType { span, tys } => {
            if tys.len() == 0 {
                Some((BType::Primitive(Primitive::Void),
                      IType::Primitive(Primitive::Void)))
            } else {
                let mut bTys = vec![];
                let mut iTys = vec![];
                for t in tys {
                    match prepare_type(t) {
                        None => return None,
                        Some((b, i)) => {
                            bTys.push(b);
                            iTys.push(i);
                        },
                    }
                }
                Some((BType::Tuple(bTys), IType::Tuple(iTys)))
            }
        },
        _ => { todo!("Type not implemented yet"); }
    }
}

fn process_stmt(
    stmt : &lang_y::Stmt, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
    stringtab : &mut InternalStringTable, env : &mut Env<usize, Entity>,
    bindings : &mut usize, extra_returns : &Vec<(usize, IType)>,
    in_loop : bool, return_type : &(IType, BType))
    -> Option<Vec<Stmt>> {
    match stmt {
        lang_y::Stmt::LetStmt { span, var : VarBind{span : vSpan, pattern, typ}, init } => {
            match typ {
                None => todo!("Variable type inference not supported yet"),
                Some(ty) => {
                    match pattern {
                        lang_y::SPattern::Variable { span : varSpan, name } => {
                            if name.len() != 1 {
                                error_message(varSpan, lexer,
                                              format!("Variable names cannot contain package separators"));
                                return None;
                                            
                            }

                            let argName = intern_id(&name[0], lexer, stringtab);
                            let binding = *bindings;
                            *bindings += 1;

                            match prepare_type(ty) {
                                None => None,
                                Some((bType, iType)) => {
                                    let res =
                                        match init {
                                            None => {
                                                let init_val = default_value(&iType);
                                                Some(vec![Stmt::AssignStmt {
                                                            var : binding,
                                                            typ : iType.clone(),
                                                            val : init_val }])
                                            },
                                            Some(exp) => {
                                                match process_expr(exp, &bType, lexer, stringtab, env) {
                                                    None => None,
                                                    Some(e) => {
                                                        Some(vec![Stmt::AssignStmt {
                                                                    var : binding,
                                                                    typ : iType.clone(),
                                                                    val : e }])
                                                    },
                                                }
                                            },
                                        };

                                    env.insert(argName,
                                               Entity::Variable {
                                                   binding : binding,
                                                   iTy : iType,
                                                   bTy : bType,
                                                   is_const : false });
                                    res
                                },
                            }
                        },
                        _ => todo!("Non-variable patterns in let binding not implemented"),
                    }
                }
            }
        },
        lang_y::Stmt::ConstStmt { span, var, init } => {
            todo!("Constants not implemented yet")
        },
        lang_y::Stmt::AssignStmt { span, lhs, assign, assign_span, rhs } => {
            match process_lexpr(lhs, lexer, stringtab, env) {
                Some(((bind, vType), accs, (iType, bType))) => {
                    match process_expr(rhs, &bType, lexer, stringtab, env) {
                        None => None,
                        Some(e) => {
                            let mut err = false;
                            let mut op  = None;

                            match assign {
                                lang_y::AssignOp::None => {},
                                lang_y::AssignOp::Add => {
                                    if !bType.is_numeric() {
                                        error_message(assign_span, lexer,
                                                      format!("Value of type {bType} does not support addition"));
                                        err = true;
                                    } else {
                                        op = Some(BinaryOp::Add);
                                    }
                                },
                                lang_y::AssignOp::Sub => {
                                    if !bType.is_numeric() {
                                        error_message(assign_span, lexer,
                                                      format!("Value of type {bType} does not support subtraction"));
                                        err = true;
                                    } else {
                                        op = Some(BinaryOp::Sub);
                                    }
                                },
                                lang_y::AssignOp::Mul => {
                                    if !bType.is_numeric() {
                                        error_message(assign_span, lexer,
                                                      format!("Value of type {bType} does not support multiplication"));
                                        err = true;
                                    } else {
                                        op = Some(BinaryOp::Mul);
                                    }
                                },
                                lang_y::AssignOp::Div => {
                                    if !bType.is_numeric() {
                                        error_message(assign_span, lexer,
                                                      format!("Value of type {bType} does not support division"));
                                        err = true;
                                    } else {
                                        op = Some(BinaryOp::Div);
                                    }
                                },
                                lang_y::AssignOp::Mod => {
                                    if !bType.is_integer() {
                                        error_message(assign_span, lexer,
                                                      format!("Value of type {bType} does not support modulo"));
                                        err = true;
                                    } else {
                                        op = Some(BinaryOp::Mod);
                                    }
                                },
                                lang_y::AssignOp::BitAnd => {
                                    if !bType.is_bitwise() {
                                        error_message(assign_span, lexer,
                                                      format!("Value of type {bType} does not support bitwise-and"));
                                        err = true;
                                    } else {
                                        op = Some(BinaryOp::BitAnd);
                                    }
                                },
                                lang_y::AssignOp::BitOr => {
                                    if !bType.is_bitwise() {
                                        error_message(assign_span, lexer,
                                                      format!("Value of type {bType} does not support bitwise-or"));
                                        err = true;
                                    } else {
                                        op = Some(BinaryOp::BitOr);
                                    }
                                },
                                lang_y::AssignOp::Xor => {
                                    if !bType.is_bitwise() {
                                        error_message(assign_span, lexer,
                                                      format!("Value of type {bType} does not support bitwise-xor"));
                                        err = true;
                                    } else {
                                        op = Some(BinaryOp::Xor);
                                    }
                                },
                                lang_y::AssignOp::LogAnd => {
                                    if !bType.is_boolean() {
                                        error_message(assign_span, lexer,
                                                      format!("Value of type {bType} does not support logical-and"));
                                        err = true;
                                    } else {
                                        op = Some(BinaryOp::LogAnd);
                                    }
                                },
                                lang_y::AssignOp::LogOr => {
                                    if !bType.is_boolean() {
                                        error_message(assign_span, lexer,
                                                      format!("Value of type {bType} does not support logical-or"));
                                        err = true;
                                    } else {
                                        op = Some(BinaryOp::LogOr);
                                    }
                                },
                                lang_y::AssignOp::LShift => {
                                    if !bType.is_bitwise() {
                                        error_message(assign_span, lexer,
                                                      format!("Value of type {bType} does not support left bit shifts"));
                                        err = true;
                                    } else {
                                        op = Some(BinaryOp::LShift);
                                    }
                                },
                                lang_y::AssignOp::RShift => {
                                    if !bType.is_bitwise() {
                                        error_message(assign_span, lexer,
                                                      format!("Value of type {bType} does not support right bit shifts"));
                                        err = true;
                                    } else {
                                        op = Some(BinaryOp::RShift);
                                    }
                                },
                            }

                            if err {
                                None
                            } else {
                                Some(vec![
                                     Stmt::AssignStmt {
                                         var : bind,
                                         typ : vType.clone(),
                                         val :
                                             match op {
                                                 None => {
                                                     Expr::Write {
                                                         var  : bind,
                                                         accs : accs,
                                                         val  : Box::new(e),
                                                         typ  : vType,
                                                     }
                                                 },
                                                 Some(op) => {
                                                     let var =
                                                         Expr::Variable { bind : bind, typ : vType.clone() };
                                                     let read =
                                                         Expr::Read {
                                                             lhs  : Box::new(var),
                                                             accs : accs.clone(),
                                                             typ  : iType.clone() };
                                                     let update =
                                                         Expr::BinOp {
                                                             lhs : Box::new(read),
                                                             op  : op,
                                                             rhs : Box::new(e),
                                                             typ : iType };

                                                     Expr::Write {
                                                         var  : bind,
                                                         accs : accs,
                                                         val  : Box::new(update),
                                                         typ  : vType,
                                                     }
                                                 },
                                             }
                                     }
                                ])
                            }
                        },
                    }
                },
                None => None,
            }
        },
        lang_y::Stmt::IfStmt { span, cond, thn, els } => {
            let mut condition
                = Expr::Const { val : PrimConstant::Bool(false),
                                typ : IType::Primitive(Primitive::Bool) };
            let mut then_branch = vec![];
            let mut else_branch = vec![];
            let mut err = false;

            match process_expr(cond, &BType::Primitive(Primitive::Bool),
                               lexer, stringtab, env) {
                None => err = true,
                Some(c) => {
                    condition = c;
                },
            }

            env.openScope();
            match process_stmt(thn, lexer, stringtab, env, bindings,
                               extra_returns, in_loop, return_type) {
                None => err = true,
                Some(s) => then_branch = s,
            }
            env.closeScope();

            match els {
                None => {},
                Some(els) => {
                    env.openScope();
                    match process_stmt(els, lexer, stringtab, env, bindings,
                                       extra_returns, in_loop, return_type) {
                        None => err = true,
                        Some(s) => else_branch = s,
                    }
                    env.closeScope();
                },
            }

            if err { None }
            else { Some(vec![Stmt::IfStmt{ cond : condition, thn : then_branch,
                                      els  : else_branch }]) }
        },
        lang_y::Stmt::MatchStmt { span, expr, body } => {
            todo!("Match not implemented yet")
        },
        lang_y::Stmt::ForStmt { span, var : VarBind{span : vSpan, pattern, typ}, init, bound, step, body } => {
            match typ {
                None => todo!("Variable type inference not supported yet"),
                Some(ty) => {
                    match pattern {
                        lang_y::SPattern::Variable { span : varSpan, name } => {
                            if name.len() != 1 {
                                error_message(varSpan, lexer,
                                               format!("Variable names cannot contain package separators"));
                                None
                            } else {
                                let varName = intern_id(&name[0], lexer, stringtab);
                                let binding = *bindings;
                                *bindings += 1;

                                match prepare_type(ty) {
                                    None => None,
                                    Some((bType, iType)) => {
                                        let mut err = false;

                                        if !is_integer(bType) {
                                            error_message(vSpan, lexer,
                                                          format!("For loop variables must have integer type"));
                                            err = true;
                                        }

                                        let mut step_exp  = None;

                                        let init_exp = process_expr(init,  bType, lexer, stringtab, env);
                                        let stop_exp = process_expr(bound, bType, lexer, stringtab, env);
                                        let step_exp =
                                            match step {
                                                None => Some(const_one(bType)),
                                                Some(exp) =>
                                                    process_expr(exp, bType, lexer, stringtab, env)
                                            };
                                        
                                        match (init_exp, stop_exp, step_exp) {
                                            Some(init), Some(stop), Some(step) => {
                                                env.openScope();
                                                env.insert(varName,
                                                           Entity::Variable {
                                                               binding : binding,
                                                               iTy : iType.clone(),
                                                               bTy : bType.clone(),
                                                               is_const : false });

                                                let res =
                                                    vec![
                                                    // var = init
                                                    // vstop = stop
                                                    // vstep = step
                                                    // while var < vstop
                                                    //   ... body ...
                                                    //   ???
                                                env.closeScope();

                                                todo!()
                                            },
                                            (_, _, _) => {
                                                None
                                            },
                                        }
                                    },
                                }
                            }
                        },
                        _ => {
                            error_message(vSpan, lexer,
                                          format!("For loop variables must be single integer values"));
                            None
                        },
                    }
                }
            }
        },
        lang_y::Stmt::WhileStmt { span, cond, body } => {
            match process_expr(cond, &BType::Primitive(Primitive::Bool),
                               lexer, stringtab, env) {
                None => None,
                Some(e) => {
                    env.openScope();
                    let res =
                        match process_stmt(body, lexer, stringtab, env, bindings,
                                           extra_returns, true, return_type) {
                            None => None,
                            Some(s) => {
                                Some(vec![Stmt::WhileLoop { cond : e,
                                                            body : s }])
                            },
                        };
                    env.closeScope();

                    res
                },
            }
        },
        lang_y::Stmt::ReturnStmt { span, expr } => {
            match process_expr(expr, &return_type.1, lexer, stringtab, env) {
                None => None,
                Some(e) => {
                    let mut eretExps : Vec<Expr> = extra_returns.iter()
                        .map(|v| Expr::Variable { bind : v.0, typ : v.1.clone() })
                        .collect();
                    let mut eretTys  : Vec<IType> = extra_returns.iter()
                        .map(|v| v.1.clone())
                        .collect();
                    let eretTy  = IType::Tuple(eretTys);
                    let eretExp = Expr::Tuple { vals : eretExps, typ : eretTy.clone() };

                    Some(vec![Stmt::ReturnStmt {
                            val : Expr::Tuple {
                                vals : vec![e, eretExp],
                                typ  : IType::Tuple(vec![return_type.0.clone(),
                                                         eretTy]),
                            }}])
                },
            }
        },
        lang_y::Stmt::BreakStmt { span } => {
            if !in_loop {
                error_message(span, lexer,
                              format!("Break not contained in a loop"));
                None
            } else {
                Some(vec![Stmt::BreakStmt{}])
            }
        },
        lang_y::Stmt::ContinueStmt { span } => {
            if !in_loop {
                error_message(span, lexer,
                              format!("Continue not contained in a loop"));
                None
            } else {
                Some(vec![Stmt::Continue{}])
            }
        },
        lang_y::Stmt::BlockStmt { span, body } => {
            let mut res = vec![];
            let mut err = false;
            
            env.openScope();
            for s in body {
                match process_stmt(stmt, lexer, stringtab, env, bindings,
                                   extra_returns, in_loop, return_type) {
                    None    => { err = true; },
                    Some(mut s) => { res.append(&mut s); },
                }
            }
            env.closeScope();

            if err {
                None
            } else {
                Some(res)
            }
        },
        lang_y::Stmt::CallStmt { span, name, ty_args, args } => {
            todo!("Calls not implemented yet")
        },
    }
}

fn process_expr(
    exp : &lang_y::Expr, typ : &BType,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
    stringtab : &mut InternalStringTable, env : &mut Env<usize, Entity>)
    -> Option<Expr> {
 
    todo!()
}

// Returns ((variable, variable's type), access pattern, (access type as i, b))
fn process_lexpr(
    exp : &lang_y::LExpr, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
    stringtab : &mut InternalStringTable, env : &mut Env<usize, Entity>)
    -> Option<((usize, IType), Vec<Access>, (IType, BType))> {
    
    match exp {
        lang_y::LExpr::VariableLExpr { span } => {
            let name = intern_id(span, lexer, stringtab);
            match env.lookup(&name) {
                None => {
                    error_message(span, lexer, format!("Unbound variable"));
                    None
                },
                Some(Entity::Variable { binding, iTy, bTy, is_const }) => {
                    Some(((*binding, iTy.clone()),
                         vec![],
                         (iTy.clone(), bTy.clone())))
                },
            }
        },
        lang_y::LExpr::FieldLExpr { span, lhs, rhs } => {
            todo!("Struct access not implemented")
        },
        lang_y::LExpr::NumFieldLExpr { span, lhs, rhs } => {
            match process_lexpr(lhs, lexer, stringtab, env) {
                None => None,
                Some(((binding, vTyp), mut accs, (iTyp, bTyp))) => {
                    // Note: Unwrapping and indexing are safe since the lexer guarantees that the
                    // string is indeed a number and has form ".###"; technically maybe there's an
                    // issue if the number was too large but I think that's an edge case
                    let num = lexer.span_str(*rhs)[1..].parse::<usize>().unwrap();
                    accs.push(Access::Tuple(num));
                    match (iTyp, bTyp) {
                        (IType::Tuple(itys), BType::Tuple(btys)) => {
                            if num >= itys.len() || num >= btys.len() {
                                error_message(span, lexer,
                                              format!("Cannot access specified field, does not exist"));
                                None
                            } else {
                                Some(((binding, vTyp), accs, (itys[num].clone(), btys[num].clone())))
                            }
                        },
                        (IType::Primitive(_), BType::Primitive(_)) => {
                            error_message(span, lexer,
                                          format!("Dot operator does not apply to primitive types"));
                            None
                        },
                        (_, _) => {
                            eprintln!("INTERNAL ERROR: Internalized and Bound types do not match");
                            None
                        },
                    }
                },
            }
        },
        lang_y::LExpr::IndexLExpr { span, lhs, index } => {
            todo!("Arrow access not implemented")
        }
    }
}

fn default_value(ty : &IType) -> Expr {
    match ty {
        IType::Primitive(prim) => {
            match prim {
                lang_y::Primitive::Bool =>
                    Expr::Const{ val : PrimConstant::Bool(false),
                                 typ : IType::Primitive(Primitive::Bool) },
                lang_y::Primitive::I8 =>
                    Expr::Const{ val : PrimConstant::I8(0),
                                 typ : IType::Primitive(Primitive::I8) },
                lang_y::Primitive::U8 =>
                    Expr::Const{ val : PrimConstant::U8(0),
                                 typ : IType::Primitive(Primitive::U8) },
                lang_y::Primitive::I16 =>
                    Expr::Const{ val : PrimConstant::I16(0),
                                 typ : IType::Primitive(Primitive::I16) },
                lang_y::Primitive::U16 =>
                    Expr::Const{ val : PrimConstant::U16(0),
                                 typ : IType::Primitive(Primitive::U16) },
                lang_y::Primitive::I32 =>
                    Expr::Const{ val : PrimConstant::I32(0),
                                 typ : IType::Primitive(Primitive::I32) },
                lang_y::Primitive::U32 =>
                    Expr::Const{ val : PrimConstant::U32(0),
                                 typ : IType::Primitive(Primitive::U32) },
                lang_y::Primitive::I64 =>
                    Expr::Const{ val : PrimConstant::I64(0),
                                 typ : IType::Primitive(Primitive::I64) },
                lang_y::Primitive::U64 =>
                    Expr::Const{ val : PrimConstant::U64(0),
                                 typ : IType::Primitive(Primitive::U64) },
                lang_y::Primitive::USize =>
                    Expr::Const{ val : PrimConstant::USize(0),
                                 typ : IType::Primitive(Primitive::USize) },
                lang_y::Primitive::F32 =>
                    Expr::Const{ val : PrimConstant::F32(0.0),
                                 typ : IType::Primitive(Primitive::F32) },
                lang_y::Primitive::F64 =>
                    Expr::Const{ val : PrimConstant::F64(0.0),
                                 typ : IType::Primitive(Primitive::F64) },
                lang_y::Primitive::Void =>
                    Expr::Const{ val : PrimConstant::Unit(),
                                 typ : IType::Primitive(Primitive::Void) },
            }
        },
        IType::Tuple(tys) => {
            Expr::Tuple{ vals : tys.iter().map(default_value).collect(),
                         typ  : IType::Tuple(tys.to_vec()) }
        },
    }
}
*/

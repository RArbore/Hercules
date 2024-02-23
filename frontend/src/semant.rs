extern crate hercules_ir;

use std::collections::{HashMap, LinkedList};
use std::fs::File;
use std::io::Read;
use std::fmt;

use lrlex::{lrlex_mod, DefaultLexerTypes};
use lrpar::{lrpar_mod, NonStreamingLexer};
use cfgrammar::Span;

lrlex_mod!("lang.l");
lrpar_mod!("lang.y");

use lang_y::*;
use crate::env::Env;
use crate::types;
use crate::types::{DynamicConstant, Either, Type, TypeSolver};

// Definitions and data structures for semantic analysis

// Entities in the environment
enum Entity {
    // A variable has a variable number to distinguish shadowing
    Variable { variable : usize, typ : Type, is_const : bool },
    Type     { type_args : Vec<lang_y::Kind>, value : Type },
    DynConst { value : usize }, // dynamic constant number
    Constant { value : Constant },
    // For functions we track an index, its type parameters, its argument types and if they are
    // inout, and its return type
    Function { index : usize, type_args : Vec<lang_y::Kind>,
               args : Vec<(types::Type, bool)>, return_type : types::Type },
}

// Constant values
pub enum Literal {
    Bool(bool), Signed(i64), Unsigned(u64), Float(f64),
    Tuple(Vec<Constant>),
    Struct(Vec<Constant>), // Values of fields (in their order)
    Union(usize, Box<Constant>), // The tag and value
}
pub type Constant = (Literal, Type);

impl Literal {
    fn as_usize(&self) -> usize {
        match self {
            Literal::Unsigned(val) => *val as usize,
            _ => panic!("Expected a constant integer"),
        }
    }
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

    // Produce the UID for a string
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

    // Identify the string corresponding to a UID
    fn lookup_id(&self, n : usize) -> Option<String> {
        self.index_to_string.get(&n).cloned()
    }
}

// Convert spans into uids in the String Table
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

// A location in the program, used in error messages
struct Location {
    start_line : usize, start_column : usize,
    end_line   : usize, end_column   : usize,
}

impl Location {
    fn fake() -> Location {
        Location { start_line : 0, start_column : 0,
                   end_line   : 0, end_column   : 0 }
    }
}

// Conversion from span to internal locations
fn span_to_loc(span : Span, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>)
    -> Location {
    let ((start_line, start_column), (end_line, end_column)) = lexer.line_col(span);
    Location { start_line, start_column, end_line, end_column }
}

// Printing locations
impl fmt::Display for Location {
    fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}, {} -- {}, {}",
               self.start_line, self.start_column,
               self.end_line,   self.end_column)
    }
}

// Error Messages
pub enum ErrorMessage {
    NotImplemented(Location, String),
    IOError(String),
    SyntaxError(String),
    SemanticError(Location, String), // Other errors, with a location and description
    // Undefined variable at location, variable name
    UndefinedVariable(Location, String),
    // Kind error at location, expected, actual)
    KindError(Location, String, String),
    // Type error at location, expected type, actual type)
    TypeError(Location, String, String),
}

// Printing for error messages
impl fmt::Display for ErrorMessage {
    fn fmt(&self, f : &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorMessage::NotImplemented(loc, msg) => {
                write!(f, "Error ({}). Feature not implemented : {}", loc, msg)
            },
            ErrorMessage::IOError(msg) => {
                write!(f, "Error: {}", msg)
            },
            ErrorMessage::SyntaxError(msg) => {
                write!(f, "Syntax Error : {}", msg)
            },
            ErrorMessage::SemanticError(loc, msg) => {
                write!(f, "Error ({}). {}", loc, msg)
            },
            ErrorMessage::UndefinedVariable(loc, name) => {
                write!(f, "Error ({}). Undefined variable '{}'", loc, name)
            },
            ErrorMessage::KindError(loc, expected, actual) => {
                write!(f, "Error ({}). Expected {} but found {}", loc, expected, actual)
            },
            ErrorMessage::TypeError(loc, expected, actual) => {
                write!(f, "Error ({}). Type error, expected {} but found {}",
                    loc, expected, actual)
            },
        }
    }
}

pub type ErrorMessages = LinkedList<ErrorMessage>;

pub fn print_errors(msg : ErrorMessages) {
    for err in msg {
        eprintln!("{}", err);
    }
}

// Constructors and combiners for error messages
fn singleton_error(err : ErrorMessage) -> ErrorMessages {
    LinkedList::from([err])
}

fn append_errors2<A, B>(x : Result<A, ErrorMessages>, y : Result<B, ErrorMessages>)
    -> Result<(A, B), ErrorMessages> {
    match (x, y) {
        (Err(mut err_x), Err(mut err_y)) => { 
            err_x.append(&mut err_y);
            Err(err_x)
        },
        (Err(err_x), _) => Err(err_x),
        (_, Err(err_y)) => Err(err_y),
        (Ok(x), Ok(y)) => Ok((x, y)),
    }
}

fn append_errors3<A, B, C>(x : Result<A, ErrorMessages>, y : Result<B, ErrorMessages>,
                           z : Result<C, ErrorMessages>) -> Result<(A, B, C), ErrorMessages> {
    let xy = append_errors2(x, y);
    let xyz = append_errors2(xy, z);

    match xyz {
        Err(errs) => Err(errs),
        Ok(((x, y), z)) => Ok((x, y, z)),
    }
}

// Normalized AST forms after semantic analysis
// These include type information at all expression nodes, and remove names and locations
pub type Prg = (TypeSolver, Vec<Function>);

// The function stores information for code-generation. The type information therefore is not the
// type information that is needed for type checking code that uses this function.
// In particular, the return type accounts for the type of inout arguments
pub struct Function {
    pub name : String,
    pub num_dyn_consts : usize,
    pub num_type_args : usize,
    pub arguments : Vec<Type>,
    pub return_type : Type,
    pub body : Stmt,
}

// Normalized statements differ in a number of ways from the form from the parser:
// 1. Let/Const are replaced by assignments (since we've already typed checked)
// 2. Assignment operators of the form lhs X= rhs are replaced by assignments of the form lhs = lhs
//    X rhs, and assignments now always occur to variables, so modification of fields/values in an
//    array are replaced by appropriate expressions which write to those portions of the value
// 3. Match statements are eliminated except for those on union types, and only one match per level
// 4. Both for and while loops are combined into a single loop form
// 5. Call statements are transformed to call expressions, with a new statement form for
//    expressions
// Additional notes
// - Returns in this AST include the inout values
pub enum Stmt {
    AssignStmt   { var : usize, val : Expr },
    IfStmt       { cond : Expr, thn : Box<Stmt>, els : Option<Box<Stmt>> },
    MatchStmt    { expr : Expr, cases : Vec<usize>, body : Vec<Stmt> },
    LoopStmt     { cond : Expr, update : Option<Box<Stmt>>, body : Box<Stmt> },
    ReturnStmt   { expr : Expr },
    BreakStmt    {},
    ContinueStmt {},
    BlockStmt    { body : Vec<Stmt> },
    ExprStmt     { expr : Expr },
}

// Normalized expressions differ in a number of ways:
// 1. All expressions store their type
// 2. Field, index, and array access expressions are all replaced by a read node (like the IR) and
//    we add a write expression that is used to simplify the assignment operation
// 3. Structs are eliminated and replaced by tuples
// 4. Unions are now tagged by a number rather than name and are also now separated from function
//    calls
// 5. The unary and binary operations no longer contain boolean operations, instead those are
//    expressed using the conditional expression
// 6. Functions are now identified by number, and arguments to functions are now represented as
//    either an expression or a variable number for the inout arguments
// 7. There's an additional Zero which is used to construct the default of a type
pub enum Expr {
    Variable  { var : usize, typ : Type },
    Read      { index : Vec<Index>, val : Box<Expr>, typ : Type },
    Write     { index : Vec<Index>, val : Box<Expr>, rep : Box<Expr>, typ : Type },
    Tuple     { vals : Vec<Expr>, typ : Type },
    Union     { tag : usize, val : Box<Expr>, typ : Type },
    Constant  { val : Constant, typ : Type },
    Zero      { typ : Type },
    UnaryExp  { op : UnaryOp, expr : Box<Expr>, typ : Type },
    BinaryExp { op : BinaryOp, lhs : Box<Expr>, rhs : Box<Expr>, typ : Type },
    CastExpr  { expr : Box<Expr>, typ : Type },
    CondExpr  { cond : Box<Expr>, thn : Box<Expr>, els : Box<Expr>, typ : Type },
    CallExpr  { func : usize, ty_args : Vec<Type>, dyn_consts : Vec<DynamicConstant>,
                args : Vec<Either<Expr, usize>>, typ : Type },
}

pub enum Index { Field(usize), Variant(usize), Array(Box<Expr>) }

pub enum UnaryOp  { Negation, BitwiseNot }
pub enum BinaryOp { Add, Sub, Mul, Div, Mod,
                    BitAnd, BitOr, Xor,
                    Lt, Le, Gt, Ge, Eq, Neq,
                    LShift, RShift }

// Be able to access the type of an expression easily
impl Expr {
    pub fn get_type(&self) -> Type {
        match self {
            Expr::Variable { var : _, typ } | Expr::Read { index : _, val : _, typ }
            | Expr::Write { index : _, val : _, rep : _, typ }
            | Expr::Tuple { vals : _, typ } | Expr::Union { tag : _, val : _, typ }
            | Expr::Constant { val : _, typ } | Expr::UnaryExp { op : _, expr : _, typ }
            | Expr::BinaryExp { op : _, lhs : _, rhs : _, typ }
            | Expr::CastExpr { expr : _, typ }
            | Expr::CondExpr { cond : _, thn : _, els : _, typ }
            | Expr::CallExpr { func : _, ty_args : _, dyn_consts : _, args : _, typ }
            | Expr::Zero { typ }
            => *typ
        }
    }
}

// Start of parsing and semantic analysis

// Loads the contents of the given file name, parses, and performs semantic analysis
pub fn parse_and_analyze(src_file : String) -> Result<Prg, ErrorMessages> {
    if let Ok(mut file) = File::open(src_file) {
        let mut contents = String::new();
        if let Ok(_) = file.read_to_string(&mut contents) {
            let lexerdef = lang_l::lexerdef();
            let lexer = lexerdef.lexer(&contents);
            let (res, errs) = lang_y::parse(&lexer);

            if errs.is_empty() {
                match res {
                    None => Err(singleton_error(
                            ErrorMessage::SyntaxError("Parser did not return".to_string()))),
                    Some(Err(())) => Err(singleton_error(
                            ErrorMessage::SyntaxError("Unspecified parse error".to_string()))),
                    Some(Ok(r)) => analyze_program(r, &lexer),
                }
            } else {
                Err(errs.iter()
                        .map(|e| ErrorMessage::SyntaxError(
                                e.pp(&lexer, &lang_y::token_epp)))
                        .collect())
            }
        } else {
            Err(singleton_error(ErrorMessage::IOError("Unable to read input file".to_string())))
        }
    } else {
        Err(singleton_error(ErrorMessage::IOError("Unable to open input file".to_string())))
    }
}

fn analyze_program(
    prg : lang_y::Prg, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>)
    -> Result<Prg, ErrorMessages> {
    
    let mut stringtab = StringTable::new();
    let mut env : Env<usize, Entity> = Env::new();
    let mut types = TypeSolver::new();

    let mut res = vec![];

    env.open_scope();

    for top in prg {
        match top {
            lang_y::Top::Import { span, name: _ } => {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "imports".to_string())))?
            },
            lang_y::Top::TypeDecl { span : _, public: _, name, ty_vars, body } => {
                // TODO: Handle public
                env.open_scope(); // Create a new scope for the body (for type variables)

                // Add the type variables to the environment
                let mut num_type = 0;
                let mut num_dyn_const = 0;
                // Track the kinds of the variables
                let mut kinds = vec![];

                for TypeVar { span : _, name, kind } in ty_vars {
                    let nm = intern_id(&name, lexer, &mut stringtab);
                    kinds.push(kind);

                    match kind {
                        Kind::Type => {
                            let typ = types.new_type_var(nm, num_type, false, false);
                            env.insert(nm, Entity::Type { type_args : vec![], value : typ });
                            num_type += 1;
                        },
                        Kind::USize => {
                            env.insert(nm, Entity::DynConst { value : num_dyn_const });
                            num_dyn_const += 1;
                        },
                        Kind::Number => {
                            let typ = types.new_type_var(nm, num_type, true, false);
                            env.insert(nm, Entity::Type { type_args : vec![], value : typ });
                            num_type += 1;
                        },
                        Kind::Integer => {
                            let typ = types.new_type_var(nm, num_type, true, true);
                            env.insert(nm, Entity::Type { type_args : vec![], value : typ });
                            num_type += 1;
                        },
                    }
                }

                let nm = intern_id(&name, lexer, &mut stringtab);
                let typ = process_type_def(body, nm, lexer, &mut stringtab,
                                           &mut env, &mut types)?;
                
                // Insert information into the global scope
                env.close_scope();
                env.insert(nm, Entity::Type { type_args : kinds, value : typ });
            },
            lang_y::Top::ConstDecl { span : _, public: _, name, ty: _, body } => {
                // TODO: Handle public
                let nm = intern_id(&name, lexer, &mut stringtab);
                let val = process_expr_as_constant(body, lexer, &mut stringtab,
                                                   &mut env, &mut types)?;
                env.insert(nm, Entity::Constant { value : val });
            },
            lang_y::Top::FuncDecl { span, public: _, attr: _, name, ty_vars, args, ty, body } => {
                // TODO: Handle public, attributes
                env.open_scope(); // Open a new scope immediately to put type variables in

                // Process the type variables and add them into the environment
                let mut num_dyn_const = 0;
                let mut num_type_var  = 0;
                let mut type_kinds = vec![];
                for TypeVar { span : _, name, kind } in ty_vars {
                    type_kinds.push(kind);
                    let nm = intern_id(&name, lexer, &mut stringtab);
                    match kind {
                        Kind::USize => {
                            let num = num_dyn_const;
                            num_dyn_const += 1;

                            env.insert(nm, Entity::DynConst { value : num });
                        },
                        Kind::Type => {
                            let typ = types.new_type_var(nm, num_type_var, false, false);
                            env.insert(nm, Entity::Type { type_args : vec![], value : typ });
                            num_type_var += 1;
                        },
                        Kind::Number => {
                            let typ = types.new_type_var(nm, num_type_var, true, false);
                            env.insert(nm, Entity::Type { type_args : vec![], value : typ });
                            num_type_var += 1;
                        },
                        Kind::Integer => {
                            let typ = types.new_type_var(nm, num_type_var, true, true);
                            env.insert(nm, Entity::Type { type_args : vec![], value : typ });
                            num_type_var += 1;
                        },
                    }
                }

                // Process arguments
                let mut arg_types : Vec<(usize, Type, bool)> = vec![]; // list of name, type, and
                                                                       // whether is inout
                let mut inout_args = vec![]; // list of indices into args
                
                // A collection of errors we encounter processing the arguments
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

                            let nm = intern_package_name(&name, lexer, &mut stringtab)[0];
                            match process_type(typ.expect("FROM ABOVE"), lexer,
                                               &mut stringtab, &env, &mut types) {
                                Ok(ty) => {
                                    if inout.is_some() {
                                        inout_args.push(arg_types.len());
                                    }
                                    arg_types.push((nm, ty, inout.is_some()));
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
                            types.new_primitive(types::Primitive::Unit)
                        },
                        Some(ty) => {
                            match process_type(ty, lexer, &mut stringtab, &env,
                                               &mut types) {
                                Ok(ty) => ty,
                                Err(mut errs) => {
                                    errors.append(&mut errs);
                                    types.new_primitive(types::Primitive::Unit)
                                },
                            }
                        },
                    };

                if !errors.is_empty() {
                    Err(errors)?
                }

                // Compute the proper type accounting for the inouts (which become returns)
                let mut inout_types = vec![];
                for arg_idx in &inout_args {
                    inout_types.push(arg_types[*arg_idx].1.clone());
                }
                
                let inout_tuple = types.new_tuple(inout_types.clone());
                let pure_return_type
                    = types.new_tuple(vec![return_type, inout_tuple]);

                // Add the arguments to the environment and assign each a unique variable number
                // Also track the variable numbers of the inout arguments for generating returns
                let mut arg_variables = vec![];
                let mut inout_variables = vec![];
                for (nm, ty, is_inout) in arg_types.iter() {
                    let variable = env.uniq();
                    env.insert(*nm,
                               Entity::Variable {
                                   variable : variable,
                                   typ : *ty,
                                   is_const : false });
                    arg_variables.push(variable);

                    if *is_inout { inout_variables.push(variable); }
                }

                // Finally, we have a properly built environment and we can
                // start processing the body
                let (mut body, end_reachable)
                    = process_stmt(body, lexer, &mut stringtab, &mut env, &mut types,
                                   false, return_type, &inout_variables, &inout_types)?;

                if end_reachable {
                    // The end of a function being reachable (i.e. there is some possible path
                    // where there is no return statement) is an error unless the return type is
                    // void
                    if types.is_void(return_type) {
                        // Insert return at the end
                        body = Stmt::BlockStmt {
                            body : vec![
                                    body,
                                    generate_return(Expr::Tuple {
                                            vals : vec![],
                                            typ : types.new_primitive(types::Primitive::Unit)
                                        },
                                        &inout_variables, &inout_types,
                                        &mut types)] };
                    } else {
                        Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(span, lexer),
                                    "May reach end of control without return".to_string())))?
                    }
                }

                env.close_scope();

                // Add the function to the global environment
                let nm = intern_id(&name, lexer, &mut stringtab);
                env.insert(nm, Entity::Function {
                    index       : res.len(),
                    type_args   : type_kinds,
                    args        : arg_types.iter()
                                    .map(|(_, ty, is)| (*ty, *is))
                                    .collect::<Vec<_>>(),
                    return_type : return_type });
                
                // Add the function definition to the list of functions
                res.push(Function {
                    name           : lexer.span_str(name).to_string(),
                    num_dyn_consts : num_dyn_const,
                    num_type_args  : num_type_var,
                    arguments      : arg_types.iter().map(|v| v.1).collect::<Vec<_>>(),
                    return_type    : pure_return_type,
                    body           : body });
            },
            lang_y::Top::ModDecl { span, public: _, name: _, body: _ } => {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "modules".to_string())))?
            },
        }
    }

    Ok((types, res))
}

fn process_type_def(def : lang_y::TyDef, name : usize,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
    stringtab : &mut StringTable, env : &mut Env<usize, Entity>,
    types : &mut TypeSolver) -> Result<Type, ErrorMessages> {
  
    match def {
        lang_y::TyDef::TypeAlias { span: _, body } => {
            process_type(body, lexer, stringtab, env, types)
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
                        match process_type(ty, lexer, stringtab, env, types) {
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
                Ok(types.new_struct(name, env.uniq(), field_list, field_map))
            }
        },
        lang_y::TyDef::Union { span : _, public: _, fields } => {
            // TODO: handle public correctly
            let mut constr_list = vec![];
            let mut constr_map = HashMap::new();
            let mut errors = LinkedList::new();

            for ObjField { span, public, name, typ } in fields {
                if public {
                    errors.push_back(
                        ErrorMessage::SemanticError(
                            span_to_loc(span, lexer),
                            "union constructors cannot be marked public, all constructors share the visibility of the union".to_string()));
                } else {
                    let nm = intern_id(&name, lexer, stringtab);
                    match typ {
                        None => {
                            let idx = constr_list.len();
                            constr_list.push(types.new_primitive(types::Primitive::Unit));
                            constr_map.insert(nm, idx);
                        },
                        Some(ty) => {
                            match process_type(ty, lexer, stringtab, env, types) {
                                Ok(typ) => {
                                    let idx = constr_list.len();
                                    constr_list.push(typ);
                                    constr_map.insert(nm, idx);
                                },
                                Err(mut errs) => errors.append(&mut errs),
                            }
                        },
                    }
                }
            }

            if !errors.is_empty() { Err(errors) }
            else {
                Ok(types.new_union(name, env.uniq(), constr_list, constr_map))
            }
        },
    }
}

fn process_type(typ : lang_y::Type, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
                stringtab : &mut StringTable, env : &Env<usize, Entity>,
                types : &mut TypeSolver) -> Result<Type, ErrorMessages> {

    match typ {
        lang_y::Type::PrimType { span: _, typ } => {
            Ok(types.new_primitive(convert_primitive(typ)))
        },
        lang_y::Type::TupleType { span: _, tys } => {
            let mut fields = vec![];
            let mut errors = LinkedList::new();

            for ty in tys {
                match process_type(ty, lexer, stringtab, env, types) {
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
                    Ok(types.new_tuple(fields))
                }
            }
        },
        lang_y::Type::NamedType { span, name, args } => {
            if name.len() != 1 {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "packages".to_string())))
            } else {
                let id = intern_package_name(&name, lexer, stringtab);
                let nm = id[0];
                match env.lookup(&nm) {
                    Some(Entity::Type { type_args, value }) => {
                        if args.len() != type_args.len() {
                            Err(singleton_error(
                                    ErrorMessage::SemanticError(
                                        span_to_loc(span, lexer),
                                        format!("Expected {} type arguments, provided {}",
                                                type_args.len(), args.len()))))?
                        }

                        // Process the type arguments, ensuring they match the given kinds
                        let mut type_vars = vec![];
                        let mut dynamic_constants = vec![];
                        let mut errors = LinkedList::new();

                        for (arg, kind) in args.into_iter().zip(type_args.iter()) {
                            let arg_span = arg.span();
                            match kind {
                                lang_y::Kind::USize => {
                                    match process_type_expr_as_expr(
                                            arg, lexer, stringtab, env, types) {
                                        Err(mut errs) => errors.append(&mut errs),
                                        Ok(val) => dynamic_constants.push(val),
                                    }
                                },
                                lang_y::Kind::Type => {
                                    match process_type_expr_as_type(
                                            arg, lexer, stringtab, env, types) {
                                        Err(mut errs) => errors.append(&mut errs),
                                        Ok(typ) => type_vars.push(typ),
                                    }
                                },
                                lang_y::Kind::Number => {
                                    match process_type_expr_as_type(
                                            arg, lexer, stringtab, env, types) {
                                        Err(mut errs) => errors.append(&mut errs),
                                        Ok(typ) => {
                                            if types.is_number(typ) {
                                                type_vars.push(typ);
                                            } else {
                                                errors.push_back(
                                                    ErrorMessage::KindError(
                                                        span_to_loc(arg_span, lexer),
                                                        "number".to_string(),
                                                        types.to_string(typ,
                                                            &|n| stringtab.lookup_id(n).unwrap())));
                                            }
                                        },
                                    }
                                },
                                lang_y::Kind::Integer => {
                                    match process_type_expr_as_type(
                                            arg, lexer, stringtab, env, types) {
                                        Err(mut errs) => errors.append(&mut errs),
                                        Ok(typ) => {
                                            if types.is_integer(typ) {
                                                type_vars.push(typ);
                                            } else {
                                                errors.push_back(
                                                    ErrorMessage::KindError(
                                                        span_to_loc(arg_span, lexer),
                                                        "integer".to_string(),
                                                        types.to_string(typ,
                                                            &|n| stringtab.lookup_id(n).unwrap())));
                                            }
                                        },
                                    }
                                },
                            }
                        }

                        if !errors.is_empty() { Err(errors)? }

                        if type_vars.len() == 0 && dynamic_constants.len() == 0 {
                            Ok(*value)
                        } else {
                            Ok(types.instantiate(*value, &type_vars, &dynamic_constants))
                        }
                    },
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

            let element = process_type(*elem, lexer, stringtab, env, types);

            for dim in dims {
                match process_type_expr_as_expr(dim, lexer, stringtab, env, types) {
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
                        if types.is_array(element_type) {
                            let elem_type = types.get_element_type(element_type).unwrap();
                            let mut inner_dims = types.get_dimensions(element_type).unwrap();

                            dimensions.append(&mut inner_dims);
                            Ok(types.new_array(elem_type, dimensions))
                        } else {
                            Ok(types.new_array(element_type, dimensions))
                        }
                    }
                }
            }
        },
    }
}

fn process_type_expr_as_expr(exp : lang_y::TypeExpr,
                             lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
                             stringtab : &mut StringTable,
                             env : &Env<usize, Entity>, types : &mut TypeSolver)
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
            if name.len() != 1 {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "packages".to_string())))
            } else {
                let id = intern_package_name(&name, lexer, stringtab);
                let nm = id[0];
                match env.lookup(&nm) {
                    Some(Entity::DynConst { value }) => {
                        if args.len() > 0 {
                            Err(singleton_error(
                                    ErrorMessage::SemanticError(
                                        span_to_loc(span, lexer),
                                        format!("No type arguments exists on dynamic constants"))))
                        } else {
                            Ok(DynamicConstant::DynConst(nm, *value))
                        }
                    },
                    Some(Entity::Constant { value : (val, typ) }) => {
                        match val {
                            Literal::Unsigned(val) =>
                                Ok(DynamicConstant::Constant(*val as usize)),
                            _ =>
                                Err(singleton_error(
                                        ErrorMessage::TypeError(
                                            span_to_loc(span, lexer),
                                            "usize".to_string(),
                                            types.to_string(*typ,
                                                &|n| stringtab.lookup_id(n).unwrap())))),
                        }
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
                    Some(Entity::Function { .. }) =>
                        Err(singleton_error(
                                ErrorMessage::KindError(
                                    span_to_loc(span, lexer),
                                    "dynamic constant expression".to_string(),
                                    "function".to_string()))),
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

fn process_type_expr_as_type(exp : lang_y::TypeExpr,
                             lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
                             stringtab : &mut StringTable,
                             env : &Env<usize, Entity>, types : &mut TypeSolver)
    -> Result<Type, ErrorMessages> {

    match exp {
        lang_y::TypeExpr::IntLiteral { span, .. }
        | lang_y::TypeExpr::Negative { span, .. }
        | lang_y::TypeExpr::Add { span, .. }
        | lang_y::TypeExpr::Sub { span, .. }
        | lang_y::TypeExpr::Mul { span, .. } => {
            Err(singleton_error(
                    ErrorMessage::KindError(
                        span_to_loc(span, lexer),
                        "type".to_string(), "expression".to_string())))
        },
        lang_y::TypeExpr::PrimType { span : _, typ } => {
            Ok(types.new_primitive(convert_primitive(typ)))
        },
        lang_y::TypeExpr::TupleType { span : _, tys } => {
            let mut fields = vec![];
            let mut errors = LinkedList::new();

            for ty in tys {
                match process_type_expr_as_type(ty, lexer, stringtab, env, types) {
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
                    Ok(types.new_tuple(fields))
                }
            }
        },
        lang_y::TypeExpr::ArrayTypeExpr { span : _, elem, dims } => {
            let mut dimensions = vec![];
            let mut errors = LinkedList::new();

            let element = process_type_expr_as_type(*elem, lexer, stringtab, env, types);

            for dim in dims {
                match process_type_expr_as_expr(dim, lexer, stringtab, env, types) {
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
                        if types.is_array(element_type) {
                            let elem_type = types.get_element_type(element_type).unwrap();
                            let mut inner_dims = types.get_dimensions(element_type).unwrap();

                            dimensions.append(&mut inner_dims);
                            Ok(types.new_array(elem_type, dimensions))
                        } else {
                            Ok(types.new_array(element_type, dimensions))
                        }
                    }
                }
            }
        },
        lang_y::TypeExpr::NamedTypeExpr { span, name, args } => {
            if name.len() != 1 {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "packages".to_string())))
            } else {
                let id = intern_package_name(&name, lexer, stringtab);
                let nm = id[0];
                match env.lookup(&nm) {
                    Some(Entity::Type { type_args, value }) => {
                        if args.len() != type_args.len() {
                            Err(singleton_error(
                                    ErrorMessage::SemanticError(
                                        span_to_loc(span, lexer),
                                        format!("Expected {} type arguments, provided {}",
                                                type_args.len(), args.len()))))?
                        }

                        // Process the type arguments, ensuring they match the given kinds
                        let mut type_vars = vec![];
                        let mut dynamic_constants = vec![];
                        let mut errors = LinkedList::new();

                        for (arg, kind) in args.into_iter().zip(type_args.iter()) {
                            let arg_span = arg.span();
                            match kind {
                                lang_y::Kind::USize => {
                                    match process_type_expr_as_expr(
                                            arg, lexer, stringtab, env, types) {
                                        Err(mut errs) => errors.append(&mut errs),
                                        Ok(val) => dynamic_constants.push(val),
                                    }
                                },
                                lang_y::Kind::Type => {
                                    match process_type_expr_as_type(
                                            arg, lexer, stringtab, env, types) {
                                        Err(mut errs) => errors.append(&mut errs),
                                        Ok(typ) => type_vars.push(typ),
                                    }
                                },
                                lang_y::Kind::Number => {
                                    match process_type_expr_as_type(
                                            arg, lexer, stringtab, env, types) {
                                        Err(mut errs) => errors.append(&mut errs),
                                        Ok(typ) => {
                                            if types.is_number(typ) {
                                                type_vars.push(typ);
                                            } else {
                                                errors.push_back(
                                                    ErrorMessage::KindError(
                                                        span_to_loc(arg_span, lexer),
                                                        "number".to_string(),
                                                        types.to_string(typ,
                                                            &|n| stringtab.lookup_id(n).unwrap())));
                                            }
                                        },
                                    }
                                },
                                lang_y::Kind::Integer => {
                                    match process_type_expr_as_type(
                                            arg, lexer, stringtab, env, types) {
                                        Err(mut errs) => errors.append(&mut errs),
                                        Ok(typ) => {
                                            if types.is_integer(typ) {
                                                type_vars.push(typ);
                                            } else {
                                                errors.push_back(
                                                    ErrorMessage::KindError(
                                                        span_to_loc(arg_span, lexer),
                                                        "integer".to_string(),
                                                        types.to_string(typ,
                                                            &|n| stringtab.lookup_id(n).unwrap())));
                                            }
                                        },
                                    }
                                },
                            }
                        }

                        if !errors.is_empty() { Err(errors)? }

                        if type_vars.len() == 0 && dynamic_constants.len() == 0 {
                            Ok(*value)
                        } else {
                            Ok(types.instantiate(*value, &type_vars, &dynamic_constants))
                        }
                    },
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
    }
}

// Normalizes the given statement, and returns the normalized statement plus whether a statement
// after the analyzed one is reachable or not
fn process_stmt(stmt : lang_y::Stmt, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
                stringtab : &mut StringTable, env : &mut Env<usize, Entity>,
                types : &mut TypeSolver, in_loop : bool, return_type : Type,
                inout_vars : &Vec<usize>, inout_types : &Vec<Type>)
    -> Result<(Stmt, bool), ErrorMessages> {

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
                        Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(span, lexer),
                                    "Bound variables must be local names, without a package separator".to_string())))?
                    }

                    let nm = intern_package_name(&name, lexer, stringtab)[0];
                    let ty = process_type(typ.expect("FROM ABOVE"), lexer, stringtab, env, types)?;

                    let var = env.uniq();

                    let ((val, typ), exp_loc) =
                        match init {
                            Some(exp) => {
                                let loc = span_to_loc(exp.span(), lexer);
                                (process_expr(exp, lexer, stringtab, env, types)?, loc)
                            },
                            None => {
                                ((Expr::Zero { typ : ty }, ty), Location::fake())
                            },
                        };

                    env.insert(nm,
                               Entity::Variable { variable : var, typ : ty, is_const : false });

                    if !types.can_coerce(ty, typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    exp_loc,
                                    types.to_string(ty, &|n| stringtab.lookup_id(n).unwrap()),
                                    types.to_string(typ, &|n| stringtab.lookup_id(n).unwrap()))))
                    } else {
                        Ok((Stmt::AssignStmt { var : var, val : val }, true))
                    }
                },
                _ => {
                    Err(singleton_error(
                            ErrorMessage::NotImplemented(
                                span_to_loc(v_span, lexer),
                                "non-variable bindings".to_string())))
                },
            }
        },
        // HERE
        /*
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
        */
        _ => todo!(),
    }
}

fn process_expr_as_constant(expr : lang_y::Expr,
                            lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
                            stringtab : &mut StringTable, env : &mut Env<usize, Entity>,
                            types : &mut TypeSolver)
    -> Result<Constant, ErrorMessages> {

    todo!()
}

fn process_expr(expr : lang_y::Expr, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
                stringtab : &mut StringTable, env : &mut Env<usize, Entity>,
                types : &mut TypeSolver) -> Result<(Expr, Type), ErrorMessages> {

    todo!()
}

fn generate_return(expr : Expr, vars : &Vec<usize>, var_types : &Vec<Type>,
                   types : &mut TypeSolver) -> Stmt {
    let var_exprs = vars.iter().zip(var_types.iter())
                        .map(|(var, typ)| Expr::Variable { var : *var, typ : *typ })
                        .collect::<Vec<_>>();

    let inout_type = types.new_tuple(var_types.clone());
    let inout_vals = Expr::Tuple { vals : var_exprs, typ : inout_type };

    let expr_type = expr.get_type();

    let val = Expr::Tuple { vals : vec![expr, inout_vals],
                            typ  : types.new_tuple(vec![expr_type, inout_type]) };

    Stmt::ReturnStmt { expr : val }
}

fn convert_primitive(prim : lang_y::Primitive) -> types::Primitive {
    match prim {
        lang_y::Primitive::Bool => types::Primitive::Bool,
        lang_y::Primitive::I8   => types::Primitive::I8,
        lang_y::Primitive::U8   => types::Primitive::U8,
        lang_y::Primitive::I16  => types::Primitive::I16,
        lang_y::Primitive::U16  => types::Primitive::U16,
        lang_y::Primitive::I32  => types::Primitive::I32,
        lang_y::Primitive::U32  => types::Primitive::U32,
        lang_y::Primitive::I64  => types::Primitive::I64,
        lang_y::Primitive::U64  => types::Primitive::U64,
        lang_y::Primitive::USize=> types::Primitive::U64,
        lang_y::Primitive::F32  => types::Primitive::F32,
        lang_y::Primitive::F64  => types::Primitive::F64,
        lang_y::Primitive::Void => types::Primitive::Unit,
    }
}

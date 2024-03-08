extern crate hercules_ir;

use std::collections::{HashMap, LinkedList};
use std::fs::File;
use std::io::Read;
use std::fmt;

use lrlex::{lrlex_mod, DefaultLexerTypes};
use lrpar::{lrpar_mod, NonStreamingLexer};
use cfgrammar::Span;

use ordered_float::OrderedFloat;

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
#[derive(Clone, Debug)]
pub enum Literal {
    Unit, Bool(bool), Integer(u64), Float(f64),
    Tuple(Vec<Constant>),
    Sum(usize, Box<Constant>), // The tag and value
}
pub type Constant = (Literal, Type);

impl PartialEq for Literal {
    fn eq(&self, other : &Self) -> bool {
        match (self, other) {
            (Literal::Unit, Literal::Unit) => true,
            (Literal::Bool(b), Literal::Bool(c)) => b == c,
            (Literal::Integer(i), Literal::Integer(j)) => i == j,
            (Literal::Float(i), Literal::Float(j)) =>
                OrderedFloat(*i) == OrderedFloat(*j),
            (Literal::Tuple(fs), Literal::Tuple(gs)) => fs == gs,
            (Literal::Sum(i, v), Literal::Sum(j, u)) => i == j && *v == *u,
            _ => false,
        }
    }
}

impl Eq for Literal {}

impl Literal {
    fn as_usize(&self) -> usize {
        match self {
            Literal::Integer(val) => *val as usize,
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
pub struct Location {
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
    pub arguments : Vec<(usize, Type)>,
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
    // TODO: Not implemented
    //MatchStmt    { expr : Expr, cases : Vec<usize>, body : Vec<Stmt> },
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
//    TODO: Technically inout arguments could be any l-expression
// 7. There's an additional Zero which is used to construct the default of a type
#[derive(Clone, Debug)]
pub enum Expr {
    Variable  { var : usize, typ : Type },
    DynConst  { idx : usize, typ : Type },
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

#[derive(Clone, Debug)]
pub enum Index { Field(usize), Variant(usize), Array(Vec<Expr>) }

#[derive(Clone, Debug)]
pub enum UnaryOp  { Negation, BitwiseNot }
#[derive(Clone, Debug)]
pub enum BinaryOp { Add, Sub, Mul, Div, Mod,
                    BitAnd, BitOr, Xor,
                    Lt, Le, Gt, Ge, Eq, Neq,
                    LShift, RShift }

fn convert_assign_op(op : lang_y::AssignOp) -> BinaryOp {
    match op {
        AssignOp::None   => panic!("Do not call convert_assign_op on AssignOp::None"),
        AssignOp::Add    => BinaryOp::Add,
        AssignOp::Sub    => BinaryOp::Sub,
        AssignOp::Mul    => BinaryOp::Mul,
        AssignOp::Div    => BinaryOp::Div,
        AssignOp::Mod    => BinaryOp::Mod,
        AssignOp::BitAnd => BinaryOp::BitAnd,
        AssignOp::BitOr  => BinaryOp::BitOr,
        AssignOp::Xor    => BinaryOp::Xor,
        AssignOp::LShift => BinaryOp::LShift,
        AssignOp::RShift => BinaryOp::RShift,
        AssignOp::LogAnd => panic!("Do not call convert_assign_op on AssignOp::LogAnd"),
        AssignOp::LogOr  => panic!("Do not call convert_assign_op on AssignOp::LogOr"),
    }
}

fn convert_binary_op(op : lang_y::BinaryOp) -> BinaryOp {
    match op {
        lang_y::BinaryOp::Add    => BinaryOp::Add,
        lang_y::BinaryOp::Sub    => BinaryOp::Sub,
        lang_y::BinaryOp::Mul    => BinaryOp::Mul,
        lang_y::BinaryOp::Div    => BinaryOp::Div,
        lang_y::BinaryOp::Mod    => BinaryOp::Mod,
        lang_y::BinaryOp::BitAnd => BinaryOp::BitAnd,
        lang_y::BinaryOp::BitOr  => BinaryOp::BitOr,
        lang_y::BinaryOp::Xor    => BinaryOp::Xor,
        lang_y::BinaryOp::Lt     => BinaryOp::Lt,
        lang_y::BinaryOp::Le     => BinaryOp::Le,
        lang_y::BinaryOp::Gt     => BinaryOp::Gt,
        lang_y::BinaryOp::Ge     => BinaryOp::Ge,
        lang_y::BinaryOp::Eq     => BinaryOp::Eq,
        lang_y::BinaryOp::Neq    => BinaryOp::Neq,
        lang_y::BinaryOp::LShift => BinaryOp::LShift,
        lang_y::BinaryOp::RShift => BinaryOp::RShift,
        lang_y::BinaryOp::LogAnd => panic!("Do not call convert_binary_op on BinaryOp::LogAnd"),
        lang_y::BinaryOp::LogOr  => panic!("Do not call convert_binary_op on BinaryOp::LogOr"),
    }
}

// Be able to access the type of an expression easily
impl Expr {
    pub fn get_type(&self) -> Type {
        match self {
            Expr::Variable { var : _, typ } | Expr::DynConst { idx : _, typ }
            | Expr::Read { index : _, val : _, typ }
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

// Helper function to unparse types
fn unparse_type(types : &TypeSolver, typ : Type, stringtab : &StringTable) -> String {
    types.to_string(typ, &|n| stringtab.lookup_id(n).unwrap())
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
                        Pattern::Variable { span, name } => {
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
                    arguments      : arg_types.iter().zip(arg_variables.iter())
                                        .map(|(v, n)| (*n, v.1)).collect::<Vec<_>>(),
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
                                                        unparse_type(types, typ, stringtab)));
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
                                                        unparse_type(types, typ, stringtab)));
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
                            Literal::Integer(val) =>
                                Ok(DynamicConstant::Constant(*val as usize)),
                            _ =>
                                Err(singleton_error(
                                        ErrorMessage::TypeError(
                                            span_to_loc(span, lexer),
                                            "usize".to_string(),
                                            unparse_type(types, *typ, stringtab)))),
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
                                                        unparse_type(types, typ, stringtab)));
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
                                                        unparse_type(types, typ, stringtab)));
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
                Pattern::Variable { span, name } => {
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

                    let (val, exp_loc) =
                        match init {
                            Some(exp) => {
                                let loc = span_to_loc(exp.span(), lexer);
                                (process_expr(exp, lexer, stringtab, env, types)?, loc)
                            },
                            None => {
                                (Expr::Zero { typ : ty }, Location::fake())
                            },
                        };
                    let typ = val.get_type();

                    env.insert(nm,
                               Entity::Variable { variable : var, typ : ty, is_const : false });

                    if !types.equal(ty, typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    exp_loc,
                                    unparse_type(types, ty, stringtab),
                                    unparse_type(types, typ, stringtab))))
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
        lang_y::Stmt::ConstStmt { span: _, var : VarBind { span : v_span, pattern, typ }, init } => {
            match pattern {
                Pattern::Variable { span, name } => {
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

                    let (val, exp_loc) =
                        match init {
                            Some(exp) => {
                                let loc = span_to_loc(exp.span(), lexer);
                                (process_expr(exp, lexer, stringtab, env, types)?, loc)
                            },
                            None => {
                                (Expr::Zero { typ : ty }, Location::fake())
                            },
                        };
                    let typ = val.get_type();

                    env.insert(nm,
                               Entity::Variable { variable : var, typ : ty, is_const : true });

                    if !types.equal(ty, typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    exp_loc,
                                    unparse_type(types, ty, stringtab),
                                    unparse_type(types, typ, stringtab))))
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
        lang_y::Stmt::AssignStmt { span: _, lhs, assign, assign_span, rhs } => {
            let lhs_res = process_lexpr(lhs, lexer, stringtab, env, types);
            let rhs_res = process_expr(rhs, lexer, stringtab, env, types);
            let (((var, var_typ), (exp_typ, index)), val)
                = append_errors2(lhs_res, rhs_res)?;
            let typ = val.get_type();

            // Perform the appropriate type checking
            match assign {
                AssignOp::None   => {
                    if !types.equal(exp_typ, typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(assign_span, lexer),
                                    unparse_type(types, exp_typ, stringtab),
                                    unparse_type(types, typ, stringtab))))?
                    }
                },
                AssignOp::Add | AssignOp::Sub | AssignOp::Mul | AssignOp::Div => {
                    if !types.equal(exp_typ, typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(assign_span, lexer),
                                    unparse_type(types, exp_typ, stringtab),
                                    unparse_type(types, typ, stringtab))))?
                    }
                    if !types.is_number(exp_typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(assign_span, lexer),
                                    "number".to_string(),
                                    unparse_type(types, exp_typ, stringtab))))?
                    }
                },
                AssignOp::Mod | AssignOp::BitAnd | AssignOp::BitOr | AssignOp::Xor
                    | AssignOp::LShift | AssignOp::RShift => {
                    if !types.equal(exp_typ, typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(assign_span, lexer),
                                    unparse_type(types, exp_typ, stringtab),
                                    unparse_type(types, typ, stringtab))))?
                    }
                    if !types.is_integer(exp_typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(assign_span, lexer),
                                    "integer".to_string(),
                                    unparse_type(types, exp_typ, stringtab))))?
                    }
                },
                AssignOp::LogAnd | AssignOp::LogOr => {
                    if !types.equal(exp_typ, typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(assign_span, lexer),
                                    unparse_type(types, exp_typ, stringtab),
                                    unparse_type(types, typ, stringtab))))?
                    }
                    if !types.is_bool(exp_typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(assign_span, lexer),
                                    "bool".to_string(),
                                    unparse_type(types, exp_typ, stringtab))))?
                    }
                },
            }

            let empty_index = index.is_empty();
            let rhs_var = Expr::Variable { var : var, typ : var_typ };

            let rhs_val =
                if empty_index {
                    rhs_var
                } else {
                    Expr::Read {
                        index : index.clone(),
                        val   : Box::new(rhs_var),
                        typ   : exp_typ }
                };

            // Construct the right-hand side for the normalized expression; for x= operations this
            // will construct the read and the operation; the write is left for after this since it
            // is common to all cases
            let result_rhs = 
                match assign {
                    AssignOp::None   => {
                        val
                    },
                    AssignOp::Add | AssignOp::Sub | AssignOp::Mul | AssignOp::Div
                        | AssignOp::Mod | AssignOp::BitAnd | AssignOp::BitOr
                        | AssignOp::Xor | AssignOp::LShift | AssignOp::RShift => {
                        Expr::BinaryExp {
                            op  : convert_assign_op(assign),
                            lhs : Box::new(rhs_val),
                            rhs : Box::new(val),
                            typ : typ }
                    },
                    // For x &&= y we convert to if x then y else false
                    AssignOp::LogAnd => {
                        Expr::CondExpr {
                            cond : Box::new(rhs_val),
                            thn  : Box::new(val),
                            // We know that the expected type is bool, so just use it to avoid
                            // creating additional new types
                            els  : Box::new(Expr::Constant {
                                    val : (Literal::Bool(false), exp_typ),
                                    typ : exp_typ }),
                            typ  : typ }
                    },
                    // For x ||= y we convert to if x then true else y
                    AssignOp::LogOr  => {
                        Expr::CondExpr {
                            cond : Box::new(rhs_val),
                            thn  : Box::new(Expr::Constant {
                                    val : (Literal::Bool(true), exp_typ),
                                    typ : exp_typ }),
                            els  : Box::new(val),
                            typ  : typ }
                    },
                };

            let write_exp =
                if empty_index {
                    result_rhs
                } else {
                    Expr::Write {
                        index : index,
                        val   : Box::new(Expr::Variable { var : var, typ : var_typ }),
                        rep   : Box::new(result_rhs),
                        typ   : var_typ }
                };

            Ok((Stmt::AssignStmt {
                    var : var,
                    val : write_exp }, true))
        },
        lang_y::Stmt::IfStmt { span: _, cond, thn, els } => {
            let cond_span = cond.span();
            let cond_res = process_expr(cond, lexer, stringtab, env, types);

            env.open_scope();
            let thn_res  = process_stmt(*thn, lexer, stringtab, env, types,
                                        in_loop, return_type, inout_vars, inout_types);
            env.close_scope();

            env.open_scope();
            let els_res  =
                match els { None => Ok((None, true)),
                    Some(stmt) =>
                        process_stmt(*stmt, lexer, stringtab, env, types,
                                     in_loop, return_type, inout_vars, inout_types)
                        .map(|(s, b)| (Some(s), b)), };
            env.close_scope();

            let (cond_exp, (thn_body, thn_fall), (els_body, els_fall))
                = append_errors3(cond_res, thn_res, els_res)?;
            let cond_typ = cond_exp.get_type();

            if !types.is_bool(cond_typ) {
                Err(singleton_error(
                        ErrorMessage::TypeError(
                            span_to_loc(cond_span, lexer),
                            "bool".to_string(),
                            unparse_type(types, cond_typ, stringtab))))?
            }

            Ok((Stmt::IfStmt {
                cond : cond_exp,
                thn  : Box::new(thn_body),
                els  : els_body.map(|s| Box::new(s)) },
                thn_fall || els_fall))
        },
        lang_y::Stmt::MatchStmt { span, expr: _, body: _ } => {
            Err(singleton_error(
                    ErrorMessage::NotImplemented(
                        span_to_loc(span, lexer),
                        "match statements".to_string())))
        },
        lang_y::Stmt::ForStmt { span: _, var : VarBind { span : v_span, pattern, typ },
                                init, bound, step, body } => {
            let (var, var_name, var_type) =
                match pattern {
                    Pattern::Variable { span, name } => {
                        if name.len() != 1 {
                            return Err(singleton_error(
                                    ErrorMessage::SemanticError(
                                        span_to_loc(span, lexer),
                                        "Bound variables must be local names, without a package separator".to_string())));
                        }

                        let nm = intern_package_name(&name, lexer, stringtab)[0];
                        let var_type =
                            match typ {
                                None => types.new_primitive(types::Primitive::U64),
                                Some(t) => {
                                    let ty = process_type(t, lexer, stringtab, env, types)?;
                                    if !types.is_integer(ty) {
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

            // Evaluate the initial value, bound, and step
            let init_span = init.span();
            let bound_span = bound.span();

            let init_res = process_expr(init, lexer, stringtab, env, types);
            let bound_res = process_expr(bound, lexer, stringtab, env, types);

            // The step is tracked as a pair of the step's amount (always positive) and whether the
            // step should be positive or negative
            let (step_val, step_pos) =
                match step {
                    None => {
                        (1, true)
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

                        (num, !negative)
                    },
                };

            let (init_val, bound_val)
                = append_errors2(init_res, bound_res)?;
            let init_typ = init_val.get_type();
            let bound_typ = bound_val.get_type();

            // Verify that the types of the initial value and bound are correct
            let mut type_errors = LinkedList::new();
            if !types.equal(var_type, init_typ) {
                type_errors.push_back(
                    ErrorMessage::TypeError(
                        span_to_loc(init_span, lexer),
                        unparse_type(types, var_type, stringtab),
                        unparse_type(types, init_typ, stringtab)));
            }
            if !types.equal(var_type, bound_typ) {
                type_errors.push_back(
                    ErrorMessage::TypeError(
                        span_to_loc(bound_span, lexer),
                        unparse_type(types, var_type, stringtab),
                        unparse_type(types, bound_typ, stringtab)));
            }
            if !type_errors.is_empty() {
                Err(type_errors)?
            }

            // Create the scope for the body
            env.open_scope();
            env.insert(var_name, Entity::Variable {
                                    variable : var, typ : var_type, is_const : true });

            // Process the body
            let (body, _)
                = process_stmt(*body, lexer, stringtab, env, types, true,
                               return_type, inout_vars, inout_types)?;

            env.close_scope();

            // We bind the initial value of the loop counter
            let init_eval = Stmt::AssignStmt { var : var, val : init_val };

            // We create a new variable for the loop bound and we're going to bind the bound to
            // that value before the loop so that it is only evaluated once
            let bound_var = env.uniq();
            let bound_eval = Stmt::AssignStmt { var : bound_var, val : bound_val };

            // The condition of the loop is var < bound, unless the step is negative in which case
            // it is var > bound
            let condition =
                Expr::BinaryExp {
                    op  : if step_pos { BinaryOp::Lt } else { BinaryOp::Gt },
                    lhs : Box::new(Expr::Variable { var : var, typ : var_type }),
                    rhs : Box::new(Expr::Variable { var : bound_var, typ : bound_typ }),
                    typ : types.new_primitive(types::Primitive::Bool) };

            // The update of the loop is var = var + step, unless the step is negative in which
            // case it is var = var - step
            let update =
                Stmt::AssignStmt {
                    var : var,
                    val : Expr::BinaryExp {
                            op  : if step_pos { BinaryOp::Add } else { BinaryOp::Sub },
                            lhs : Box::new(Expr::Variable { var : var, typ : var_type }),
                            rhs : Box::new(Expr::Constant {
                                                val : (Literal::Integer(step_val), var_type),
                                                typ : var_type }),
                            typ : var_type }};

            // Finally, the entire loop is constructed as:
            // Evaluate initial value
            // Evaluate bound value
            // Loop
            // Note that the statement after a loop is always assumed to be reachable
            Ok((Stmt::BlockStmt {
                    body : vec![
                        init_eval,
                        bound_eval,
                        Stmt::LoopStmt {
                            cond : condition,
                            update : Some(Box::new(update)),
                            body : Box::new(body)
                        }
                    ]
                }, true))
        },
        lang_y::Stmt::WhileStmt { span: _, cond, body } => {
            let cond_span = cond.span();
            let cond_res = process_expr(cond, lexer, stringtab, env, types);

            env.open_scope();
            let body_res = process_stmt(*body, lexer, stringtab, env, types,
                                        true, return_type, inout_vars, inout_types);
            env.close_scope();

            let (cond_val, (body_stmt, _))
                = append_errors2(cond_res, body_res)?;
            let cond_typ = cond_val.get_type();

            if !types.is_bool(cond_typ) {
                Err(singleton_error(
                        ErrorMessage::TypeError(
                            span_to_loc(cond_span, lexer),
                            "bool".to_string(),
                            unparse_type(types, cond_typ, stringtab))))?
            }

            // Again, the statement after a loop is always considered reachable
            Ok((Stmt::LoopStmt {
                cond : cond_val,
                update : None,
                body : Box::new(body_stmt) }, true))
        },
        lang_y::Stmt::ReturnStmt { span, expr } => {
            let return_val =
                if expr.is_none() && types.is_void(return_type) {
                    Expr::Constant {
                        val : (Literal::Unit, return_type),
                        typ : return_type }
                } else if expr.is_none() {
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("Expected return of type {} found no return value",
                                        unparse_type(types, return_type, stringtab)))))?
                } else {
                    let val = process_expr(expr.unwrap(), lexer, stringtab, env,
                                           types)?;
                    let typ = val.get_type();
                    if !types.equal(return_type, typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(span, lexer),
                                    unparse_type(types, return_type, stringtab),
                                    unparse_type(types, typ, stringtab))))?
                    }
                    val
                };

            // We return a tuple of the return value and of the inout variables
            // Statements after a return are never reachable
            Ok((generate_return(return_val, inout_vars, inout_types, types),
                false))
        },
        lang_y::Stmt::BreakStmt { span } => {
            if !in_loop {
                Err(singleton_error(
                        ErrorMessage::SemanticError(
                            span_to_loc(span, lexer),
                            "Break not contained within loop".to_string())))?
            }

            // Code after a break is unreachable
            Ok((Stmt::BreakStmt {}, false))
        },
        lang_y::Stmt::ContinueStmt { span } => {
            if !in_loop {
                Err(singleton_error(
                        ErrorMessage::SemanticError(
                            span_to_loc(span, lexer),
                            "Continue not contained within loop".to_string())))?
            }

            // Code after a continue is unreachable
            Ok((Stmt::ContinueStmt {}, false))
        },
        lang_y::Stmt::BlockStmt { span: _, body } => {
            // Blocks create a new scope for variables declared in them
            env.open_scope();

            let mut reachable = true;
            let mut errors = LinkedList::new();
            let mut res = vec![];

            for stmt in body {
                if !reachable {
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(stmt.span(), lexer),
                                "Unreachable statement".to_string())))?
                }

                match process_stmt(stmt, lexer, stringtab, env, types, in_loop,
                                   return_type, inout_vars, inout_types) {
                    Err(mut errs) => { errors.append(&mut errs); },
                    Ok((stmt, post_reachable)) => {
                        res.push(stmt);
                        reachable = post_reachable;
                    },
                }
            }

            env.close_scope();

            if !errors.is_empty() {
                Err(errors)
            } else {
                Ok((Stmt::BlockStmt { body : res }, reachable))
            }
        },
        lang_y::Stmt::CallStmt { span, name, ty_args, args } => {
            // Call statements are lowered to call expressions which is made a statment using the
            // ExprStmt constructor
            // Code after a call is always reachable
            Ok((Stmt::ExprStmt {
                expr : process_expr(
                           lang_y::Expr::CallExpr { span, name, ty_args, args },
                           lexer, stringtab, env, types)? },
                true))
        },
    }
}

// Process an l-expression to produce the variable that's modified and its type along with the type
// of the piece being modified and a list of the index operations needed to access the accessed
// piece
// This should only be used for the left-hand side of an assignment since it will return an error
// if the variable that is accessed is marked as constant
fn process_lexpr(expr : lang_y::LExpr,
                 lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
                 stringtab : &mut StringTable, env : &mut Env<usize, Entity>,
                 types : &mut TypeSolver)
    -> Result<((usize, Type), (Type, Vec<Index>)), ErrorMessages> {
    match expr {
        lang_y::LExpr::VariableLExpr { span } => {
            let nm = intern_id(&span, lexer, stringtab);
            match env.lookup(&nm) {
                Some(Entity::Variable { variable, typ, is_const }) => {
                    if *is_const {
                        Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(span, lexer),
                                    format!("Variable {} is const, cannot assign to it",
                                            lexer.span_str(span)))))
                    } else {
                        Ok(((*variable, *typ), (*typ, vec![])))
                    }
                },
                Some(Entity::DynConst { .. }) => {
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("{} is a dynamic constant, cannot assign to it",
                                        lexer.span_str(span)))))
                },
                Some(Entity::Constant { .. }) => {
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("{} is a constant, cannot assign to it",
                                        lexer.span_str(span)))))
                },
                Some(Entity::Function { .. }) => {
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("{} is a function, cannot assign to it",
                                        lexer.span_str(span)))))
                },
                Some(Entity::Type { .. }) => {
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("{} is a type, cannot assign to it",
                                        lexer.span_str(span)))))
                },
                None => {
                    Err(singleton_error(
                            ErrorMessage::UndefinedVariable(
                                span_to_loc(span, lexer),
                                lexer.span_str(span).to_string())))
                },
            }
        },
        lang_y::LExpr::FieldLExpr { span, lhs, rhs } => {
            let ((var, var_typ), (idx_typ, mut idx))
                = process_lexpr(*lhs, lexer, stringtab, env, types)?;
            let field_nm = intern_id(&rhs, lexer, stringtab);

            match types.get_field(idx_typ, field_nm) {
                None => Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("Type {} does not possess field {}",
                                        unparse_type(types, idx_typ, stringtab),
                                        stringtab.lookup_id(field_nm).unwrap())))),
                Some((field_idx, field_type)) => {
                    idx.push(Index::Field(field_idx));
                    Ok(((var, var_typ), (field_type, idx)))
                },
            }
        },
        lang_y::LExpr::NumFieldLExpr { span, lhs, rhs } => {
            let ((var, var_typ), (idx_typ, mut idx))
                = process_lexpr(*lhs, lexer, stringtab, env, types)?;

            // Identify the field number; to do this we remove the first character of the string of
            // the right-hand side since the ".###" is lexed as a single token
            let num = lexer.span_str(rhs)[1..].parse::<usize>()
                                              .expect("From lexical analysis");

            match types.get_index(idx_typ, num) {
                None => Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("Type {} does not possess index {}",
                                        unparse_type(types, idx_typ, stringtab),
                                        num)))),
                Some(field_type) => {
                    idx.push(Index::Field(num));
                    Ok(((var, var_typ), (field_type, idx)))
                },
            }
        },
        lang_y::LExpr::IndexLExpr { span, lhs, index } => {
            let ((var, var_typ), (idx_typ, mut idx))
                = process_lexpr(*lhs, lexer, stringtab, env, types)?;

            let mut indices = vec![];
            let mut errors = LinkedList::new();
            for idx in index {
                let idx_span = idx.span();
                match process_expr(idx, lexer, stringtab, env, types) {
                    Err(mut errs) => errors.append(&mut errs),
                    Ok(exp) => {
                        let typ = exp.get_type();
                        if !types.is_u64(typ) {
                            errors.push_back(
                                ErrorMessage::TypeError(
                                    span_to_loc(idx_span, lexer),
                                    "usize".to_string(),
                                    unparse_type(types, typ, stringtab)));
                        } else {
                            indices.push(exp);
                        }
                    },
                }
            }

            if !errors.is_empty() {
                Err(errors)?
            }

            if !types.is_array(idx_typ) {
                Err(singleton_error(
                        ErrorMessage::SemanticError(
                            span_to_loc(span, lexer),
                            format!("Array index does not apply to type {}",
                                    unparse_type(types, idx_typ, stringtab)))))?
            }

            let num_dims = types.get_num_dimensions(idx_typ).unwrap();
            if indices.len() < num_dims {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            format!("fewer array indices than dimensions, array has {} dimensions but using {} indices",
                                    num_dims, indices.len()))))
            } else if indices.len() > num_dims {
                Err(singleton_error(
                        ErrorMessage::SemanticError(
                            span_to_loc(span, lexer),
                            format!("Too many array indices, array has {} dimensions but using {} indices",
                                    num_dims, indices.len()))))
            } else {
                idx.push(Index::Array(indices));
                Ok(((var, var_typ),
                    (types.get_element_type(idx_typ).unwrap(), idx)))
            }
        },
    }
}

fn process_expr_as_constant(expr : lang_y::Expr,
                            lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
                            stringtab : &mut StringTable, env : &mut Env<usize, Entity>,
                            types : &mut TypeSolver)
    -> Result<Constant, ErrorMessages> {

    match expr {
        lang_y::Expr::Variable { span, name } => {
            if name.len() != 1 {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "packages".to_string())))?
            }
            let nm = intern_package_name(&name, lexer, stringtab)[0];

            match env.lookup(&nm) {
                Some(Entity::Variable { .. }) => {
                    panic!("Constant should not be evaluated in an environment with variables")
                },
                Some(Entity::DynConst { .. }) => {
                    panic!("Constant should not be evaluated in an environment with dynamic constants")
                },
                Some(Entity::Constant { value }) => {
                    Ok(value.clone())
                },
                Some(Entity::Function { .. }) =>
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("{} is a function, expected a value",
                                        stringtab.lookup_id(nm).unwrap())))),
                Some(Entity::Type { .. }) =>
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("{} is a type, expected a value",
                                        stringtab.lookup_id(nm).unwrap())))),
                None =>
                    Err(singleton_error(
                            ErrorMessage::UndefinedVariable(
                                span_to_loc(span, lexer),
                                stringtab.lookup_id(nm).unwrap())))
            }
        },
        lang_y::Expr::Field { span, lhs, rhs } => {
            let field_name = intern_id(&rhs, lexer, stringtab);
            let (lit, typ) = process_expr_as_constant(*lhs, lexer, stringtab, env, types)?;

            match types.get_field(typ, field_name) {
                None => Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("Type {} does not possess field {}",
                                        unparse_type(types, typ, stringtab),
                                        stringtab.lookup_id(field_name).unwrap())))),
                Some((field_idx, _)) => {
                    let Literal::Tuple(fields) = lit else { panic!("Wrong constant constructor") };
                    Ok(fields[field_idx].clone())
                },
            }
        },
        lang_y::Expr::NumField { span, lhs, rhs } => {
            let (lit, typ) = process_expr_as_constant(*lhs, lexer, stringtab, env, types)?;

            let num = lexer.span_str(rhs)[1..].parse::<usize>()
                                              .expect("From lexical analysis");

            match types.get_index(typ, num) {
                None => Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("Type {} does not possess index {}",
                                        unparse_type(types, typ, stringtab),
                                        num)))),
                Some(_) => {
                    let Literal::Tuple(fields) = lit else { panic!("Wrong constant constructor") };
                    Ok(fields[num].clone())
                },
            }
        },
        lang_y::Expr::ArrIndex { span, .. } => {
            Err(singleton_error(
                    ErrorMessage::SemanticError(
                        span_to_loc(span, lexer),
                        format!("Arrays are not allowed in constants"))))
        },
        lang_y::Expr::Tuple { span : _, mut exprs } => {
            if exprs.len() == 1 {
                return process_expr_as_constant(exprs.pop().unwrap(), lexer, stringtab, env, types);
            }
            if exprs.len() == 0 {
                return Ok((Literal::Unit, types.new_primitive(types::Primitive::Unit)));
            }

            let mut vals = vec![];
            let mut typs = vec![];
            let mut errors = LinkedList::new();

            for exp in exprs {
                match process_expr_as_constant(exp, lexer, stringtab, env, types) {
                    Err(mut errs) => errors.append(&mut errs),
                    Ok((lit, typ)) => {
                        typs.push(typ);
                        vals.push((lit, typ));
                    },
                }
            }

            if !errors.is_empty() {
                Err(errors)
            } else {
                Ok((Literal::Tuple(vals), types.new_tuple(typs)))
            }
        },
        lang_y::Expr::Struct { span, name, ty_args, exprs } => {
            if name.len() != 1 {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "packages".to_string())))?
            }

            let struct_nm = intern_package_name(&name, lexer, stringtab)[0];
            match env.lookup(&struct_nm) {
                Some(Entity::Variable { .. }) => {
                    Err(singleton_error(
                            ErrorMessage::KindError(
                                span_to_loc(span, lexer),
                                "struct name".to_string(),
                                "variable".to_string())))
                },
                Some(Entity::DynConst { .. }) => {
                    Err(singleton_error(
                            ErrorMessage::KindError(
                                span_to_loc(span, lexer),
                                "struct name".to_string(),
                                "dynamic constant".to_string())))
                },
                Some(Entity::Constant { .. }) => {
                    Err(singleton_error(
                            ErrorMessage::KindError(
                                span_to_loc(span, lexer),
                                "struct name".to_string(),
                                "constant".to_string())))
                },
                Some(Entity::Function { .. }) => {
                    Err(singleton_error(
                            ErrorMessage::KindError(
                                span_to_loc(span, lexer),
                                "struct name".to_string(),
                                "function".to_string())))
                },
                None => {
                    Err(singleton_error(
                            ErrorMessage::UndefinedVariable(
                                span_to_loc(span, lexer),
                                stringtab.lookup_id(struct_nm).unwrap())))
                },
                Some(Entity::Type { type_args : kinds, value : typ }) => {
                    if !types.is_struct(*typ) {
                        Err(singleton_error(
                                ErrorMessage::KindError(
                                    span_to_loc(span, lexer),
                                    "struct name".to_string(),
                                    "non-struct type".to_string())))?
                    }
                    if kinds.len() != ty_args.len() {
                        Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(span, lexer),
                                    format!("Expected {} type arguments, provided {}",
                                            kinds.len(), ty_args.len()))))?
                    }

                    // Verify that the type arguments we are provided are correct and collect the
                    // type variable and dynamic constant substitutions
                    let mut type_vars = vec![];
                    let mut dyn_consts = vec![];
                    let mut errors = LinkedList::new();

                    for (arg, kind) in ty_args.into_iter().zip(kinds.iter()) {
                        let arg_span = arg.span();
                        match kind {
                            lang_y::Kind::USize => {
                                match process_type_expr_as_expr(
                                        arg, lexer, stringtab, env, types) {
                                    Err(mut errs) => errors.append(&mut errs),
                                    Ok(val) => dyn_consts.push(val),
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
                                                    unparse_type(types, typ, stringtab)));
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
                                                        unparse_type(types, typ, stringtab)));
                                            }
                                        },
                                    }
                            },
                        }
                    }

                    if !errors.is_empty() { return Err(errors); }

                    let struct_type =
                        if type_vars.len() == 0 && dyn_consts.len() == 0 {
                            *typ
                        } else {
                            types.instantiate(*typ, &type_vars, &dyn_consts)
                        };

                    // Check each field and construct the appropriate tuple
                    // Note that fields that are omitted will be initialized with their type's
                    // default value
                    let num_fields = types.get_num_struct_fields(struct_type).unwrap();

                    // Values for the fields, in order
                    let mut values : Vec<Option<Constant>> = vec![None; num_fields];

                    for (field_name, expr) in exprs {
                        let field_nm = intern_id(&field_name, lexer, stringtab);
                        let expr_span = expr.span();

                        match types.get_field(struct_type, field_nm) {
                            None => {
                                errors.push_back(
                                    ErrorMessage::SemanticError(
                                        span_to_loc(field_name, lexer),
                                        format!("Struct {} does not have field {}",
                                                unparse_type(types, struct_type, stringtab),
                                                stringtab.lookup_id(field_nm).unwrap())));
                            },
                            Some((idx, field_typ)) => {
                                if values[idx].is_some() {
                                    errors.push_back(
                                        ErrorMessage::SemanticError(
                                            span_to_loc(field_name, lexer),
                                            format!("Field {} defined multiple times",
                                                    stringtab.lookup_id(field_nm).unwrap())));
                                } else {
                                    match process_expr_as_constant(expr, lexer, stringtab, env, types) {
                                        Err(mut errs) => errors.append(&mut errs),
                                        Ok((lit, typ)) => {
                                            if !types.equal(field_typ, typ) {
                                                // Set the value at this index even though there's
                                                // an error so that we also report if the field is
                                                // defined multiple times
                                                values[idx] 
                                                    = Some((Literal::Unit, 
                                                            types.new_primitive(types::Primitive::Unit)));
                                                errors.push_back(
                                                    ErrorMessage::TypeError(
                                                        span_to_loc(expr_span, lexer),
                                                        unparse_type(types, field_typ, stringtab),
                                                        unparse_type(types, typ, stringtab)));
                                            } else {
                                                values[idx] = Some((lit, typ));
                                            }
                                        },
                                    }
                                }
                            },
                        }
                    }

                    if !errors.is_empty() { return Err(errors); }

                    if values.iter().any(|n| n.is_none()) {
                        Err(singleton_error(
                                ErrorMessage::NotImplemented(
                                    span_to_loc(span, lexer),
                                    "constant struct with missing fields".to_string())))?
                    }

                    // Construct the list of field values, filling in zero values as needed
                    let filled_fields
                        = values.into_iter().map(|t| t.unwrap()).collect::<Vec<_>>();

                    Ok((Literal::Tuple(filled_fields), struct_type))
                },
            }
        },
        lang_y::Expr::BoolLit { span : _, value } => {
            let bool_typ = types.new_primitive(types::Primitive::Bool);
            Ok((Literal::Bool(value), bool_typ))
        },
        lang_y::Expr::IntLit { span, base } => {
            let res = u64::from_str_radix(lexer.span_str(span), base.base());
            assert!(res.is_ok(), "Internal Error: Int literal is not an integer");

            let num_typ = types.new_number();
            Ok((Literal::Integer(res.unwrap()), num_typ))
        },
        lang_y::Expr::FloatLit { span } => {
            let res = lexer.span_str(span).parse::<f64>();
            assert!(res.is_ok(), "Internal Error: Float literal is not a float");

            let float_typ = types.new_float();
            Ok((Literal::Float(res.unwrap()), float_typ))
        },
        lang_y::Expr::UnaryExpr { span, op, expr } => {
            let (expr_lit, expr_typ) 
                = process_expr_as_constant(*expr, lexer, stringtab, env, types)?;

            match op {
                lang_y::UnaryOp::Negation => {
                    if !types.is_number(expr_typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(span, lexer),
                                    "number".to_string(),
                                    unparse_type(types, expr_typ, stringtab))))
                    } else {
                        Ok((match expr_lit {
                                Literal::Integer(i) => Literal::Integer(- (i as i64) as u64),
                                Literal::Float(f)   => Literal::Float(- f),
                                _ => panic!("Incorrect literal constructor"),
                            }, expr_typ))
                    }
                },
                lang_y::UnaryOp::BitwiseNot => {
                    if !types.is_integer(expr_typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(span, lexer),
                                    "integer".to_string(),
                                    unparse_type(types, expr_typ, stringtab))))
                    } else {
                        let Literal::Integer(i) = expr_lit 
                            else { panic!("Incorrect literal constructor"); };
                        Ok((Literal::Integer(! i), expr_typ))
                    }
                },
                lang_y::UnaryOp::LogicalNot => {
                    if !types.is_bool(expr_typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(span, lexer),
                                    "bool".to_string(),
                                    unparse_type(types, expr_typ, stringtab))))
                    } else {
                        let Literal::Bool(b) = expr_lit
                            else { panic!("Incorrect literal constructor"); };
                        Ok((Literal::Bool(! b), expr_typ))
                    }
                },
            }
        },
        lang_y::Expr::BinaryExpr { span : _, op, lhs, rhs } => {
            let lhs_span = lhs.span();
            let rhs_span = rhs.span();

            let lhs_res = process_expr_as_constant(*lhs, lexer, stringtab, env, types);
            let rhs_res = process_expr_as_constant(*rhs, lexer, stringtab, env, types);

            let ((lhs_lit, lhs_typ), (rhs_lit, rhs_typ)) 
                = append_errors2(lhs_res, rhs_res)?;

            // First, type-check
            match op {
                // Equality and inequality work on any types
                lang_y::BinaryOp::Eq | lang_y::BinaryOp::Neq => {
                    if !types.equal(lhs_typ, rhs_typ) {
                        return Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(rhs_span, lexer),
                                    unparse_type(types, lhs_typ, stringtab),
                                    unparse_type(types, rhs_typ, stringtab))));
                    }
                },
                // These work on any numbers
                lang_y::BinaryOp::Add   | lang_y::BinaryOp::Sub | lang_y::BinaryOp::Mul
                | lang_y::BinaryOp::Div | lang_y::BinaryOp::Lt  | lang_y::BinaryOp::Le
                | lang_y::BinaryOp::Gt  | lang_y::BinaryOp::Ge => {
                    let mut errors = LinkedList::new();
                    let lhs_number = types.is_number(lhs_typ);
                    let rhs_number = types.is_number(rhs_typ);
                    let equal = types.equal(lhs_typ, rhs_typ);

                    if lhs_number && !equal {
                        errors.push_back(
                            ErrorMessage::TypeError(
                                span_to_loc(rhs_span, lexer),
                                unparse_type(types, lhs_typ, stringtab),
                                unparse_type(types, rhs_typ, stringtab)));
                    } else if rhs_number && !equal {
                        errors.push_back(
                            ErrorMessage::TypeError(
                                span_to_loc(lhs_span, lexer),
                                unparse_type(types, rhs_typ, stringtab),
                                unparse_type(types, lhs_typ, stringtab)));
                    } else {
                        // The types are equal or both are not numbers
                        if !lhs_number {
                            errors.push_back(
                                ErrorMessage::TypeError(
                                    span_to_loc(lhs_span, lexer),
                                    "number".to_string(),
                                    unparse_type(types, lhs_typ, stringtab)));
                        }
                        if !rhs_number {
                            errors.push_back(
                                ErrorMessage::TypeError(
                                    span_to_loc(rhs_span, lexer),
                                    "number".to_string(),
                                    unparse_type(types, rhs_typ, stringtab)));
                        }
                    }

                    if !errors.is_empty() { return Err(errors); }
                },
                lang_y::BinaryOp::Mod   | lang_y::BinaryOp::BitAnd | lang_y::BinaryOp::BitOr
                | lang_y::BinaryOp::Xor | lang_y::BinaryOp::LShift | lang_y::BinaryOp::RShift
                => {
                    let mut errors = LinkedList::new();
                    let lhs_integer = types.is_integer(lhs_typ);
                    let rhs_integer = types.is_integer(rhs_typ);
                    let equal = types.equal(lhs_typ, rhs_typ);

                    if lhs_integer && !equal {
                        errors.push_back(
                            ErrorMessage::TypeError(
                                span_to_loc(rhs_span, lexer),
                                unparse_type(types, lhs_typ, stringtab),
                                unparse_type(types, rhs_typ, stringtab)));
                    } else if rhs_integer && !equal {
                        errors.push_back(
                            ErrorMessage::TypeError(
                                span_to_loc(lhs_span, lexer),
                                unparse_type(types, rhs_typ, stringtab),
                                unparse_type(types, lhs_typ, stringtab)));
                    } else {
                        // The types are equal or both are not integers
                        if !lhs_integer {
                            errors.push_back(
                                ErrorMessage::TypeError(
                                    span_to_loc(lhs_span, lexer),
                                    "integer".to_string(),
                                    unparse_type(types, lhs_typ, stringtab)));
                        }
                        if !rhs_integer {
                            errors.push_back(
                                ErrorMessage::TypeError(
                                    span_to_loc(rhs_span, lexer),
                                    "integer".to_string(),
                                    unparse_type(types, rhs_typ, stringtab)));
                        }
                    }

                    if !errors.is_empty() { return Err(errors); }
                },
                lang_y::BinaryOp::LogAnd | lang_y::BinaryOp::LogOr => {
                    let mut errors = LinkedList::new();
                    let lhs_bool = types.is_bool(lhs_typ);
                    let rhs_bool = types.is_bool(rhs_typ);
                    let equal = types.equal(lhs_typ, rhs_typ);

                    if lhs_bool && !equal {
                        errors.push_back(
                            ErrorMessage::TypeError(
                                span_to_loc(rhs_span, lexer),
                                unparse_type(types, lhs_typ, stringtab),
                                unparse_type(types, rhs_typ, stringtab)));
                    } else if rhs_bool && !equal {
                        errors.push_back(
                            ErrorMessage::TypeError(
                                span_to_loc(lhs_span, lexer),
                                unparse_type(types, rhs_typ, stringtab),
                                unparse_type(types, lhs_typ, stringtab)));
                    } else {
                        // The types are equal or both are not bools
                        if !lhs_bool {
                            errors.push_back(
                                ErrorMessage::TypeError(
                                    span_to_loc(lhs_span, lexer),
                                    "bool".to_string(),
                                    unparse_type(types, lhs_typ, stringtab)));
                        }
                        if !rhs_bool {
                            errors.push_back(
                                ErrorMessage::TypeError(
                                    span_to_loc(rhs_span, lexer),
                                    "bool".to_string(),
                                    unparse_type(types, rhs_typ, stringtab)));
                        }
                    }

                    if !errors.is_empty() { return Err(errors); }
                },
            };

            match op {
                lang_y::BinaryOp::Add    => {
                    match (lhs_lit, rhs_lit) {
                        (Literal::Integer(i), Literal::Integer(j))
                            => Ok((Literal::Integer(i + j), lhs_typ)),
                        (Literal::Integer(i), Literal::Float(j))
                            => Ok((Literal::Float((i as f64) + j), lhs_typ)),
                        (Literal::Float(i), Literal::Integer(j))
                            => Ok((Literal::Float(i + (j as f64)), lhs_typ)),
                        (Literal::Float(i), Literal::Float(j))
                            => Ok((Literal::Float(i + j), lhs_typ)),
                        _ => panic!("Incorrect literal constructor"),
                    }
                },
                lang_y::BinaryOp::Sub    => {
                    match (lhs_lit, rhs_lit) {
                        (Literal::Integer(i), Literal::Integer(j))
                            => Ok((Literal::Integer((i as i64 - j as i64) as u64), lhs_typ)),
                        (Literal::Integer(i), Literal::Float(j))
                            => Ok((Literal::Float((i as f64) - j), lhs_typ)),
                        (Literal::Float(i), Literal::Integer(j))
                            => Ok((Literal::Float(i - (j as f64)), lhs_typ)),
                        (Literal::Float(i), Literal::Float(j))
                            => Ok((Literal::Float(i - j), lhs_typ)),
                        _ => panic!("Incorrect literal constructor"),
                    }
                },
                lang_y::BinaryOp::Mul    => {
                    match (lhs_lit, rhs_lit) {
                        (Literal::Integer(i), Literal::Integer(j))
                            => Ok((Literal::Integer(i * j), lhs_typ)),
                        (Literal::Integer(i), Literal::Float(j))
                            => Ok((Literal::Float((i as f64) * j), lhs_typ)),
                        (Literal::Float(i), Literal::Integer(j))
                            => Ok((Literal::Float(i * (j as f64)), lhs_typ)),
                        (Literal::Float(i), Literal::Float(j))
                            => Ok((Literal::Float(i * j), lhs_typ)),
                        _ => panic!("Incorrect literal constructor"),
                    }
                },
                lang_y::BinaryOp::Div    => {
                    match (lhs_lit, rhs_lit) {
                        (Literal::Integer(i), Literal::Integer(j))
                            => Ok((Literal::Integer(i / j), lhs_typ)),
                        (Literal::Integer(i), Literal::Float(j))
                            => Ok((Literal::Float((i as f64) / j), lhs_typ)),
                        (Literal::Float(i), Literal::Integer(j))
                            => Ok((Literal::Float(i / (j as f64)), lhs_typ)),
                        (Literal::Float(i), Literal::Float(j))
                            => Ok((Literal::Float(i / j), lhs_typ)),
                        _ => panic!("Incorrect literal constructor"),
                    }
                },
                lang_y::BinaryOp::Mod    => {
                    match (lhs_lit, rhs_lit) {
                        (Literal::Integer(i), Literal::Integer(j))
                            => Ok((Literal::Integer(i % j), lhs_typ)),
                        _ => panic!("Incorrect literal constructor"),
                    }
                },
                lang_y::BinaryOp::BitAnd => {
                    match (lhs_lit, rhs_lit) {
                        (Literal::Integer(i), Literal::Integer(j))
                            => Ok((Literal::Integer(i & j), lhs_typ)),
                        _ => panic!("Incorrect literal constructor"),
                    }
                },
                lang_y::BinaryOp::BitOr  => {
                    match (lhs_lit, rhs_lit) {
                        (Literal::Integer(i), Literal::Integer(j))
                            => Ok((Literal::Integer(i | j), lhs_typ)),
                        _ => panic!("Incorrect literal constructor"),
                    }
                },
                lang_y::BinaryOp::Xor    => {
                    match (lhs_lit, rhs_lit) {
                        (Literal::Integer(i), Literal::Integer(j))
                            => Ok((Literal::Integer(i ^ j), lhs_typ)),
                        _ => panic!("Incorrect literal constructor"),
                    }
                },
                lang_y::BinaryOp::Lt     => {
                    let bool_typ = types.new_primitive(types::Primitive::Bool);
                    match (lhs_lit, rhs_lit) {
                        (Literal::Integer(i), Literal::Integer(j))
                            => Ok((Literal::Bool(i < j), bool_typ)),
                        (Literal::Integer(i), Literal::Float(j))
                            => Ok((Literal::Bool(((i as f64)) < j), bool_typ)),
                        (Literal::Float(i), Literal::Integer(j))
                            => Ok((Literal::Bool(i < (j as f64)), bool_typ)),
                        (Literal::Float(i), Literal::Float(j))
                            => Ok((Literal::Bool(i < j), bool_typ)),
                        _ => panic!("Incorrect literal constructor"),
                    }
                },
                lang_y::BinaryOp::Le     => {
                    let bool_typ = types.new_primitive(types::Primitive::Bool);
                    match (lhs_lit, rhs_lit) {
                        (Literal::Integer(i), Literal::Integer(j))
                            => Ok((Literal::Bool(i <= j), bool_typ)),
                        (Literal::Integer(i), Literal::Float(j))
                            => Ok((Literal::Bool((i as f64) <= j), bool_typ)),
                        (Literal::Float(i), Literal::Integer(j))
                            => Ok((Literal::Bool(i <= (j as f64)), bool_typ)),
                        (Literal::Float(i), Literal::Float(j))
                            => Ok((Literal::Bool(i <= j), bool_typ)),
                        _ => panic!("Incorrect literal constructor"),
                    }
                },
                lang_y::BinaryOp::Gt     => {
                    let bool_typ = types.new_primitive(types::Primitive::Bool);
                    match (lhs_lit, rhs_lit) {
                        (Literal::Integer(i), Literal::Integer(j))
                            => Ok((Literal::Bool(i > j), bool_typ)),
                        (Literal::Integer(i), Literal::Float(j))
                            => Ok((Literal::Bool((i as f64) > j), bool_typ)),
                        (Literal::Float(i), Literal::Integer(j))
                            => Ok((Literal::Bool(i > (j as f64)), bool_typ)),
                        (Literal::Float(i), Literal::Float(j))
                            => Ok((Literal::Bool(i > j), bool_typ)),
                        _ => panic!("Incorrect literal constructor"),
                    }
                },
                lang_y::BinaryOp::Ge     => {
                    let bool_typ = types.new_primitive(types::Primitive::Bool);
                    match (lhs_lit, rhs_lit) {
                        (Literal::Integer(i), Literal::Integer(j))
                            => Ok((Literal::Bool(i >= j), bool_typ)),
                        (Literal::Integer(i), Literal::Float(j))
                            => Ok((Literal::Bool((i as f64) >= j), bool_typ)),
                        (Literal::Float(i), Literal::Integer(j))
                            => Ok((Literal::Bool(i >= (j as f64)), bool_typ)),
                        (Literal::Float(i), Literal::Float(j))
                            => Ok((Literal::Bool(i >= j), bool_typ)),
                        _ => panic!("Incorrect literal constructor"),
                    }
                },
                lang_y::BinaryOp::Eq     => {
                    let bool_typ = types.new_primitive(types::Primitive::Bool);
                    match (lhs_lit, rhs_lit) {
                        (Literal::Integer(i), Literal::Integer(j))
                            => Ok((Literal::Bool(i == j), bool_typ)),
                        (Literal::Integer(i), Literal::Float(j))
                            => Ok((Literal::Bool((i as f64) == j), bool_typ)),
                        (Literal::Float(i), Literal::Integer(j))
                            => Ok((Literal::Bool(i == (j as f64)), bool_typ)),
                        (Literal::Float(i), Literal::Float(j))
                            => Ok((Literal::Bool(i == j), bool_typ)),
                        (lhs_lit, rhs_lit)
                            => Ok((Literal::Bool(lhs_lit == rhs_lit), bool_typ)),
                    }
                },
                lang_y::BinaryOp::Neq    => {
                    let bool_typ = types.new_primitive(types::Primitive::Bool);
                    match (lhs_lit, rhs_lit) {
                        (Literal::Integer(i), Literal::Integer(j))
                            => Ok((Literal::Bool(i != j), bool_typ)),
                        (Literal::Integer(i), Literal::Float(j))
                            => Ok((Literal::Bool((i as f64) != j), bool_typ)),
                        (Literal::Float(i), Literal::Integer(j))
                            => Ok((Literal::Bool(i != (j as f64)), bool_typ)),
                        (Literal::Float(i), Literal::Float(j))
                            => Ok((Literal::Bool(i != j), bool_typ)),
                        (lhs_lit, rhs_lit)
                            => Ok((Literal::Bool(lhs_lit != rhs_lit), bool_typ)),
                    }
                },
                lang_y::BinaryOp::LShift => {
                    match (lhs_lit, rhs_lit) {
                        (Literal::Integer(i), Literal::Integer(j))
                            => Ok((Literal::Integer(i << j), lhs_typ)),
                        _ => panic!("Incorrect literal constructor"),
                    }
                },
                lang_y::BinaryOp::RShift => {
                    match (lhs_lit, rhs_lit) {
                        (Literal::Integer(i), Literal::Integer(j))
                            => Ok((Literal::Integer(i >> j), lhs_typ)),
                        _ => panic!("Incorrect literal constructor"),
                    }
                },
                lang_y::BinaryOp::LogAnd => {
                    match (lhs_lit, rhs_lit) {
                        (Literal::Bool(i), Literal::Bool(j))
                            => Ok((Literal::Bool(i && j), lhs_typ)),
                        _ => panic!("Incorrect literal constructor"),
                    }
                },
                lang_y::BinaryOp::LogOr  => {
                    match (lhs_lit, rhs_lit) {
                        (Literal::Bool(i), Literal::Bool(j))
                            => Ok((Literal::Bool(i || j), lhs_typ)),
                        _ => panic!("Incorrect literal constructor"),
                    }
                },
            }
        },
        lang_y::Expr::CastExpr { span, expr, typ } => {
            // Cast between numeric types
            let expr_res = process_expr_as_constant(*expr, lexer, stringtab, env, types);
            let type_res = process_type(typ, lexer, stringtab, env, types);

            let ((expr_lit, expr_typ), to_typ) = append_errors2(expr_res, type_res)?;

            if !types.is_number(expr_typ) || !types.is_number(to_typ) {
                Err(singleton_error(
                        ErrorMessage::SemanticError(
                            span_to_loc(span, lexer),
                            format!("Can only cast between numeric types, cannot cast {} to {}",
                                    unparse_type(types, expr_typ, stringtab),
                                    unparse_type(types, to_typ, stringtab)))))
            } else {
                if types.is_integer(to_typ) {
                    Ok((match expr_lit {
                         Literal::Integer(i) => Literal::Integer(i),
                         Literal::Float(f)   => Literal::Integer(f as u64),
                         _ => panic!("Incorrect literal constructor"),
                       }, to_typ))
                } else {
                    Ok((match expr_lit {
                         Literal::Integer(i) => Literal::Float(i as f64),
                         Literal::Float(f)   => Literal::Float(f),
                         _ => panic!("Incorrect literal constructor"),
                       }, to_typ))
                }
            }
        },
        lang_y::Expr::CondExpr { span, cond, thn, els } => {
            let cond_span = cond.span();

            let cond_res = process_expr_as_constant(*cond, lexer, stringtab, env, types);
            let thn_res  = process_expr_as_constant(*thn,  lexer, stringtab, env, types);
            let els_res  = process_expr_as_constant(*els,  lexer, stringtab, env, types);

            let ((cond_lit, cond_typ), (thn_lit, thn_typ), (els_lit, els_typ))
                = append_errors3(cond_res, thn_res, els_res)?;

            let mut errors = LinkedList::new();

            if !types.is_bool(cond_typ) {
                errors.push_back(
                    ErrorMessage::TypeError(
                        span_to_loc(cond_span, lexer),
                        "bool".to_string(),
                        unparse_type(types, cond_typ, stringtab)));
            }
            if !types.equal(thn_typ, els_typ) {
                errors.push_back(
                    ErrorMessage::SemanticError(
                        span_to_loc(span, lexer),
                        format!("Types of conditional branches do not match, have {} and {}",
                                unparse_type(types, thn_typ, stringtab),
                                unparse_type(types, els_typ, stringtab))));
            }

            if !errors.is_empty() {
                Err(errors)
            } else {
                let Literal::Bool(condition) = cond_lit else { panic!("Incorrect literal constructor"); };
                if condition { Ok((thn_lit, thn_typ)) } else { Ok((els_lit, els_typ)) }
            }
        },
        lang_y::Expr::CallExpr { span, name, ty_args, args } => {
            // While calls cannot be evaluated as constants, enum values can be, so we need to
            // distinguish whether this is actually a call or the construction of some enum value
            if name.len() > 2 {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "packages".to_string())))?
            }
            
            let nm = intern_package_name(&name, lexer, stringtab);

            match env.lookup(&nm[0]) {
                Some(Entity::Variable { .. })   | Some(Entity::DynConst { .. })
                | Some(Entity::Constant { .. }) | Some(Entity::Function { .. })
                | None if name.len() != 1 => {
                    Err(singleton_error(
                            ErrorMessage::NotImplemented(
                                span_to_loc(span, lexer),
                                "packages".to_string())))
                },
                None =>
                    Err(singleton_error(
                            ErrorMessage::UndefinedVariable(
                                span_to_loc(name[0], lexer),
                                stringtab.lookup_id(nm[0]).unwrap()))),
                Some(Entity::Variable { .. }) =>
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(name[0], lexer),
                                format!("{} is a variable, expected a function or union constructor",
                                        stringtab.lookup_id(nm[0]).unwrap())))),
                Some(Entity::DynConst { .. }) =>
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(name[0], lexer),
                                format!("{} is a dynamic constant, expected a function or union constructor",
                                        stringtab.lookup_id(nm[0]).unwrap())))),
                Some(Entity::Constant { .. }) =>
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(name[0], lexer),
                                format!("{} is a constant, expected a function or union constructor",
                                        stringtab.lookup_id(nm[0]).unwrap())))),
                Some(Entity::Function { index : function, type_args : kinds, 
                                        args : func_args, return_type }) =>
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(name[0], lexer),
                                format!("Function calls cannot be evaluated as a constant")))),
                Some(Entity::Type { type_args : kinds, value : typ }) => {
                    if !types.is_union(*typ) {
                        if name.len() != 1 {
                            Err(singleton_error(
                                    ErrorMessage::NotImplemented(
                                        span_to_loc(span, lexer),
                                        "packages".to_string())))?
                        } else {
                            Err(singleton_error(
                                    ErrorMessage::SemanticError(
                                        span_to_loc(name[0], lexer),
                                        format!("{} is a type, expected a function or union constructor",
                                                stringtab.lookup_id(nm[0]).unwrap()))))?
                        }
                    }
                    if name.len() != 2 {
                        Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(name[0], lexer),
                                    format!("Expected constructor name"))))?
                    }

                    if types.get_constructor_info(*typ, nm[1]).is_none() {
                        Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(name[1], lexer),
                                    format!("{} is not a constructor of type {}",
                                            stringtab.lookup_id(nm[1]).unwrap(),
                                            unparse_type(types, *typ, stringtab)))))?
                    }
                    
                    // Now, we know that we are constructing some union, we need to verify that
                    // the type arguments are appropriate
                    if kinds.len() != ty_args.len() {
                        Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(span, lexer),
                                    format!("Expected {} type arguments, provided {}",
                                            kinds.len(), ty_args.len()))))?
                    }
                    
                    let mut type_vars = vec![];
                    let mut dyn_consts = vec![];
                    let mut errors = LinkedList::new();

                    for (arg, kind) in ty_args.into_iter().zip(kinds.iter()) {
                        let arg_span = arg.span();
                        match kind {
                            lang_y::Kind::USize => {
                                match process_type_expr_as_expr(
                                        arg, lexer, stringtab, env, types) {
                                    Err(mut errs) => errors.append(&mut errs),
                                    Ok(val) => dyn_consts.push(val),
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
                                                    unparse_type(types, typ, stringtab)));
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
                                                        unparse_type(types, typ, stringtab)));
                                            }
                                        },
                                    }
                            },
                        }
                    }

                    if !errors.is_empty() { return Err(errors); }
                    
                    let union_type =
                        if type_vars.len() == 0 && dyn_consts.len() == 0 {
                            *typ
                        } else {
                            types.instantiate(*typ, &type_vars, &dyn_consts)
                        };
                    let Some((constr_idx, constr_typ))
                        = types.get_constructor_info(union_type, nm[1])
                        else { panic!("From above"); };

                    // Now, process the arguments to ensure they has the type needed by this
                    // constructor
                    // To do this, since unions take a single argument, we process the arguments as
                    // a single tuple, reporting an error if inout is used anywhere
                    for (is_inout, arg) in args.iter() {
                        if *is_inout {
                            Err(singleton_error(
                                    ErrorMessage::SemanticError(
                                        span_to_loc(arg.span(), lexer),
                                        format!("Union constructors cannot be marked inout"))))?
                        }
                    }

                    let (body_lit, body_typ)
                        = process_expr_as_constant(
                                lang_y::Expr::Tuple {
                                    span : span,
                                    exprs : args.into_iter().map(|(_, a)| a).collect::<Vec<_>>() },
                                lexer, stringtab, env, types)?;

                    if !types.equal(constr_typ, body_typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(span, lexer),
                                    unparse_type(types, constr_typ, stringtab),
                                    unparse_type(types, body_typ, stringtab))))
                    } else {
                        Ok((Literal::Sum(constr_idx, Box::new((body_lit, body_typ))),
                            body_typ))
                    }
                },
            }
        },
    }
}

fn process_expr(expr : lang_y::Expr, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
                stringtab : &mut StringTable, env : &mut Env<usize, Entity>,
                types : &mut TypeSolver)
    -> Result<Expr, ErrorMessages> {

    match expr {
        lang_y::Expr::Variable { span, name } => {
            if name.len() != 1 {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "packages".to_string())))?
            }
            let nm = intern_package_name(&name, lexer, stringtab)[0];

            match env.lookup(&nm) {
                Some(Entity::Variable { variable, typ, .. }) => {
                    Ok(Expr::Variable { var : *variable, typ : *typ })
                },
                Some(Entity::DynConst { value }) => {
                    let typ = types.new_primitive(types::Primitive::U64);
                    Ok(Expr::DynConst { idx : *value, typ : typ })
                },
                Some(Entity::Constant { value : (lit, typ) }) => {
                    Ok(Expr::Constant { val : (lit.clone(), *typ), typ : *typ })
                },
                Some(Entity::Function { .. }) =>
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("{} is a function, expected a value",
                                        stringtab.lookup_id(nm).unwrap())))),
                Some(Entity::Type { .. }) =>
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("{} is a type, expected a value",
                                        stringtab.lookup_id(nm).unwrap())))),
                None =>
                    Err(singleton_error(
                            ErrorMessage::UndefinedVariable(
                                span_to_loc(span, lexer),
                                stringtab.lookup_id(nm).unwrap())))
            }
        },
        lang_y::Expr::Field { span, lhs, rhs } => {
            let field_name = intern_id(&rhs, lexer, stringtab);
            let exp = process_expr(*lhs, lexer, stringtab, env, types)?;
            let exp_typ = exp.get_type();

            match types.get_field(exp_typ, field_name) {
                None => Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("Type {} does not possess field {}",
                                        unparse_type(types, exp_typ, stringtab),
                                        stringtab.lookup_id(field_name).unwrap())))),
                Some((field_idx, field_type)) =>
                    Ok(Expr::Read {
                        index : vec![Index::Field(field_idx)],
                        val   : Box::new(exp),
                        typ   : field_type }),
            }
        },
        lang_y::Expr::NumField { span, lhs, rhs } => {
            let exp = process_expr(*lhs, lexer, stringtab, env, types)?;
            let exp_typ = exp.get_type();

            let num = lexer.span_str(rhs)[1..].parse::<usize>()
                                              .expect("From lexical analysis");

            match types.get_index(exp_typ, num) {
                None => Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(span, lexer),
                                format!("Type {} does not possess index {}",
                                        unparse_type(types, exp_typ, stringtab),
                                        num)))),
                Some(field_type) =>
                    Ok(Expr::Read {
                        index : vec![Index::Field(num)],
                        val   : Box::new(exp),
                        typ   : field_type }),
            }
        },
        lang_y::Expr::ArrIndex { span, lhs, index } => {
            let exp = process_expr(*lhs, lexer, stringtab, env, types)?;
            let exp_typ = exp.get_type();

            let mut indices = vec![];
            let mut errors = LinkedList::new();
            for idx in index {
                let idx_span = idx.span();
                match process_expr(idx, lexer, stringtab, env, types) {
                    Err(mut errs) => errors.append(&mut errs),
                    Ok(exp) => {
                        let typ = exp.get_type();
                        if !types.is_u64(typ) {
                            errors.push_back(
                                ErrorMessage::TypeError(
                                    span_to_loc(idx_span, lexer),
                                    "usize".to_string(),
                                    unparse_type(types, typ, stringtab)));
                        } else {
                            indices.push(exp);
                        }
                    },
                }
            }

            if !errors.is_empty() {
                Err(errors)?
            }

            if !types.is_array(exp_typ) {
                Err(singleton_error(
                        ErrorMessage::SemanticError(
                            span_to_loc(span, lexer),
                            format!("Array index does not apply to type {}",
                                    unparse_type(types, exp_typ, stringtab)))))?
            }

            let num_dims = types.get_num_dimensions(exp_typ).unwrap();
            if indices.len() < num_dims {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            format!("fewer array indices than dimensions, array has {} dimensions but using {} indices",
                                    num_dims, indices.len()))))
            } else if indices.len() > num_dims {
                Err(singleton_error(
                        ErrorMessage::SemanticError(
                            span_to_loc(span, lexer),
                            format!("Too many array indices, array has {} dimensions but using {} indices",
                                    num_dims, indices.len()))))
            } else {
                Ok(Expr::Read {
                    index : vec![Index::Array(indices)],
                    val   : Box::new(exp),
                    typ   : types.get_element_type(exp_typ).unwrap() })
            }
        },
        lang_y::Expr::Tuple { span : _, mut exprs } => {
            if exprs.len() == 1 {
                return process_expr(exprs.pop().unwrap(), lexer, stringtab, env, types);
            }
            if exprs.len() == 0 {
                let unit_type = types.new_primitive(types::Primitive::Unit);
                return Ok(Expr::Constant {
                            val : (Literal::Unit, unit_type),
                            typ : unit_type });
            }

            let mut vals = vec![];
            let mut typs = vec![];
            let mut errors = LinkedList::new();

            for exp in exprs {
                match process_expr(exp, lexer, stringtab, env, types) {
                    Err(mut errs) => errors.append(&mut errs),
                    Ok(val) => {
                        typs.push(val.get_type());
                        vals.push(val);
                    },
                }
            }

            if !errors.is_empty() {
                Err(errors)
            } else {
                Ok(Expr::Tuple {
                    vals : vals,
                    typ  : types.new_tuple(typs) })
            }
        },
        lang_y::Expr::Struct { span, name, ty_args, exprs } => {
            if name.len() != 1 {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "packages".to_string())))?
            }

            let struct_nm = intern_package_name(&name, lexer, stringtab)[0];
            match env.lookup(&struct_nm) {
                Some(Entity::Variable { .. }) => {
                    Err(singleton_error(
                            ErrorMessage::KindError(
                                span_to_loc(span, lexer),
                                "struct name".to_string(),
                                "variable".to_string())))
                },
                Some(Entity::DynConst { .. }) => {
                    Err(singleton_error(
                            ErrorMessage::KindError(
                                span_to_loc(span, lexer),
                                "struct name".to_string(),
                                "dynamic constant".to_string())))
                },
                Some(Entity::Constant { .. }) => {
                    Err(singleton_error(
                            ErrorMessage::KindError(
                                span_to_loc(span, lexer),
                                "struct name".to_string(),
                                "constant".to_string())))
                },
                Some(Entity::Function { .. }) => {
                    Err(singleton_error(
                            ErrorMessage::KindError(
                                span_to_loc(span, lexer),
                                "struct name".to_string(),
                                "function".to_string())))
                },
                None => {
                    Err(singleton_error(
                            ErrorMessage::UndefinedVariable(
                                span_to_loc(span, lexer),
                                stringtab.lookup_id(struct_nm).unwrap())))
                },
                Some(Entity::Type { type_args : kinds, value : typ }) => {
                    if !types.is_struct(*typ) {
                        Err(singleton_error(
                                ErrorMessage::KindError(
                                    span_to_loc(span, lexer),
                                    "struct name".to_string(),
                                    "non-struct type".to_string())))?
                    }
                    if kinds.len() != ty_args.len() {
                        Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(span, lexer),
                                    format!("Expected {} type arguments, provided {}",
                                            kinds.len(), ty_args.len()))))?
                    }

                    // Verify that the type arguments we are provided are correct and collect the
                    // type variable and dynamic constant substitutions
                    let mut type_vars = vec![];
                    let mut dyn_consts = vec![];
                    let mut errors = LinkedList::new();

                    for (arg, kind) in ty_args.into_iter().zip(kinds.iter()) {
                        let arg_span = arg.span();
                        match kind {
                            lang_y::Kind::USize => {
                                match process_type_expr_as_expr(
                                        arg, lexer, stringtab, env, types) {
                                    Err(mut errs) => errors.append(&mut errs),
                                    Ok(val) => dyn_consts.push(val),
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
                                                    unparse_type(types, typ, stringtab)));
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
                                                        unparse_type(types, typ, stringtab)));
                                            }
                                        },
                                    }
                            },
                        }
                    }

                    if !errors.is_empty() { return Err(errors); }

                    let struct_type =
                        if type_vars.len() == 0 && dyn_consts.len() == 0 {
                            *typ
                        } else {
                            types.instantiate(*typ, &type_vars, &dyn_consts)
                        };

                    // Check each field and construct the appropriate tuple
                    // Note that fields that are omitted will be initialized with their type's
                    // default value
                    let num_fields = types.get_num_struct_fields(struct_type).unwrap();

                    // Values for the fields, in order
                    let mut values : Vec<Option<Expr>> = vec![None; num_fields];

                    for (field_name, expr) in exprs {
                        let field_nm = intern_id(&field_name, lexer, stringtab);
                        let expr_span = expr.span();

                        match types.get_field(struct_type, field_nm) {
                            None => {
                                errors.push_back(
                                    ErrorMessage::SemanticError(
                                        span_to_loc(field_name, lexer),
                                        format!("Struct {} does not have field {}",
                                                unparse_type(types, struct_type, stringtab),
                                                stringtab.lookup_id(field_nm).unwrap())));
                            },
                            Some((idx, field_typ)) => {
                                if values[idx].is_some() {
                                    errors.push_back(
                                        ErrorMessage::SemanticError(
                                            span_to_loc(field_name, lexer),
                                            format!("Field {} defined multiple times",
                                                    stringtab.lookup_id(field_nm).unwrap())));
                                } else {
                                    match process_expr(expr, lexer, stringtab, env, types) {
                                        Err(mut errs) => errors.append(&mut errs),
                                        Ok(val) => {
                                            let val_typ = val.get_type();
                                            if !types.equal(field_typ, val_typ) {
                                                // Set the value at this index even though there's
                                                // an error so that we also report if the field is
                                                // defined multiple times
                                                values[idx] = Some(Expr::Zero { typ : field_typ });
                                                errors.push_back(
                                                    ErrorMessage::TypeError(
                                                        span_to_loc(expr_span, lexer),
                                                        unparse_type(types, field_typ, stringtab),
                                                        unparse_type(types, val_typ, stringtab)));
                                            } else {
                                                values[idx] = Some(val);
                                            }
                                        },
                                    }
                                }
                            },
                        }
                    }

                    if !errors.is_empty() { return Err(errors); }

                    // Construct the list of field values, filling in zero values as needed
                    let filled_fields
                        = values.into_iter().enumerate()
                                .map(|(i, t)| t.unwrap_or(
                                        Expr::Zero { 
                                            typ : types.get_struct_field_type(struct_type, i).unwrap() }))
                                .collect::<Vec<_>>();

                    Ok(Expr::Tuple { vals : filled_fields,
                                     typ  : struct_type })
                },
            }
        },
        lang_y::Expr::BoolLit { span : _, value } => {
            let bool_typ = types.new_primitive(types::Primitive::Bool);
            Ok(Expr::Constant {
                val : (Literal::Bool(value), bool_typ),
                typ : bool_typ })
        },
        lang_y::Expr::IntLit { span, base } => {
            let res = u64::from_str_radix(lexer.span_str(span), base.base());
            assert!(res.is_ok(), "Internal Error: Int literal is not an integer");

            let num_typ = types.new_number();
            Ok(Expr::Constant {
                val : (Literal::Integer(res.unwrap()), num_typ),
                typ : num_typ })
        },
        lang_y::Expr::FloatLit { span } => {
            let res = lexer.span_str(span).parse::<f64>();
            assert!(res.is_ok(), "Internal Error: Float literal is not a float");

            let float_typ = types.new_float();
            Ok(Expr::Constant {
                val : (Literal::Float(res.unwrap()), float_typ),
                typ : float_typ })
        },
        lang_y::Expr::UnaryExpr { span, op, expr } => {
            let expr_val = process_expr(*expr, lexer, stringtab, env, types)?;
            let expr_typ = expr_val.get_type();

            match op {
                lang_y::UnaryOp::Negation => {
                    if !types.is_number(expr_typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(span, lexer),
                                    "number".to_string(),
                                    unparse_type(types, expr_typ, stringtab))))
                    } else {
                        Ok(Expr::UnaryExp {
                            op   : UnaryOp::Negation,
                            expr : Box::new(expr_val),
                            typ  : expr_typ })
                    }
                },
                lang_y::UnaryOp::BitwiseNot => {
                    if !types.is_integer(expr_typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(span, lexer),
                                    "integer".to_string(),
                                    unparse_type(types, expr_typ, stringtab))))
                    } else {
                        Ok(Expr::UnaryExp {
                            op   : UnaryOp::BitwiseNot,
                            expr : Box::new(expr_val),
                            typ  : expr_typ })
                    }
                },
                lang_y::UnaryOp::LogicalNot => {
                    if !types.is_bool(expr_typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(span, lexer),
                                    "bool".to_string(),
                                    unparse_type(types, expr_typ, stringtab))))
                    } else {
                        // ! x is translated into if x then false else true
                        let val_true = 
                            Expr::Constant {
                                val : (Literal::Bool(true), expr_typ),
                                typ : expr_typ };
                        let val_false =
                            Expr::Constant {
                                val : (Literal::Bool(false), expr_typ),
                                typ : expr_typ };
                        Ok(Expr::CondExpr {
                            cond : Box::new(expr_val),
                            thn  : Box::new(val_false),
                            els  : Box::new(val_true),
                            typ  : expr_typ })
                    }
                },
            }
        },
        lang_y::Expr::BinaryExpr { span : _, op, lhs, rhs } => {
            let lhs_span = lhs.span();
            let rhs_span = rhs.span();

            let lhs_res = process_expr(*lhs, lexer, stringtab, env, types);
            let rhs_res = process_expr(*rhs, lexer, stringtab, env, types);

            let (lhs_val, rhs_val) = append_errors2(lhs_res, rhs_res)?;
            let lhs_typ = lhs_val.get_type();
            let rhs_typ = rhs_val.get_type();

            // First, type-check
            match op {
                // Equality and inequality work on any types
                lang_y::BinaryOp::Eq | lang_y::BinaryOp::Neq => {
                    if !types.equal(lhs_typ, rhs_typ) {
                        return Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(rhs_span, lexer),
                                    unparse_type(types, lhs_typ, stringtab),
                                    unparse_type(types, rhs_typ, stringtab))));
                    }
                },
                // These work on any numbers
                lang_y::BinaryOp::Add   | lang_y::BinaryOp::Sub | lang_y::BinaryOp::Mul
                | lang_y::BinaryOp::Div | lang_y::BinaryOp::Lt  | lang_y::BinaryOp::Le
                | lang_y::BinaryOp::Gt  | lang_y::BinaryOp::Ge => {
                    let mut errors = LinkedList::new();
                    let lhs_number = types.is_number(lhs_typ);
                    let rhs_number = types.is_number(rhs_typ);
                    let equal = types.equal(lhs_typ, rhs_typ);

                    if lhs_number && !equal {
                        errors.push_back(
                            ErrorMessage::TypeError(
                                span_to_loc(rhs_span, lexer),
                                unparse_type(types, lhs_typ, stringtab),
                                unparse_type(types, rhs_typ, stringtab)));
                    } else if rhs_number && !equal {
                        errors.push_back(
                            ErrorMessage::TypeError(
                                span_to_loc(lhs_span, lexer),
                                unparse_type(types, rhs_typ, stringtab),
                                unparse_type(types, lhs_typ, stringtab)));
                    } else {
                        // The types are equal or both are not numbers
                        if !lhs_number {
                            errors.push_back(
                                ErrorMessage::TypeError(
                                    span_to_loc(lhs_span, lexer),
                                    "number".to_string(),
                                    unparse_type(types, lhs_typ, stringtab)));
                        }
                        if !rhs_number {
                            errors.push_back(
                                ErrorMessage::TypeError(
                                    span_to_loc(rhs_span, lexer),
                                    "number".to_string(),
                                    unparse_type(types, rhs_typ, stringtab)));
                        }
                    }

                    if !errors.is_empty() { return Err(errors); }
                },
                // These work on integer inputs
                lang_y::BinaryOp::Mod   | lang_y::BinaryOp::BitAnd | lang_y::BinaryOp::BitOr
                | lang_y::BinaryOp::Xor | lang_y::BinaryOp::LShift | lang_y::BinaryOp::RShift
                => {
                    let mut errors = LinkedList::new();
                    let lhs_integer = types.is_integer(lhs_typ);
                    let rhs_integer = types.is_integer(rhs_typ);
                    let equal = types.equal(lhs_typ, rhs_typ);

                    if lhs_integer && !equal {
                        errors.push_back(
                            ErrorMessage::TypeError(
                                span_to_loc(rhs_span, lexer),
                                unparse_type(types, lhs_typ, stringtab),
                                unparse_type(types, rhs_typ, stringtab)));
                    } else if rhs_integer && !equal {
                        errors.push_back(
                            ErrorMessage::TypeError(
                                span_to_loc(lhs_span, lexer),
                                unparse_type(types, rhs_typ, stringtab),
                                unparse_type(types, lhs_typ, stringtab)));
                    } else {
                        // The types are equal or both are not integers
                        if !lhs_integer {
                            errors.push_back(
                                ErrorMessage::TypeError(
                                    span_to_loc(lhs_span, lexer),
                                    "integer".to_string(),
                                    unparse_type(types, lhs_typ, stringtab)));
                        }
                        if !rhs_integer {
                            errors.push_back(
                                ErrorMessage::TypeError(
                                    span_to_loc(rhs_span, lexer),
                                    "integer".to_string(),
                                    unparse_type(types, rhs_typ, stringtab)));
                        }
                    }

                    if !errors.is_empty() { return Err(errors); }
                },
                // These work on boolean inputs
                lang_y::BinaryOp::LogAnd | lang_y::BinaryOp::LogOr => {
                    let mut errors = LinkedList::new();
                    let lhs_bool = types.is_bool(lhs_typ);
                    let rhs_bool = types.is_bool(rhs_typ);
                    let equal = types.equal(lhs_typ, rhs_typ);

                    if lhs_bool && !equal {
                        errors.push_back(
                            ErrorMessage::TypeError(
                                span_to_loc(rhs_span, lexer),
                                unparse_type(types, lhs_typ, stringtab),
                                unparse_type(types, rhs_typ, stringtab)));
                    } else if rhs_bool && !equal {
                        errors.push_back(
                            ErrorMessage::TypeError(
                                span_to_loc(lhs_span, lexer),
                                unparse_type(types, rhs_typ, stringtab),
                                unparse_type(types, lhs_typ, stringtab)));
                    } else {
                        // The types are equal or both are not bools
                        if !lhs_bool {
                            errors.push_back(
                                ErrorMessage::TypeError(
                                    span_to_loc(lhs_span, lexer),
                                    "bool".to_string(),
                                    unparse_type(types, lhs_typ, stringtab)));
                        }
                        if !rhs_bool {
                            errors.push_back(
                                ErrorMessage::TypeError(
                                    span_to_loc(rhs_span, lexer),
                                    "bool".to_string(),
                                    unparse_type(types, rhs_typ, stringtab)));
                        }
                    }

                    if !errors.is_empty() { return Err(errors); }
                },
            };

            match op {
                // The binary operations are compiled into conditional expressions:
                // x && y = if x then y    else false
                // x || y = if x then true else y
                lang_y::BinaryOp::LogAnd => {
                    let false_val =
                        Expr::Constant {
                            val : (Literal::Bool(false), lhs_typ),
                            typ : lhs_typ };
                    Ok(Expr::CondExpr {
                            cond : Box::new(lhs_val),
                            thn  : Box::new(rhs_val),
                            els  : Box::new(false_val),
                            typ  : lhs_typ })
                },
                lang_y::BinaryOp::LogOr => {
                    let true_val =
                        Expr::Constant {
                            val : (Literal::Bool(true), lhs_typ),
                            typ : lhs_typ };
                    Ok(Expr::CondExpr {
                            cond : Box::new(lhs_val),
                            thn  : Box::new(true_val),
                            els  : Box::new(rhs_val),
                            typ  : lhs_typ })
                },
                // For comparison operators, the resulting type is a boolean, while for all other
                // operations the result is the same as the two operands
                lang_y::BinaryOp::Lt   | lang_y::BinaryOp::Le
                | lang_y::BinaryOp::Gt | lang_y::BinaryOp::Ge
                | lang_y::BinaryOp::Eq | lang_y::BinaryOp::Neq => {
                    Ok(Expr::BinaryExp {
                        op  : convert_binary_op(op),
                        lhs : Box::new(lhs_val),
                        rhs : Box::new(rhs_val),
                        typ : types.new_primitive(types::Primitive::Bool) })
                },
                _ => {
                    Ok(Expr::BinaryExp {
                        op  : convert_binary_op(op),
                        lhs : Box::new(lhs_val),
                        rhs : Box::new(rhs_val),
                        typ : lhs_typ })
                },
            }
        },
        lang_y::Expr::CastExpr { span, expr, typ } => {
            // For the moment at least, casting is only supported between numeric types, and all
            // numeric types can be cast to each other
            let expr_res = process_expr(*expr, lexer, stringtab, env, types);
            let type_res = process_type(typ, lexer, stringtab, env, types);

            let (expr_val, to_typ) = append_errors2(expr_res, type_res)?;
            let expr_typ = expr_val.get_type();

            if !types.is_number(expr_typ) || !types.is_number(to_typ) {
                Err(singleton_error(
                        ErrorMessage::SemanticError(
                            span_to_loc(span, lexer),
                            format!("Can only cast between numeric types, cannot cast {} to {}",
                                    unparse_type(types, expr_typ, stringtab),
                                    unparse_type(types, to_typ, stringtab)))))
            } else {
                Ok(Expr::CastExpr { expr : Box::new(expr_val),
                                    typ  : to_typ })
            }
        },
        lang_y::Expr::CondExpr { span, cond, thn, els } => {
            let cond_span = cond.span();

            let cond_res = process_expr(*cond, lexer, stringtab, env, types);
            let thn_res  = process_expr(*thn,  lexer, stringtab, env, types);
            let els_res  = process_expr(*els,  lexer, stringtab, env, types);

            let (cond_val, thn_val, els_val)
                = append_errors3(cond_res, thn_res, els_res)?;

            let cond_typ = cond_val.get_type();
            let thn_typ  = thn_val.get_type();
            let els_typ  = els_val.get_type();

            let mut errors = LinkedList::new();

            if !types.is_bool(cond_typ) {
                errors.push_back(
                    ErrorMessage::TypeError(
                        span_to_loc(cond_span, lexer),
                        "bool".to_string(),
                        unparse_type(types, cond_typ, stringtab)));
            }
            if !types.equal(thn_typ, els_typ) {
                errors.push_back(
                    ErrorMessage::SemanticError(
                        span_to_loc(span, lexer),
                        format!("Types of conditional branches do not match, have {} and {}",
                                unparse_type(types, thn_typ, stringtab),
                                unparse_type(types, els_typ, stringtab))));
            }

            if !errors.is_empty() {
                Err(errors)
            } else {
                Ok(Expr::CondExpr {
                    cond : Box::new(cond_val),
                    thn  : Box::new(thn_val),
                    els  : Box::new(els_val),
                    typ  : thn_typ })
            }
        },
        lang_y::Expr::CallExpr { span, name, ty_args, args } => {
            // In the AST from the parser we have no way to distinguish between function calls and
            // union construction. We have to identify which case we're in here. We do this by
            // identifying whether the name (looking for the moment at just the first part of the
            // name) and determining whether it's a type or a function. Obviously we then report
            // errors if there are additional parts of the name
            if name.len() > 2 {
                Err(singleton_error(
                        ErrorMessage::NotImplemented(
                            span_to_loc(span, lexer),
                            "packages".to_string())))?
            }

            let nm = intern_package_name(&name, lexer, stringtab);

            match env.lookup(&nm[0]) {
                Some(Entity::Variable { .. })   | Some(Entity::DynConst { .. })
                | Some(Entity::Constant { .. }) | Some(Entity::Function { .. })
                | None if name.len() != 1 => {
                    Err(singleton_error(
                            ErrorMessage::NotImplemented(
                                span_to_loc(span, lexer),
                                "packages".to_string())))
                },
                None =>
                    Err(singleton_error(
                            ErrorMessage::UndefinedVariable(
                                span_to_loc(name[0], lexer),
                                stringtab.lookup_id(nm[0]).unwrap()))),
                Some(Entity::Variable { .. }) =>
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(name[0], lexer),
                                format!("{} is a variable, expected a function or union constructor",
                                        stringtab.lookup_id(nm[0]).unwrap())))),
                Some(Entity::DynConst { .. }) =>
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(name[0], lexer),
                                format!("{} is a dynamic constant, expected a function or union constructor",
                                        stringtab.lookup_id(nm[0]).unwrap())))),
                Some(Entity::Constant { .. }) =>
                    Err(singleton_error(
                            ErrorMessage::SemanticError(
                                span_to_loc(name[0], lexer),
                                format!("{} is a constant, expected a function or union constructor",
                                        stringtab.lookup_id(nm[0]).unwrap())))),
                Some(Entity::Type { type_args : kinds, value : typ }) => {
                    if !types.is_union(*typ) {
                        if name.len() != 1 {
                            Err(singleton_error(
                                    ErrorMessage::NotImplemented(
                                        span_to_loc(span, lexer),
                                        "packages".to_string())))?
                        } else {
                            Err(singleton_error(
                                    ErrorMessage::SemanticError(
                                        span_to_loc(name[0], lexer),
                                        format!("{} is a type, expected a function or union constructor",
                                                stringtab.lookup_id(nm[0]).unwrap()))))?
                        }
                    }
                    if name.len() != 2 {
                        Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(name[0], lexer),
                                    format!("Expected constructor name"))))?
                    }

                    if types.get_constructor_info(*typ, nm[1]).is_none() {
                        Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(name[1], lexer),
                                    format!("{} is not a constructor of type {}",
                                            stringtab.lookup_id(nm[1]).unwrap(),
                                            unparse_type(types, *typ, stringtab)))))?
                    }

                    // Now, we know that we are constructing some union, we need to verify that
                    // the type arguments are appropriate
                    if kinds.len() != ty_args.len() {
                        Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(span, lexer),
                                    format!("Expected {} type arguments, provided {}",
                                            kinds.len(), ty_args.len()))))?
                    }
                    
                    let mut type_vars = vec![];
                    let mut dyn_consts = vec![];
                    let mut errors = LinkedList::new();

                    for (arg, kind) in ty_args.into_iter().zip(kinds.iter()) {
                        let arg_span = arg.span();
                        match kind {
                            lang_y::Kind::USize => {
                                match process_type_expr_as_expr(
                                        arg, lexer, stringtab, env, types) {
                                    Err(mut errs) => errors.append(&mut errs),
                                    Ok(val) => dyn_consts.push(val),
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
                                                    unparse_type(types, typ, stringtab)));
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
                                                        unparse_type(types, typ, stringtab)));
                                            }
                                        },
                                    }
                            },
                        }
                    }

                    if !errors.is_empty() { return Err(errors); }

                    let union_type =
                        if type_vars.len() == 0 && dyn_consts.len() == 0 {
                            *typ
                        } else {
                            types.instantiate(*typ, &type_vars, &dyn_consts)
                        };
                    let Some((constr_idx, constr_typ))
                        = types.get_constructor_info(union_type, nm[1])
                        else { panic!("From above"); };

                    // Now, process the arguments to ensure they has the type needed by this
                    // constructor
                    // To do this, since unions take a single argument, we process the arguments as
                    // a single tuple, reporting an error if inout is used anywhere
                    for (is_inout, arg) in args.iter() {
                        if *is_inout {
                            Err(singleton_error(
                                    ErrorMessage::SemanticError(
                                        span_to_loc(arg.span(), lexer),
                                        format!("Union constructors cannot be marked inout"))))?
                        }
                    }

                    let body = process_expr(
                                lang_y::Expr::Tuple { 
                                    span : span,
                                    exprs : args.into_iter().map(|(_, a)| a).collect::<Vec<_>>() },
                                lexer, stringtab, env, types)?;
                    let body_typ = body.get_type();

                    if !types.equal(constr_typ, body_typ) {
                        Err(singleton_error(
                                ErrorMessage::TypeError(
                                    span_to_loc(span, lexer),
                                    unparse_type(types, constr_typ, stringtab),
                                    unparse_type(types, body_typ, stringtab))))
                    } else {
                        Ok(Expr::Union {
                            tag : constr_idx,
                            val : Box::new(body),
                            typ : union_type })
                    }
                },
                Some(Entity::Function { index : function, type_args : kinds, 
                                        args : func_args, return_type }) => {
                    let func = *function;

                    // Verify that the type arguments are appropriate
                    if kinds.len() != ty_args.len() {
                        Err(singleton_error(
                                ErrorMessage::SemanticError(
                                    span_to_loc(span, lexer),
                                    format!("Expected {} type arguments, provided {}",
                                            kinds.len(), ty_args.len()))))?
                    }
                    
                    let mut type_vars = vec![];
                    let mut dyn_consts = vec![];
                    let mut errors = LinkedList::new();

                    for (arg, kind) in ty_args.into_iter().zip(kinds.iter()) {
                        let arg_span = arg.span();
                        match kind {
                            lang_y::Kind::USize => {
                                match process_type_expr_as_expr(
                                        arg, lexer, stringtab, env, types) {
                                    Err(mut errs) => errors.append(&mut errs),
                                    Ok(val) => dyn_consts.push(val),
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
                                                    unparse_type(types, typ, stringtab)));
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
                                                        unparse_type(types, typ, stringtab)));
                                            }
                                        },
                                    }
                            },
                        }
                    }

                    if !errors.is_empty() { return Err(errors); }

                    let arg_types =
                        if type_vars.len() == 0 && dyn_consts.len() == 0 {
                            func_args.clone()
                        } else {
                            let mut tys = vec![];
                            for (t, inout) in func_args {
                                tys.push((
                                    types.instantiate(*t, &type_vars, &dyn_consts),
                                    *inout));
                            }
                            tys
                        };
                    let return_typ =
                        types.instantiate(*return_type, &type_vars, &dyn_consts);

                    // Now, process the arguments to ensure they has the type needed by this
                    // constructor
                    let mut arg_vals : Vec<Either<Expr, usize>> = vec![];
                    let mut errors = LinkedList::new();

                    for ((is_inout, arg), (arg_typ, expect_inout))
                        in args.into_iter().zip(arg_types.into_iter()) {
                    
                        let arg_span = arg.span();

                        if is_inout && !expect_inout {
                            errors.push_back(
                                ErrorMessage::SemanticError(
                                    span_to_loc(arg_span, lexer),
                                    format!("Argument should be inout")));
                        } else if !is_inout && expect_inout {
                            errors.push_back(
                                ErrorMessage::SemanticError(
                                    span_to_loc(arg_span, lexer),
                                    format!("Argument should not be inout")));
                        } else if is_inout {
                            // If the argument is an inout then it needs to just be a variable
                            match process_expr(arg, lexer, stringtab, env, types) {
                                Err(mut errs) => errors.append(&mut errs),
                                Ok(Expr::Variable { var, typ }) => {
                                    if !types.equal(arg_typ, typ) {
                                        errors.push_back(
                                            ErrorMessage::TypeError(
                                                span_to_loc(arg_span, lexer),
                                                unparse_type(types, arg_typ, stringtab),
                                                unparse_type(types, typ, stringtab)));
                                    } else {
                                        arg_vals.push(Either::Right(var));
                                    }
                                },
                                Ok(_) => {
                                    errors.push_back(
                                        ErrorMessage::SemanticError(
                                            span_to_loc(arg_span, lexer),
                                            format!("An inout argument must just be a variable")));
                                },
                            }
                        } else {
                            match process_expr(arg, lexer, stringtab, env, types) {
                                Err(mut errs) => errors.append(&mut errs),
                                Ok(exp) => {
                                    if !types.equal(arg_typ, exp.get_type()) {
                                        errors.push_back(
                                            ErrorMessage::TypeError(
                                                span_to_loc(arg_span, lexer),
                                                unparse_type(types, arg_typ, stringtab),
                                                unparse_type(types, exp.get_type(), stringtab)));
                                    } else {
                                        arg_vals.push(Either::Left(exp));
                                    }
                                },
                            }
                        }
                    }

                    if !errors.is_empty() {
                        Err(errors)
                    } else {
                        Ok(Expr::CallExpr {
                            func       : func,
                            ty_args    : type_vars,
                            dyn_consts : dyn_consts,
                            args       : arg_vals,
                            typ        : return_typ })
                    }
                },
            }
        },
    }
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

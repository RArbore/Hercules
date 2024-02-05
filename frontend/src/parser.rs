use std::collections::HashMap;
use std::fs::File;
use std::io::Read;

use lrlex::{lrlex_mod, DefaultLexerTypes};
use lrpar::{lrpar_mod, NonStreamingLexer};
use cfgrammar::Span;

lrlex_mod!("lang.l");
lrpar_mod!("lang.y");

use lang_y::*;
use crate::env::Env;

#[derive(Clone, PartialEq, Eq)]
pub enum IType {
    Primitive(Primitive),
    Tuple(Vec<IType>),
}
pub type BType = IType;

pub enum Entity {
    Variable { binding : usize, ty : BType, is_const : bool },
}

pub struct Prg {
    span : Span,
    name : usize, // Function name in the string table
    args : Vec<(usize, IType)>, // Argument list: binding number and type
    ty   : IType, // Result type of the function
    body : Vec<Stmt> // Body
}
pub enum Stmt {
    AssignStmt { var  : usize, typ   : IType, val   : Expr },
    IfStmt     { cond : Expr,  thn   : Vec<Stmt>, els : Vec<Stmt> },
    LoopStmt   { body : Box<Stmt> },
    ReturnStmt { val  : Expr },
    BreakStmt  { },
}
pub enum Expr {
    Const    { val  : PrimConstant, typ : IType },
    Variable { bind : usize,        typ : IType },
    Tuple    { vals : Vec<Expr>,    typ : IType },
}

pub enum PrimConstant {
    Bool(bool), I8(i8),   U8(u8),       I16(i16), U16(u16), I32(i32), U32(u32),
    I64(i64),   U64(u64), USize(usize), F32(f32), F64(f64), Unit()
}

// Internal string table records a map from a string number to the string itself
// along with the current count of string numbers and a map from strings to their
// numbers
type InternalStringTable = (HashMap<usize, String>, usize, HashMap<String, usize>);
pub type StringTable = HashMap<usize, String>;

pub fn parse_program(src_file : String) -> (Option<Prg>, StringTable) {
    let mut stringtab = (HashMap::new(), 0, HashMap::new());
    let mut file = File::open(src_file.clone()).expect("PANIC: Unable to open input file.");
    let prg = parse_file(&mut file, &mut stringtab);
    return (prg, stringtab.0);
}

fn insert_stringtab(t : &mut InternalStringTable, s : String) -> usize {
    match t.2.get(&s) {
        Some(n) => *n,
        None => {
            let n = t.1;
            t.1 += 1;
            t.2.insert(s.clone(), n);
            t.0.insert(n, s);
            n
        },
    }
}

fn intern_id(n : &Span, lex : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
             stringtab : &mut InternalStringTable) -> usize {
    insert_stringtab(stringtab, lex.span_str(*n).to_string())
}

fn intern_packageName(
    n : &PackageName, lex : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
    stringtab : &mut InternalStringTable) -> Vec<usize> {

    let mut res = vec![];
    for s in n {
        res.push(intern_id(s, lex, stringtab));
    }
    res
}

fn lookup_stringtab<'a>(i : &usize, t : &'a InternalStringTable) -> Option<&'a String> {
    t.0.get(i)
}

fn lookup_stringtab_err(i : &usize, t : &InternalStringTable) -> String {
    match lookup_stringtab(i, t) {
        Some(s) => String::from(s),
        None => "ERROR".to_string(),
    }
}

fn lookup_path_name(ns : &Vec<usize>, t : &InternalStringTable) -> Option<String> {
    let opts = ns.iter().map(|n| lookup_stringtab(n, t));
    let mut strs = vec![];
    for o in opts {
        match o {
            Some(s) => { strs.push(String::from(s)); },
            None => { return None; },
        }
    }
    Some(strs.join("::"))
}

fn lookup_path_name_err(ns : &Vec<usize>, t : &InternalStringTable) -> String {
    match lookup_path_name(ns, t) {
        Some(s) => s,
        None => "ERROR".to_string(),
    }
}

fn parse_file(file : &mut File, stringtab : &mut InternalStringTable)
    -> Option<Prg> {
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("PANIC: Unable to read input file contents.");

    let lexerdef = lang_l::lexerdef();
    let lexer = lexerdef.lexer(&contents);
    let (res, errs) = lang_y::parse(&lexer);
    if errs.is_empty() {
        match res {
            Some(Ok(r)) => prepare_program(r, &lexer, stringtab),
            _ => None,
        }
    } else {
        for e in errs {
            eprintln!("ERROR: {}", e.pp(&lexer, &lang_y::token_epp));
        }
        None
    }
}

fn prepare_program(
    prg : lang_y::Prg, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
    stringtab : &mut InternalStringTable) -> Option<Prg> {

    assert!(prg.len() == 1, "Currently, only programs with a single function are supported for compilation");

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
                                assert!(name.len() == 1); // TODO: Error message
                                let argName = intern_id(&name[0], lexer, stringtab);
                                let binding = bindNum;
                                bindNum += 1;

                                match prepare_type(t) {
                                    None => err = true,
                                    Some((bType, iType)) => {
                                        env.insert(argName,
                                                   Entity::Variable { binding, ty : bType, is_const : false });
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
}

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
                            assert!(name.len() == 1); // TODO: Error message
                            let argName = intern_id(&name[0], lexer, stringtab);
                            let binding = *bindings;
                            *bindings += 1;

                            match prepare_type(ty) {
                                None => None,
                                Some((bType, iType)) => {
                                    env.insert(argName,
                                               Entity::Variable { binding,
                                                            ty : bType.clone(),
                                                            is_const : false });

                                    match init {
                                        None => {
                                            let init_val = default_value(&iType);
                                            Some(vec![Stmt::AssignStmt {
                                                        var : binding,
                                                        typ : iType,
                                                        val : init_val }])
                                        },
                                        Some(exp) => {
                                            match process_expr(exp, lexer, stringtab, env) {
                                                None => None,
                                                Some((e, t)) => {
                                                    assert!(t == bType, "Initialization does not match specified type");
                                                    Some(vec![Stmt::AssignStmt {
                                                                var : binding,
                                                                typ : iType,
                                                                val : e }])
                                                },
                                            }
                                        },
                                    }
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
            todo!()
        },
        lang_y::Stmt::IfStmt { span, cond, thn, els } => {
            let mut condition
                = Expr::Const { val : PrimConstant::Bool(false),
                                typ : IType::Primitive(Primitive::Bool) };
            let mut then_branch = vec![];
            let mut else_branch = vec![];
            let mut err = false;

            match process_expr(cond, lexer, stringtab, env) {
                None => err = true,
                Some((c, ty)) => {
                    condition = c;
                    match ty {
                        BType::Primitive(Primitive::Bool) => {},
                        _ => {
                            err = true;
                            assert!(false, "If condition must be a boolean value"); // TODO: Error message
                        },
                    }
                },
            }

            match process_stmt(thn, lexer, stringtab, env, bindings,
                               extra_returns, in_loop, return_type) {
                None => err = true,
                Some(s) => then_branch = s,
            }

            match els {
                None => {},
                Some(els) => {
                    match process_stmt(els, lexer, stringtab, env, bindings,
                                       extra_returns, in_loop, return_type) {
                        None => err = true,
                        Some(s) => else_branch = s,
                    }
                },
            }

            if err { None }
            else { Some(vec![Stmt::IfStmt{ cond : condition, thn : then_branch,
                                      els  : else_branch }]) }
        },
        lang_y::Stmt::MatchStmt { span, expr, body } => {
            todo!("Match not implemented yet")
        },
        lang_y::Stmt::ForStmt { span, var, init, bound, step, body } => {
            todo!()
        },
        lang_y::Stmt::WhileStmt { span, cond, body } => {
            todo!()
        },
        lang_y::Stmt::ReturnStmt { span, expr } => {
            match process_expr(expr, lexer, stringtab, env) {
                None => None,
                Some((e, t)) => {
                    assert!(t == return_type.1, "Incorrect return type"); // TODO: Error message
                    
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
            assert!(in_loop, "Break not contained in loop"); // TODO: Error message
            Some(vec![Stmt::BreakStmt{}])
        },
        lang_y::Stmt::ContinueStmt { span } => {
            todo!()
        },
        lang_y::Stmt::BlockStmt { span, body } => {
            todo!()
        },
        lang_y::Stmt::CallStmt { span, name, ty_args, args } => {
            todo!()
        },
    }
}

fn process_expr(
    exp : &lang_y::Expr, lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>,
    stringtab : &mut InternalStringTable, env : &mut Env<usize, Entity>)
    -> Option<(Expr, BType)> {
 
    todo!()
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

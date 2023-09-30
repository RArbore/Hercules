extern crate clap;

use std::io::Read;
use std::fs::File;
use clap::Parser;

use cfgrammar::Span;
use lrlex::{lrlex_mod, DefaultLexerTypes};
use lrpar::{lrpar_mod, NonStreamingLexer};

lrlex_mod!("lang.l");
lrpar_mod!("lang.y");

use lang_y::*;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    src_file : String,
}

fn main() {
    let args = Cli::parse();
    let mut file = File::open(args.src_file).expect("PANIC: Unable to open input file.");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("PANIC: Unable to read input file contents.");

    let lexerdef = lang_l::lexerdef();
    let lexer = lexerdef.lexer(&contents);
    let (res, errs) = lang_y::parse(&lexer);
    for e in errs {
        eprintln!("ERROR: {}", e.pp(&lexer, &lang_y::token_epp));
    }
    match res {
        Some(Ok(r)) => println!("Unparsed Result:\n{}", unparse_prg(&r, 0, &lexer)),
        _ => eprintln!("Fatal Error.")
    }
}

fn indentation(n : usize) -> String {
    "  ".repeat(n)
}

fn unparse_int_lit(
    n : &Span, base : &IntBase,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match base {
        IntBase::Binary =>
            format!("0b{}", lexer.span_str(*n).to_string()),
        IntBase::Octal =>
            format!("0o{}", lexer.span_str(*n).to_string()),
        IntBase::Decimal =>
            format!("{}", lexer.span_str(*n).to_string()),
        IntBase::Hexadecimal =>
            format!("0x{}", lexer.span_str(*n).to_string()),
    }
}

fn unparse_id(
    n : &Id,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    lexer.span_str(*n).to_string()
}

fn unparse_package_name(
    n : &PackageName,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    n.iter().map(|&i| lexer.span_str(i)).collect::<Vec<_>>().join("::")
}

fn unparse_import_name(
    n : &ImportName,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match n.1 {
        Some(_) => format!("{}::*", unparse_package_name(&n.0, lexer)),
        None    => unparse_package_name(&n.0, lexer),
    }
}

fn unparse_kind(
    k : &Kind,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match k {
        Kind::Type    => "type".to_string(),
        Kind::USize   => "usize".to_string(),
        Kind::Number  => "number".to_string(),
        Kind::Integer => "integer".to_string(),
    }
}

fn unparse_prim_type(
    p : &Primitive,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match p {
        Primitive::Bool  => "bool".to_string(),
        Primitive::I8    => "i8".to_string(),
        Primitive::U8    => "u8".to_string(),
        Primitive::I16   => "i16".to_string(),
        Primitive::U16   => "u16".to_string(),
        Primitive::I32   => "i32".to_string(),
        Primitive::U32   => "u32".to_string(),
        Primitive::I64   => "i64".to_string(),
        Primitive::U64   => "u64".to_string(),
        Primitive::USize => "usize".to_string(),
        Primitive::F32   => "f32".to_string(),
        Primitive::F64   => "f64".to_string(),
        Primitive::Void  => "void".to_string(),
    }
}

fn unparse_assign_op(
    op : &AssignOp,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match op {
        AssignOp::None   => "=",
        AssignOp::Add    => "+=",
        AssignOp::Sub    => "-=",
        AssignOp::Mul    => "*=",
        AssignOp::Div    => "/=",
        AssignOp::Mod    => "%=",
        AssignOp::BitAnd => "&=",
        AssignOp::BitOr  => "|=",
        AssignOp::Xor    => "^=",
        AssignOp::LogAnd => "&&=",
        AssignOp::LogOr  => "||=",
        AssignOp::LShift => "<<=",
        AssignOp::RShift => ">>=",
    }.to_string()
}

fn unparse_unary_op(
    op : &UnaryOp,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match op {
        UnaryOp::Negation   => "-",
        UnaryOp::BitwiseNot => "~",
        UnaryOp::LogicalNot => "!",
    }.to_string()
}

fn unparse_binary_op(
    op : &BinaryOp,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match op {
        BinaryOp::Add    => "+",
        BinaryOp::Sub    => "-",
        BinaryOp::Mul    => "*",
        BinaryOp::Div    => "/",
        BinaryOp::Mod    => "%",
        BinaryOp::BitAnd => "&",
        BinaryOp::LogAnd => "&&",
        BinaryOp::BitOr  => "|",
        BinaryOp::LogOr  => "||",
        BinaryOp::Xor    => "^",
        BinaryOp::Lt     => "<",
        BinaryOp::Le     => "<=",
        BinaryOp::Gt     => ">",
        BinaryOp::Ge     => ">=",
        BinaryOp::Eq     => "==",
        BinaryOp::Neq    => "!=",
        BinaryOp::LShift => "<<",
        BinaryOp::RShift => ">>",
    }.to_string()
}

fn unparse_prg(
    r : &Prg, indent : usize,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    r.iter().map(|t| unparse_top(t, indent, lexer)).collect::<Vec<_>>().join("\n")
}

fn unparse_top(
    t : &Top, indent : usize,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match t {
        Top::Import{ span : _, name : name }
            => format!("{}use {};",
                       indentation(indent),
                       unparse_import_name(name, lexer)),
        Top::TypeDecl{ span : _, public : public, name : name, tyVars : tyVars,
                       body : body }
            => unparse_type_decl(public, name, tyVars, body, indent, lexer),
        Top::ConstDecl{ span : _, public : public, name : name, ty : ty, body : body }
            => unparse_const_decl(public, name, ty, body, indent, lexer),
        Top::FuncDecl{ span : _, public : public, attr : attr, name : name, tyVars : tyVars,
                       args : args, ty : ty, body : body }
            => format!("{}{}{}fn {}{}({}){} {}",
                       indentation(indent),
                       match attr { None => "".to_string(),
                                    Some(a) =>
                                        format!("{}\n{}",
                                                unparse_id(a, lexer),
                                                indentation(indent)), },
                       if *public { "pub " } else { "" },
                       unparse_id(name, lexer),
                       unparse_type_vars(tyVars, lexer),
                       unparse_parameters(args, lexer),
                       match ty { None => "".to_string(),
                                  Some(t) =>
                                      format!(" : {}", unparse_type(t, lexer)) },
                       unparse_stmt(body, indent+1, lexer)),
        Top::ModDecl{ span : _, public : public, name : name, body : body }
            => format!("{}{}module {} {{\n{}\n{}}}",
                       indentation(indent),
                       if *public { "pub " } else { "" },
                       unparse_id(name, lexer),
                       unparse_prg(body, indent+1, lexer),
                       indentation(indent)),
    }
}

fn unparse_type_decl(
    public : &bool, name : &Id, tyVars : &Vec<TypeVar>, body : &TyDef,
    indent : usize,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    format!("{}{}type {}{} = {};",
            indentation(indent),
            if *public { "pub " } else { "" },
            unparse_id(name, lexer),
            unparse_type_vars(tyVars, lexer),
            unparse_type_def(body, lexer))
}

fn unparse_const_decl(
    public : &bool, name : &Id, ty : &Option<Type>, body : &Expr,
    indent : usize,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    format!("{}{}const {}{} = {};",
            indentation(indent),
            if *public { "pub " } else { "" },
            unparse_id(name, lexer),
            match ty { None    => "".to_string(),
                       Some(t) => format!(" : {}", unparse_type(t, lexer)) },
            unparse_expr(body, lexer))
}

fn unparse_type_var(
    tyVar : &TypeVar,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    format!("{} : {}",
            unparse_id(&tyVar.name, lexer),
            unparse_kind(&tyVar.kind, lexer))
}

fn unparse_type_vars(
    tyVars : &Vec<TypeVar>,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    if tyVars.is_empty() { "".to_string() }
    else { format!("<{}> ",
                   tyVars.iter()
                    .map(|tv| unparse_type_var(tv, lexer))
                    .collect::<Vec<_>>()
                    .join(", ")) }
}

fn unparse_type_def(
    def : &TyDef,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match def {
        TyDef::TypeAlias{ span : _, body : body } => unparse_type(body, lexer),
        TyDef::Struct{ span : _, public : public, fields : fields } => {
            format!("{}struct {{ {} }}",
                    if *public { "pub " } else { "" },
                    unparse_obj_fields(fields, lexer))
        },
        TyDef::Union{ span : _, public : public, fields : fields } => {
            format!("{}union {{ {} }}",
                    if *public { "pub " } else { "" },
                    unparse_obj_fields(fields, lexer))
        },
    }
}

fn unparse_obj_field(
    field : &ObjField,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    format!("{}{}{};",
            if field.public { "pub " } else { "" },
            unparse_id(&field.name, lexer),
            match &field.typ {  None => "".to_string(),
                                Some(t) => unparse_type(&t, lexer) })
}

fn unparse_obj_fields(
    def : &Vec<ObjField>,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    def.iter().map(|fd| unparse_obj_field(fd, lexer)).collect::<Vec<_>>().join(" ")
}

fn unparse_type(
    typ : &Type,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match typ {
        Type::PrimType { span : _, typ : prim } =>
            unparse_prim_type(prim, lexer),
        Type::TupleType { span : _, tys : tys } =>
            format!("({})", tys.iter()
                               .map(|t| unparse_type(t, lexer))
                               .collect::<Vec<_>>().join(", ")),
        Type::NamedType { span : _, name : name, args : args } =>
            format!("{}{}", unparse_package_name(name, lexer),
                            unparse_type_exprs(args, lexer)),
        Type::ArrayType { span : _, elem : elem, dims : dims } =>
            format!("{}[{}]", unparse_type(elem, lexer),
                    dims.iter().map(|e| unparse_expr(e, lexer))
                               .collect::<Vec<_>>().join(", ")),
    }
}

fn unparse_type_expr(
    tyExp : &TypeExpr,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match tyExp {
        TypeExpr::PrimType { span : _, typ : typ } =>
            unparse_prim_type(typ, lexer),
        TypeExpr::TupleType { span : _, tys : tys } =>
            format!("({})", tys.iter().map(|t| unparse_type(t, lexer))
                               .collect::<Vec<_>>().join(", ")),
        TypeExpr::NamedTypeExpr { span : _, name : name, args : args } =>
            format!("{}{}", unparse_package_name(name, lexer),
                    if args.is_empty() { "".to_string() }
                    else { format!("<{}>",
                                   args.iter().map(|t| unparse_type_expr(t, lexer))
                                       .collect::<Vec<_>>().join(", ")) }),
        TypeExpr::ArrayTypeExpr { span : _, elem : elem, dims : dims } =>
            format!("{}[{}]", unparse_type_expr(elem, lexer),
                    dims.iter().map(|e| unparse_expr(e, lexer))
                        .collect::<Vec<_>>().join(", ")),
        TypeExpr::BoolLiteral { span : _, val : val } =>
            if *val { "true".to_string() } else { "false".to_string() },
        TypeExpr::IntLiteral { span : span, base : base } =>
            unparse_int_lit(span, base, lexer),
        TypeExpr::Negative { span : _, expr : expr } =>
            format!("(- {})", unparse_type_expr(expr, lexer)),
        TypeExpr::Add { span : _, lhs : lhs, rhs : rhs } =>
            format!("({} + {})", unparse_type_expr(lhs, lexer),
                    unparse_type_expr(rhs, lexer)),
        TypeExpr::Sub { span : _, lhs : lhs, rhs : rhs } =>
            format!("({} - {})", unparse_type_expr(lhs, lexer),
                    unparse_type_expr(rhs, lexer)),
        TypeExpr::Mul { span : _, lhs : lhs, rhs : rhs } =>
            format!("({} * {})", unparse_type_expr(lhs, lexer),
                    unparse_type_expr(rhs, lexer)),
    }
}

fn unparse_type_exprs(
    tyExp : &Vec<TypeExpr>,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    if tyExp.is_empty() {
        "".to_string()
    } else {
        format!("<{}>", tyExp.iter().map(|e| unparse_type_expr(e, lexer))
                             .collect::<Vec<_>>().join(", "))
    }
}

fn unparse_expr(
    exp : &Expr,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match exp {
        Expr::Variable { span : _, name : name } =>
            unparse_package_name(name, lexer),
        Expr::Field { span : _, lhs : lhs, rhs : rhs } =>
            format!("{}.{}", unparse_expr(lhs, lexer), unparse_id(rhs, lexer)),
        Expr::NumField { span : _, lhs : lhs, rhs : rhs } =>
            format!("{}{}", unparse_expr(lhs, lexer), unparse_id(rhs, lexer)),
        Expr::ArrIndex { span : _, lhs : lhs, index : index } =>
            format!("{}[{}]", unparse_expr(lhs, lexer),
                    index.iter().map(|e| unparse_expr(e, lexer))
                         .collect::<Vec<_>>().join(", ")),
        Expr::Tuple { span : _, exprs : exprs } =>
            format!("({})", exprs.iter().map(|e| unparse_expr(e, lexer))
                                 .collect::<Vec<_>>().join(", ")),
        Expr::OrderedStruct { span : _, exprs : exprs } =>
            format!("{{{}}}", exprs.iter().map(|e| unparse_expr(e, lexer))
                                 .collect::<Vec<_>>().join(", ")),
        Expr::NamedStruct { span : _, exprs : exprs } =>
            format!("{{{}}}",
                    exprs.iter().map(|e| format!("{} = {}",
                                                 unparse_id(&e.0, lexer),
                                                 unparse_expr(&e.1, lexer)))
                         .collect::<Vec<_>>().join(", ")),
        Expr::BoolLit { span : _, value : value } =>
            if *value { "true" } else { "false" }.to_string(),
        Expr::IntLit { span : span, base : base } =>
            unparse_int_lit(span, base, lexer),
        Expr::FloatLit { span : span } => unparse_id(span, lexer),
        Expr::UnaryExpr { span : _, op : op, expr : expr } =>
            format!("({} {})", unparse_unary_op(op, lexer),
                    unparse_expr(expr, lexer)),
        Expr::BinaryExpr { span : _, op : op, lhs : lhs, rhs : rhs } =>
            format!("({} {} {})", unparse_expr(lhs, lexer),
                    unparse_binary_op(op, lexer), unparse_expr(rhs, lexer)),
        Expr::CastExpr { span : _, expr : expr, typ : typ } =>
            format!("({} as {})", unparse_expr(expr, lexer),
                    unparse_type(typ, lexer)),
        Expr::SizeExpr { span : _, expr : expr } =>
            format!("(size {})", unparse_expr(expr, lexer)),
        Expr::CondExpr { span : _, cond : cond, thn : thn, els : els } =>
            format!("(if {} then {} else {})", unparse_expr(cond, lexer),
                    unparse_expr(thn, lexer), unparse_expr(els, lexer)),
        Expr::CallExpr { span : _, name : name, tyArgs : tyArgs, args : args } =>
            format!("{}{}({})", unparse_package_name(name, lexer),
                    if tyArgs.is_empty() { "".to_string() }
                    else { format!("::<{}>",
                                   tyArgs.iter().map(|a| unparse_type_expr(a, lexer))
                                         .collect::<Vec<_>>().join(", ")) },
                    args.iter().map(|e| format!("{}{}", if e.0 { "&" } else { "" },
                                                unparse_expr(&e.1, lexer)))
                        .collect::<Vec<_>>().join(", ")),
    }
}

fn unparse_lexpr(
    exp : &LExpr,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match exp {
        LExpr::VariableLExpr { span : span } => unparse_id(span, lexer),
        LExpr::FieldLExpr { span : _, lhs : lhs, rhs : rhs } =>
            format!("{}.{}", unparse_lexpr(lhs, lexer), unparse_id(rhs, lexer)),
        LExpr::NumFieldLExpr { span : _, lhs : lhs, rhs : rhs } =>
            format!("{}{}", unparse_lexpr(lhs, lexer), unparse_id(rhs, lexer)),
        LExpr::IndexLExpr { span : _, lhs : lhs, index : index } =>
            format!("{}[{}]", unparse_lexpr(lhs, lexer),
                    index.iter().map(|e| unparse_expr(e, lexer))
                         .collect::<Vec<_>>().join(", ")),
    }
}

fn unparse_parameters(
    params : &Vec<(Option<Span>, VarBind)>,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    params.iter().map(|p| format!("{}{}",
                                  match &p.0 { None => "", Some(_) => "inout " },
                                  unparse_var_binding(&p.1, lexer)))
        .collect::<Vec<_>>().join(", ")
}

fn unparse_var_binding(
    var : &VarBind,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match &var.typ {
        None => unparse_spattern(&var.pattern, lexer),
        Some(t) => format!("{} : {}", unparse_spattern(&var.pattern, lexer),
                           unparse_type(t, lexer)),
    }
}

fn unparse_pattern(
    pat : &Pattern,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match pat {
        Pattern::SimplePattern{ span : _, pat : pat } =>
            unparse_spattern(pat, lexer),
        Pattern::UnionPattern{ span : _, name : name, pat : pat } =>
            format!("{} {}", unparse_package_name(name, lexer),
                    unparse_spattern(pat, lexer)),
    }
}

fn unparse_spattern(
    pat : &SPattern,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match pat {
        SPattern::Wildcard{ span : _ } => "_".to_string(),
        SPattern::IntLit{ span : span, base : base } =>
            unparse_int_lit(span, base, lexer),
        SPattern::Variable{ span : _, name : name } =>
            unparse_package_name(name, lexer),
        SPattern::TuplePat{ span : _, pats : pats } =>
            format!("({})", pats.iter().map(|p| unparse_pattern(p, lexer))
                                .collect::<Vec<_>>().join(", ")),
        SPattern::OrderedStructPat { span : _, pats : pats } =>
            format!("{{{}}}", pats.iter().map(|p| unparse_pattern(p, lexer))
                                  .collect::<Vec<_>>().join(", ")),
        SPattern::NamedStructPat { span : _, pats : pats } =>
            format!("{{{}}}", pats.iter()
                                  .map(|p| format!("{} = {}",
                                                   unparse_id(&p.0, lexer),
                                                   unparse_pattern(&p.1, lexer)))
                                  .collect::<Vec<_>>().join(", ")),
    }
}

fn unparse_stmt(
    stmt : &Stmt, indent : usize,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    match stmt {
        Stmt::LetStmt { span : _, var : var, init : init } =>
            format!("{}let {}{};", indentation(indent),
                    unparse_var_binding(var, lexer),
                    match init { None => "".to_string(),
                        Some(e) => format!(" = {}", unparse_expr(e, lexer)) }),
        Stmt::ConstStmt { span : _, var : var, init : init } =>
            format!("{}const {} = {};", indentation(indent),
                    unparse_var_binding(var, lexer),
                    unparse_expr(init, lexer)),
        Stmt::AssignStmt { span : _, lhs : lhs, assign : assign, assignSpan : _,
                           rhs : rhs } =>
            format!("{}{} {} {};", indentation(indent),
                    unparse_lexpr(lhs, lexer),
                    unparse_assign_op(assign, lexer),
                    unparse_expr(rhs, lexer)),
        Stmt::IfStmt { span : _, cond : cond, thn : thn, els : els } =>
            format!("{}if ({})\n{}{}", indentation(indent),
                    unparse_expr(cond, lexer), unparse_stmt(thn, indent+1, lexer),
                    match els { None => "".to_string(),
                        Some(s) => format!("\n{}else\n{}", indentation(indent),
                                            unparse_stmt(s, indent+1, lexer)), }),
        Stmt::MatchStmt { span : _, expr : expr, body : body } =>
            format!("{}match ({}) {{\n{}\n{}}}", indentation(indent),
                    unparse_expr(expr, lexer),
                    body.iter().map(|c| unparse_case(c, indent+1, lexer))
                        .collect::<Vec<_>>().join("\n"),
                    indentation(indent)),
        Stmt::ForStmt { span : _, var : var, init : init, bound : bound,
                        step : step, body : body } =>
            format!("{}for ({} = {} to {}{})\n{}", indentation(indent),
                    unparse_var_binding(var, lexer),
                    unparse_expr(init, lexer), unparse_expr(bound, lexer),
                    match step { None => "".to_string(),
                        Some(e) => format!(" by {}", unparse_expr(e, lexer)), },
                    unparse_stmt(body, indent+1, lexer)),
        Stmt::WhileStmt { span : _, cond : cond, body : body } =>
            format!("{}while ({})\n{}", indentation(indent),
                    unparse_expr(cond, lexer),
                    unparse_stmt(body, indent+1, lexer)),
        Stmt::ReturnStmt { span : _, expr : expr } =>
            format!("{}return {};", indentation(indent),
                    unparse_expr(expr, lexer)),
        Stmt::BlockStmt { span : _, body : body } =>
            format!("{}{{\n{}\n{}}}", indentation(indent-1),
                    body.iter().map(|s| unparse_stmt(s, indent, lexer))
                        .collect::<Vec<_>>().join("\n"),
                    indentation(indent-1)),
        Stmt::CallStmt { span : _, name : name, tyArgs : tyArgs, args : args } =>
            format!("{}{}{}({});", indentation(indent),
                    unparse_package_name(name, lexer),
                    if tyArgs.is_empty() { "".to_string() }
                    else { format!("::<{}>",
                                   tyArgs.iter().map(|a| unparse_type_expr(a, lexer))
                                         .collect::<Vec<_>>().join(", ")) },
                    args.iter().map(|e| format!("{}{}", if e.0 { "&" } else { "" },
                                                        unparse_expr(&e.1, lexer)))
                        .collect::<Vec<_>>().join(", ")),
    }
}

fn unparse_case(
    case : &Case, indent : usize,
    lexer : &dyn NonStreamingLexer<DefaultLexerTypes<u32>>
) -> String {
    format!("{} => {}",
            case.pat.iter().map(|p| unparse_pattern(p, lexer))
                .collect::<Vec<_>>().join("| "),
            unparse_stmt(&case.body, indent+1, lexer))
}

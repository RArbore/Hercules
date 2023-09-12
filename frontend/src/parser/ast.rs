use std::collections::HashMap;

pub enum PrimType { Bool, I8, U8, I16, U16, I32, U32, I64, U64, F32, F64, Void, }
pub enum Kind { StarKind(), PrimKind(PrimType), }
pub enum Type {
    Primitive(PrimType),
    Tuple(Vec<Type>),
    // name and var bindings
    NamedType(Vec<u64>, Vec<Type>),
    // element type, dimensions
    Array(Box<Type>, Vec<Expr>),
}
pub type VarBinding = (SPattern, Option<Type>);
pub enum Pattern {
    ConstructorPattern(Vec<u64>, SPattern),
    SimplePattern(SPattern),
}
pub enum SPattern {
    Wildcard(),
    IntLiteral(u64),
    // Name patterns have to be package names because it could be a constructor
    NamedPattern(Vec<u64>),
    Tuple(Vec<Pattern>),
    StructOrdered(Vec<Pattern>),
    StructNamed(Vec<(u64, Pattern)>),
}
pub enum AssignOp { Eq, EqPlus, EqMinus, EqTimes, EqDiv, EqMod, EqAndB,
                    EqAndL, EqOrB, EqOrL, EqXor, EqShiftL, EqShiftR }
pub enum LExpr {
    Ident(u64), 
    // LExpr and field name
    NamedField(Box<LExpr>, u64),
    // LExpr and field number in string table (omitting dot)
    NumberField(Box<LExpr>, u64),
    // LExpr and indices
    ArrayAccess(Box<LExpr>, Vec<Expr>),
}
pub enum NumBase { Binary, Octal, Decimal, Hexadecimal, }
pub enum UnaryOp { Negate, BitNot, BoolNot, }
pub enum BinaryOp { Add, Sub, Mul, Div, Mod, BitAnd, BoolAnd, BitOr, BoolOr,
                    Xor, Lt, Le, Gt, Ge, Eq, Neq, ShiftL, ShiftR, }
pub enum Expr {
    Ident(Vec<u64>), NamedField(Box<Expr>, u64), NumberField(Box<Expr>, u64),
    ArrayAccess(Box<Expr>, Vec<Expr>), Tuple(Vec<Expr>),
    StructOrdered(Vec<Expr>), StructNamed(Vec<(u64, Expr)>),
    Bool(bool),
    // ints and floats store their strings, and for ints their base
    Int(u64, NumBase), Float(u64),
    Cast(Box<Expr>, Type), Size(Box<Expr>),
    UnaryExpr(Box<Expr>, UnaryOp), BinaryExpr(Box<Expr>, BinaryOp, Box<Expr>),
    // condition, then, else
    IfExpr(Box<Expr>, Box<Expr>, Box<Expr>),
    // function name, type arguments, arguments (bool for ref)
    Call(Vec<u64>, Vec<Type>, Vec<(bool, Expr)>),
}
pub type Case = (Vec<Pattern>, Stmt);
pub enum Stmt {
    VarDec(VarBinding, Option<Expr>), ConstDec(VarBinding, Expr),
    Assignment(LExpr, AssignOp, Expr), IfStmt(Expr, Box<Stmt>, Option<Box<Stmt>>),
    MatchStmt(Expr, Vec<Case>),
    // Variable, start, end, by, body
    ForLoop(VarBinding, Expr, Expr, Expr, Box<Stmt>),
    While(Expr, Box<Stmt>), Return(Expr), Block(Vec<Stmt>),
}
pub enum TypeDef {
    TypeAlias(Type),
    // pub, fields (pub, name, type)
    Struct(bool, Vec<(bool, u64, Type)>),
    // pub, constructors (name, type)
    Union(bool, Vec<(u64, Type)>),
}
pub enum TopLevel {
    // names, whether a full import
    Import(Vec<u64>, bool),
    // pub, id, names and kinds of type vars, definition
    TypeDecl(bool, u64, Vec<(u64, Kind)>, TypeDef),
    // pub, id, type, expression
    ConstDecl(bool, u64, Option<Type>, Expr),
    // attribute, pub, id, type vars, arguments (inout and binding), return type, body
    FuncDecl(Option<u64>, bool, u64, Vec<(u64, Kind)>, Vec<(bool, VarBinding)>, Option<Type>, Stmt),
    // pub, id, contents
    Module(bool, u64, Vec<TopLevel>),
}
pub struct Program {
    pub string_tab : HashMap<u64, String>,
    pub program : Vec<TopLevel>,
}

fn package_name_to_string(name : &Vec<u64>, string_tab : &HashMap<u64, String>) -> String {
    let mut strs : Vec<String> = vec![];
    for i in 1..name.len() {
        match string_tab.get(&name[i]) {
            Some(s) => strs.push(s.clone()),
            None => strs.push("<error>".to_string()),
        }
    }
    return strs.join("::");
}

fn id_to_string(name : &u64, string_tab : &HashMap<u64, String>) -> String {
    match string_tab.get(name) {
        Some(s) => s.clone(),
        None => "<error>".to_string(),
    }
}

fn unparse_prim_type(p : &PrimType) -> &'static str {
    match p {
        PrimType::Bool => "bool",
        PrimType::I8   => "i8",
        PrimType::U8   => "u8",
        PrimType::I16  => "i16",
        PrimType::U16  => "u16",
        PrimType::I32  => "i32",
        PrimType::U32  => "u32",
        PrimType::I64  => "i64",
        PrimType::U64  => "u64",
        PrimType::F32  => "f32",
        PrimType::F64  => "f64",
        PrimType::Void => "void",
    }
}

fn unparse_type_vars<'a>(vars : &Vec<(u64, Kind)>, string_tab : &HashMap<u64, String>) -> String {
    if vars.is_empty() { return String::new(); }
    let mut args : Vec<String> = vec![];
    for i in 0..vars.len() {
        let (n, k) = &vars[i];
        let kind_str : &'static str;
        match &k {
            Kind::StarKind() => kind_str = "type",
            Kind::PrimKind(ty) => kind_str = unparse_prim_type(ty),
        }
        args.push(format!("{} : {}", id_to_string(n, string_tab), kind_str));
    }
    return format!("<{}>", args.join(", "));
}

fn unparse_type<'a>(ty : &Type, string_tab : &HashMap<u64, String>) -> String {
    match ty {
        Type::Primitive(pty) => unparse_prim_type(pty).to_string(),
        Type::Tuple(tys) => {
            let mut fmt_tys : Vec<String> = vec![];
            for t in tys {
                fmt_tys.push(unparse_type(t, string_tab));
            }
            format!("({})", fmt_tys.join(", "))
        },
        Type::NamedType(name, tys) => {
            let ty_args : String;
            if tys.is_empty() {
                ty_args = String::new();
            } else {
                let mut args : Vec<String> = vec![];
                for t in tys {
                    args.push(unparse_type(t, string_tab));
                }
                ty_args = format!("<{}>", args.join(", "));
            }
            format!("{}{}", package_name_to_string(name, string_tab), ty_args)
        },
        Type::Array(ty, dims) => {
            let mut fmt_dims : Vec<String> = vec![];
            for d in dims {
                fmt_dims.push(unparse_expr(d, string_tab));
            }
            format!("{}[{}]", unparse_type(ty, string_tab), fmt_dims.join(", "))
        },
    }
}

fn unparse_type_def<'a>(def : &TypeDef, string_tab : &HashMap<u64, String>) -> String {
    match def {
        TypeDef::TypeAlias(ty) => unparse_type(ty, string_tab),
        TypeDef::Struct(public, fields) => {
            let mut fmt_fields : Vec<String> = vec![];
            for (public, name, ty) in fields {
                fmt_fields.push(format!("\t{}{} : {};", if *public { "pub " } else { "" },
                                        id_to_string(name, string_tab), unparse_type(ty, string_tab)));
            }
            format!("{}struct {{\n{}\n}}", if *public { "pub " } else { "" },
                    fmt_fields.join("\n"))
        },
        TypeDef::Union(public, constructors) => {
            let mut fmt_constructors : Vec<String> = vec![];
            for (name, ty) in constructors {
                fmt_constructors.push(format!("\t{} : {};", id_to_string(name, string_tab),
                                      unparse_type(ty, string_tab)));
            }
            format!("{}union {{\n{}\n}}", if *public { "pub " } else { "" },
                    fmt_constructors.join("\n"))
        },
    }
}

fn unparse_pattern<'a>(pat : &Pattern, string_tab : &HashMap<u64, String>) -> String {
    match pat {
        Pattern::ConstructorPattern(name, pat) =>
            format!("{} {}", package_name_to_string(name, string_tab),
                    unparse_spattern(pat, string_tab)),
        Pattern::SimplePattern(pat) => unparse_spattern(pat, string_tab),
    }
}

fn unparse_spattern<'a>(pat : &SPattern, string_tab : &HashMap<u64, String>) -> String {
    match pat {
        SPattern::Wildcard() => "_".to_string(),
        SPattern::IntLiteral(id) => id_to_string(id, string_tab),
        SPattern::NamedPattern(name) => package_name_to_string(name, string_tab),
        SPattern::Tuple(ps) => {
            let mut fmt_ps : Vec<String> = vec![];
            for p in ps {
                fmt_ps.push(unparse_pattern(p, string_tab));
            }
            format!("({})", fmt_ps.join(", "))
        },
        SPattern::StructOrdered(ps) => {
            let mut fmt_ps : Vec<String> = vec![];
            for p in ps {
                fmt_ps.push(unparse_pattern(p, string_tab));
            }
            format!("{{{}}}", fmt_ps.join(", "))
        },
        SPattern::StructNamed(ps) => {
            let mut fmt_ps : Vec<String> = vec![];
            for (n, p) in ps {
                fmt_ps.push(format!("{} = {}", id_to_string(n, string_tab),
                            unparse_pattern(p, string_tab)));
            }
            format!("{{{}}}", fmt_ps.join(", "))
        },
    }
}

fn unparse_varbinding<'a>(var : &VarBinding, string_tab : &HashMap<u64, String>) -> String {
    let (pattern, typ) = var;
    let mut fmt_type = String::new();
    match typ {
        None => {},
        Some(t) => fmt_type = format!(" : {}", unparse_type(t, string_tab)),
    }
    format!("{}{}", unparse_spattern(pattern, string_tab), fmt_type)
}

fn unparse_expr<'a>(expr : &Expr, string_tab : &HashMap<u64, String>) -> String {
    match expr {
        Expr::Ident(name) => package_name_to_string(name, string_tab),
        Expr::NamedField(expr, name) =>
            format!("{}.{}", unparse_expr(expr, string_tab), id_to_string(name, string_tab)),
        Expr::NumberField(expr, id) =>
            format!("{}.{}", unparse_expr(expr, string_tab), id_to_string(id, string_tab)),
        Expr::ArrayAccess(expr, idx) => {
            let mut fmt_ind : Vec<String> = vec![];
            for e in idx {
                fmt_ind.push(unparse_expr(e, string_tab));
            }
            format!("{}[{}]", unparse_expr(expr, string_tab), fmt_ind.join(", "))
        },
        Expr::Tuple(exps) => {
            let mut fmt_exps : Vec<String> = vec![];
            for e in exps {
                fmt_exps.push(unparse_expr(e, string_tab));
            }
            format!("({})", fmt_exps.join(", "))
        },
        Expr::StructOrdered(exps) => {
            let mut fmt_exps : Vec<String> = vec![];
            for e in exps {
                fmt_exps.push(unparse_expr(e, string_tab));
            }
            format!("{{{}}}", fmt_exps.join(", "))
        },
        Expr::StructNamed(exps) => {
            let mut fmt_exps : Vec<String> = vec![];
            for (name, e) in exps {
                fmt_exps.push(format!("{} = {}", id_to_string(name, string_tab),
                                      unparse_expr(e, string_tab)));
            }
            format!("{{{}}}", fmt_exps.join(", "))
        },
        Expr::Bool(val) =>
            if *val { "true".to_string() } else { "false".to_string() },
        Expr::Int(id, base) => {
            match base {
                NumBase::Binary => format!("0b{}", id_to_string(id, string_tab)),
                NumBase::Octal => format!("0o{}", id_to_string(id, string_tab)),
                NumBase::Decimal => id_to_string(id, string_tab),
                NumBase::Hexadecimal => format!("0x{}", id_to_string(id, string_tab)),
            }
        },
        Expr::Float(id) => id_to_string(id, string_tab),
        Expr::Cast(exp, typ) =>
            format!("({} as {})", unparse_expr(exp, string_tab), unparse_type(typ, string_tab)),
        Expr::Size(exp) => format!("(size {})", unparse_expr(exp, string_tab)),
        Expr::UnaryExpr(expr, op) => {
            let fmt_op =
                match op { UnaryOp::Negate => "-", UnaryOp::BitNot => "~", UnaryOp::BoolNot => "!", };
            format!("({} {})", fmt_op, unparse_expr(expr, string_tab))
        },
        Expr::BinaryExpr(lhs, op, rhs) => {
            let fmt_op =
                match op {
                    BinaryOp::Add => "+", BinaryOp::Sub => "-", BinaryOp::Mul => "*",
                    BinaryOp::Div => "/", BinaryOp::Mod => "%", BinaryOp::BitAnd => "&",
                    BinaryOp::BoolAnd => "&&", BinaryOp::BitOr => "|", BinaryOp::BoolOr => "||",
                    BinaryOp::Xor => "^", BinaryOp::Lt => "<", BinaryOp::Le => "<=",
                    BinaryOp::Gt => ">", BinaryOp::Ge => ">=", BinaryOp::Eq => "==",
                    BinaryOp::Neq => "!=", BinaryOp::ShiftL => "<<", BinaryOp::ShiftR => ">>",
                };
            format!("({} {} {})", unparse_expr(lhs, string_tab), fmt_op, unparse_expr(rhs, string_tab))
        }
        Expr::IfExpr(cond, thn, els) => {
            format!("(if {} then {} else {})", unparse_expr(cond, string_tab),
                    unparse_expr(thn, string_tab), unparse_expr(els, string_tab))
        },
        Expr::Call(name, ty_args, args) => {
            let mut fmt_tys = String::new();
            if !ty_args.is_empty() {
                let mut fmtd_tys : Vec<String> = vec![];
                for t in ty_args {
                    fmtd_tys.push(unparse_type(t, string_tab));
                }
                fmt_tys = format!("<{}>", fmtd_tys.join(", "));
            }
            let mut fmt_args : Vec<String> = vec![];
            for (is_ref, a) in args {
                fmt_args.push(format!("{}{}", if *is_ref { "&" } else { "" },
                                      unparse_expr(a, string_tab)));
            }
            format!("{}{}({})", package_name_to_string(name, string_tab),
                    fmt_tys, fmt_args.join(", "))
        },
    }
}

fn unparse_lexpr<'a>(lexp : &LExpr, string_tab : &HashMap<u64, String>) -> String {
    match lexp {
        LExpr::Ident(name) => id_to_string(name, string_tab),
        LExpr::NamedField(lexpr, name) =>
            format!("{}.{}", unparse_lexpr(lexpr, string_tab), id_to_string(name, string_tab)),
        LExpr::NumberField(lexpr, id) =>
            format!("{}.{}", unparse_lexpr(lexpr, string_tab), id_to_string(id, string_tab)),
        LExpr::ArrayAccess(lexpr, idx) => {
            let mut fmt_idx : Vec<String> = vec![];
            for i in idx {
                fmt_idx.push(unparse_expr(i, string_tab));
            }
            format!("{}[{}]", unparse_lexpr(lexpr, string_tab), fmt_idx.join(", "))
        },
    }
}

fn unparse_stmt<'a>(stmt : &Stmt, string_tab : &HashMap<u64, String>) -> String {
    match stmt {
        Stmt::VarDec(var, init) => {
            let mut fmt_init = String::new();
            match init {
                None => {},
                Some(e) => {
                    fmt_init = format!(" = {}", unparse_expr(e, string_tab));
                },
            }
            format!("\tlet {}{};", unparse_varbinding(var, string_tab), fmt_init)
        },
        Stmt::ConstDec(var, init) => {
            format!("\tconst {} = {};", unparse_varbinding(var, string_tab),
                    unparse_expr(init, string_tab))
        },
        Stmt::Assignment(lhs, op, rhs) => {
            let fmt_op = match op {
                AssignOp::Eq => "=", AssignOp::EqPlus => "+", AssignOp::EqMinus => "-",
                AssignOp::EqTimes => "*=", AssignOp::EqDiv => "/=", AssignOp::EqMod => "%=",
                AssignOp::EqAndB => "&=", AssignOp::EqAndL => "&&=", AssignOp::EqOrB => "|=",
                AssignOp::EqOrL => "||=", AssignOp::EqXor => "^", AssignOp::EqShiftL => "<<=",
                AssignOp::EqShiftR => ">>=",
            };
            format!("\t{} {} {};", unparse_lexpr(lhs, string_tab), fmt_op,
                    unparse_expr(rhs, string_tab))
        },
        Stmt::IfStmt(cond, thn, els) => {
            let mut fmt_els = String::new();
            match els {
                None => {},
                Some(bd) => {
                    fmt_els = format!(" else {{\n{}\t}}", unparse_stmt(bd, string_tab));
                },
            }
            format!("\tif ({}) {{\n{}\t}}{}\n", unparse_expr(cond, string_tab),
                    unparse_stmt(thn, string_tab), fmt_els)
        },
        Stmt::MatchStmt(val, cases) => {
            let mut fmt_cases : Vec<String> = vec![];
            for (pats, body) in cases {
                let mut fmt_pats : Vec<String> = vec![];
                for pat in pats {
                    fmt_pats.push(unparse_pattern(pat, string_tab));
                }
                fmt_cases.push(format!("\t{} => {}\n", fmt_cases.join(" | "),
                               unparse_stmt(body, string_tab)));
            }
            format!("\tmatch ({}) {{\n{}}}\n", unparse_expr(val, string_tab),
                    fmt_cases.join("\n"))
        },
        Stmt::ForLoop(var, init, end, step, body) => {
            format!("\tfor ({} = {} to {} by {}) {{\n{}}}\n",
                    unparse_varbinding(var, string_tab), unparse_expr(init, string_tab),
                    unparse_expr(end, string_tab), unparse_expr(step, string_tab),
                    unparse_stmt(body, string_tab))
        },
        Stmt::While(cond, body) => {
            format!("\twhile ({}) {{\n{}}}\n", unparse_expr(cond, string_tab),
                    unparse_stmt(body, string_tab))
        },
        Stmt::Return(val) => format!("\treturn {};\n", unparse_expr(val, string_tab)),
        Stmt::Block(stmts) => {
            let mut fmt_stmts : Vec<String> = vec![];
            for stmt in stmts {
                fmt_stmts.push(unparse_stmt(stmt, string_tab));
            }
            format!("\t{{\n{}}}\n", fmt_stmts.join(""))
        },
    }
}

fn unparse_top<'a>(top : &TopLevel, string_tab : &HashMap<u64, String>) -> String {
    match top {
        TopLevel::Import(name, star) => {
            format!("import {}{};\n", package_name_to_string(name, string_tab),
                    if *star { "::*" } else { "" })
        },
        TopLevel::TypeDecl(public, name, ty_vars, def) => {
            format!("{}type {}{} = {};\n", if *public { "pub " } else { "" },
                    id_to_string(name, string_tab), unparse_type_vars(ty_vars, string_tab),
                    unparse_type_def(def, string_tab))
        },
        TopLevel::ConstDecl(public, name, ty_annt, def) => {
            let mut fmt_type = String::new();
            match ty_annt {
                None => {},
                Some(t) => fmt_type = format!(" : {}", unparse_type(t, string_tab)),
            }
            format!("{}const {}{} = {};\n", if *public { "pub " } else { "" },
                    id_to_string(name, string_tab), fmt_type, unparse_expr(def, string_tab))
        },
        TopLevel::FuncDecl(attr, public, name, ty_vars, args, ret_ty, def) => {
            let mut fmt_args : Vec<String> = vec![];
            for (is_inout, var) in args {
                fmt_args.push(format!("{}{}", if *is_inout { "inout " } else { "" },
                                      unparse_varbinding(var, string_tab)));
            }
            let mut fmt_attr : String = String::new();
            match attr {
                None => {},
                Some(i) => fmt_attr = id_to_string(i, string_tab),
            }
            let mut fmt_ret = String::new();
            match ret_ty {
                None => {},
                Some(t) => fmt_ret = format!(" : {}", unparse_type(t, string_tab)),
            }
            format!("{}{}fn {}{}({}){} \n{}\n", fmt_attr, if *public { "pub " } else { "" },
                    id_to_string(name, string_tab), unparse_type_vars(ty_vars, string_tab),
                    fmt_args.join(", "), fmt_ret, unparse_stmt(def, string_tab))
        },
        TopLevel::Module(public, name, body) => {
            let mut fmt_body : Vec<String> = vec![];
            for t in body {
                fmt_body.push(unparse_top(t, string_tab));
            }
            format!("{}module {} {{\n{}}}\n", if *public { "pub " } else { "" },
                    id_to_string(name, string_tab),
                    fmt_body.join(""))
        },
    }
}

pub fn unparse_program<'a>(prg : &Program) -> String {
    let mut fmt_body : Vec<String> = vec![];
    for top in prg.program.iter() {
        fmt_body.push(unparse_top(top, &prg.string_tab));
    }
    fmt_body.join("")
}

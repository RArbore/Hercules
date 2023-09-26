%start Program

%token UNARY
%avoid_insert "FUNC_ATTR" "DOT_NUM" "ID" "INT" "HEX_INT" "BIN_INT" "OCT_INT" "FLOAT"
%expect-unused Unmatched 'UNMATCHED' 'UNARY'

%nonassoc ')'
%nonassoc 'else'
%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%nonassoc '==' '!='
%nonassoc '<' '<=' '>' '>='
%left '<<' '>>'
%left '+' '-'
%left '*' '/' '%'
%left 'as' 'size'
%right '~' '!' 'UNARY'
%left '.' 'DOT_NUM' '[' ']'

%%
Program -> Result<Prg, ()>
  : { Ok(vec![]) }
  | Program Top { flatten($1, $2) }
  ;

Top -> Result<Top, ()>
  : Import    { $1 }
  | TypeDecl  { $1 }
  | ConstDecl { $1 }
  | FuncDecl  { $1 }
  | Module    { $1 }
  ;

PubOption -> Result<bool, ()>
  :       { Ok(true) }
  | 'pub' { Ok(false) }
  ;

Module -> Result<Top, ()>
  : PubOption 'module' 'ID' '{' Program '}'
    { Ok(Top::ModDecl{ span : $span, public : $1?, name : span_of_tok($3)?, body : $5? }) };

PackageName -> Result<PackageName, ()>
  : 'ID'                  { Ok(vec![span_of_tok($1)?]) }
  | PackageName '::' 'ID' { flatten($1, span_of_tok($3)) }
  ;

PackageId -> Result<ImportName, ()>
  : PackageName           { Ok(($1?, None)) }
  | PackageName '::' '*'  { Ok(($1?, Some(span_of_tok($3)?))) }
  ;

Import -> Result<Top, ()>
  : 'use' PackageId ';' { Ok(Top::Import{ span : $span, name : $2? }) };

TypeVars -> Result<Vec<TypeVar>, ()>
  :                   { Ok(vec![]) }
  | '<' TypeVarsI '>' { $2 }
  ;
TypeVarsI -> Result<Vec<TypeVar>, ()>
  :                         { Ok(vec![]) }
  | TypeVar                 { Ok(vec![$1?]) }
  | TypeVarsIs ',' TypeVar  { flatten($1, $3) }
  ;
TypeVarsIs -> Result<Vec<TypeVar>, ()>
  : TypeVar                 { Ok(vec![$1?]) }
  | TypeVarsIs ',' TypeVar  { flatten($1, $3) }
  ;
TypeVar -> Result<TypeVar, ()>
  : 'ID'          { Ok(TypeVar{ span : $span, name : span_of_tok($1)?, kind : Kind::Type }) }
  | 'ID' ':' Kind { Ok(TypeVar{ span : $span, name : span_of_tok($1)?, kind : $3? }) }
  ;

Kind -> Result<Kind, ()>
  : 'type'    { Ok(Kind::Type) }
  | 'usize'   { Ok(Kind::USize) }
  | 'number'  { Ok(Kind::Number) }
  | 'integer' { Ok(Kind::Integer) }
  ;

TypeDecl -> Result<Top, ()>
  : PubOption 'type' 'ID' TypeVars '=' TypeDef ';'
    { Ok(Top::TypeDecl{ span : $span, public : $1?, name : span_of_tok($3)?, tyVars : $4?,
                        body : $6? } )};

TypeDef -> Result<TyDef, ()>
  : Type
      { Ok(TyDef::TypeAlias{ span : $span, body : $1? }) }
  | PubOption 'struct' '{' ObjFields '}'
      { Ok(TyDef::Struct{ span : $span, public : $1?, fields : $4? }) }
  | PubOption 'union'  '{' ObjFields '}'
      { Ok(TyDef::Union { span : $span, public : $1?, fields : $4? }) }
  ;

ObjFields -> Result<Vec<ObjField>, ()>
  :                     { Ok(vec![]) }
  | ObjFields ObjField  { flatten($1, $2) }
  ;
ObjField -> Result<ObjField, ()>
  : PubOption 'ID' ';'
        { Ok(ObjField{ span : $span, public : $1?, name : span_of_tok($2)?, typ : None }) }
  | PubOption 'ID' ':' Type ';'
        { Ok(ObjField{ span : $span, public : $1?, name : span_of_tok($2)?, typ : Some($4?) }) }
  ;

Type -> Result<Type, ()>
  : PrimType
        { Ok(Type::PrimType{ span : $span, typ : $1? }) }
  | '(' Types ')'
        { Ok(Type::TupleType{ span : $span, tys : $2? }) }
  | PackageName
        { Ok(Type::NamedType{ span : $span, name : $1?, args : vec![] }) }
  | PackageName '::' '<' TypeExprs '>'
        { Ok(Type::NamedType{ span : $span, name : $1?, args : $4? }) }
  | Type '[' Exprs ']'
        { Ok(Type::ArrayType{ span : $span, elem : Box::new($1?), dims : $3? }) }
  ;
Types -> Result<Vec<Type>, ()>
  :                   { Ok(vec![]) }
  | Type              { Ok(vec![$1?]) }
  | TypesS ',' Type   { flatten($1, $3) }
  ;
TypesS -> Result<Vec<Type>, ()>
  : Type              { Ok(vec![$1?]) }
  | TypesS ',' Type   { flatten($1, $3) }
  ;
PrimType -> Result<Primitive, ()>
  : 'bool'  { Ok(Primitive::Bool) }
  | 'i8'    { Ok(Primitive::I8) }
  | 'u8'    { Ok(Primitive::U8) }
  | 'i16'   { Ok(Primitive::I16) }
  | 'u16'   { Ok(Primitive::U16) }
  | 'i32'   { Ok(Primitive::I32) }
  | 'u32'   { Ok(Primitive::U32) }
  | 'i64'   { Ok(Primitive::I64) }
  | 'u64'   { Ok(Primitive::U64) }
  | 'usize' { Ok(Primitive::USize) }
  | 'f32'   { Ok(Primitive::F32) }
  | 'f64'   { Ok(Primitive::F64) }
  | 'void'  { Ok(Primitive::Void) }
  ;

ConstDecl -> Result<Top, ()>
  : PubOption 'const' 'ID' '=' Expr ';'
        { Ok(Top::ConstDecl{ span : $span, public : $1?, name : span_of_tok($3)?, ty : None,
                             body : $5? }) }
  | PubOption 'const' 'ID' ':' Type '=' Expr ';'
        { Ok(Top::ConstDecl{ span : $span, public : $1?, name : span_of_tok($3)?, ty : Some($5?),
                             body : $7? }) }
  ;

FuncDecl -> Result<Top, ()>
  : PubOption 'fn' 'ID' TypeVars '(' Arguments ')' Stmt
      { Ok(Top::FuncDecl{ span : $span, public : $1?, attr : None, name : span_of_tok($3)?,
                          tyVars : $4?, args : $6?, ty : None, body : $8? }) }
  | 'FUNC_ATTR' PubOption 'fn' 'ID' TypeVars '(' Arguments ')' Stmt
      { Ok(Top::FuncDecl{ span : $span, public : $2?, attr : Some(span_of_tok($1)?),
                          name : span_of_tok($4)?, tyVars : $5?, args : $7?, ty : None,
                          body : $9? }) }
  | PubOption 'fn' 'ID' TypeVars '(' Arguments ')' ':' Type Stmt
      { Ok(Top::FuncDecl{ span : $span, public : $1?, attr : None, name : span_of_tok($3)?,
                          tyVars : $4?, args : $6?, ty : Some($9?), body : $10? }) }
  | 'FUNC_ATTR' PubOption 'fn' 'ID' TypeVars '(' Arguments ')' ':' Type Stmt
      { Ok(Top::FuncDecl{ span : $span, public : $2?, attr : Some(span_of_tok($1)?),
                          name : span_of_tok($4)?, tyVars : $5?, args : $7?, ty : Some($10?),
                          body : $11? }) }
  ;
Arguments -> Result<Vec<(Option<Span>, VarBind)>, ()>
  :                        { Ok(vec![]) }
  | ArgBind                { Ok(vec![$1?]) }
  | ArgumentsS ',' ArgBind { flatten($1, $3) }
  ;
ArgumentsS -> Result<Vec<(Option<Span>, VarBind)>, ()>
  : ArgBind                { Ok(vec![$1?]) }
  | ArgumentsS ',' ArgBind { flatten($1, $3) }
  ;
ArgBind -> Result<(Option<Span>, VarBind), ()>
  : 'inout' VarBind { Ok((Some(span_of_tok($1)?), $2?)) }
  | VarBind         { Ok((None, $1?)) }
  ;

VarBind -> Result<VarBind, ()>
  : SPattern          { Ok(VarBind{ span : $span, pattern : $1?, typ : None }) }
  | SPattern ':' Type { Ok(VarBind{ span : $span, pattern : $1?, typ : Some($3?) }) }
  ;

Pattern -> Result<Pattern, ()>
  : SPattern              { Ok(Pattern::SimplePattern{ span : $span, pat : $1? }) }
  | PackageName SPattern  { Ok(Pattern::UnionPattern{ span : $span, name : $1?, pat : $2? }) }
  ;
SPattern -> Result<SPattern, ()>
  : '_'                   { Ok(SPattern::Wildcard{ span : $span }) }
  | IntLit                { let (span, base) = $1?;
                            Ok(SPattern::IntLit{ span : span, base : base }) }
  | PackageName           { Ok(SPattern::Variable{ span : $span, name : $1? }) }
  | '(' PatternsComma ')' { Ok(SPattern::TuplePat{ span : $span, pats : $2? }) }
  | '{' PatternsComma '}' { Ok(SPattern::OrderedStructPat{ span : $span, pats : $2? }) }
  | '{' NamePatterns  '}' { Ok(SPattern::NamedStructPat{ span : $span, pats : $2? }) }
  ;
PatternsComma -> Result<Vec<Pattern>, ()>
  :                             { Ok(vec![]) }
  | Pattern                     { Ok(vec![$1?]) }
  | PatternsCommaS ',' Pattern  { flatten($1, $3) }
  ;
PatternsCommaS -> Result<Vec<Pattern>, ()>
  : Pattern                     { Ok(vec![$1?]) }
  | PatternsCommaS ',' Pattern  { flatten($1, $3) }
  ;
NamePatterns -> Result<Vec<(Id, Pattern)>, ()>
  : 'ID' '=' Pattern                    { Ok(vec![(span_of_tok($1)?, $3?)]) }
  | NamePatternsS ',' 'ID' '=' Pattern  { flatten($1, res_pair(span_of_tok($3), $5)) }
  ;
NamePatternsS -> Result<Vec<(Id, Pattern)>, ()>
  : 'ID' '=' Pattern                    { Ok(vec![(span_of_tok($1)?, $3?)]) }
  | NamePatternsS ',' 'ID' '=' Pattern  { flatten($1, res_pair(span_of_tok($3), $5)) }
  ;

Stmt -> Result<Stmt, ()>
  : 'let' VarBind ';'
      { Ok(Stmt::LetStmt{ span : $span, var : $2?, init : None }) }
  | 'let' VarBind '=' Expr ';'
      { Ok(Stmt::LetStmt{ span : $span, var : $2?, init : Some($4?) }) }
  | 'const' VarBind '=' Expr ';'
      { Ok(Stmt::ConstStmt{ span : $span, var : $2?, init : $4? }) }
  | LExpr   '=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::None,
                             assignSpan : span_of_tok($2)?, rhs : $3? }) }
  | LExpr  '+=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::Add,
                             assignSpan : span_of_tok($2)?, rhs : $3? }) }
  | LExpr  '-=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::Sub,
                             assignSpan : span_of_tok($2)?, rhs : $3? }) }
  | LExpr  '*=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::Mul,
                             assignSpan : span_of_tok($2)?, rhs : $3? }) }
  | LExpr  '/=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::Div,
                             assignSpan : span_of_tok($2)?, rhs : $3? }) }
  | LExpr  '%=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::Mod,
                             assignSpan : span_of_tok($2)?, rhs : $3? }) }
  | LExpr  '&=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::BitAnd,
                             assignSpan : span_of_tok($2)?, rhs : $3? }) }
  | LExpr  '|=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::BitOr,
                             assignSpan : span_of_tok($2)?, rhs : $3? }) }
  | LExpr  '^=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::Xor,
                             assignSpan : span_of_tok($2)?, rhs : $3? }) }
  | LExpr '&&=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::LogAnd,
                             assignSpan : span_of_tok($2)?, rhs : $3? }) }
  | LExpr '||=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::LogOr,
                             assignSpan : span_of_tok($2)?, rhs : $3? }) }
  | LExpr '<<=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::LShift,
                             assignSpan : span_of_tok($2)?, rhs : $3? }) }
  | LExpr '>>=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::RShift,
                             assignSpan : span_of_tok($2)?, rhs : $3? }) }
  | 'if' '(' Expr ')' Stmt
      { Ok(Stmt::IfStmt{ span : $span, cond : $3?, thn : Box::new($5?), els : None }) }
  | 'if' '(' Expr ')' Stmt 'else' Stmt
      { Ok(Stmt::IfStmt{ span : $span, cond : $3?, thn : Box::new($5?),
                         els : Some(Box::new($7?)) }) }
  | 'match' '(' Expr ')' Cases
      { Ok(Stmt::MatchStmt{ span : $span, expr : $3?, body : $5? }) }
  | 'for' '(' VarBind '=' Expr 'to' Expr ')' Stmt
      { Ok(Stmt::ForStmt{ span : $span, var : $3?, init : $5?, bound : $7?, step : None,
                          body : Box::new($9?) }) }
  | 'for' '(' VarBind '=' Expr 'to' Expr 'by' Expr ')' Stmt
      { Ok(Stmt::ForStmt{ span : $span, var : $3?, init : $5?, bound : $7?, step : Some($9?),
                          body : Box::new($11?) }) }
  | 'while' '(' Expr ')' Stmt
      { Ok(Stmt::WhileStmt{ span : $span, cond : $3?, body : Box::new($5?) }) }
  | 'return' Expr ';'
      { Ok(Stmt::ReturnStmt{ span : $span, expr : $2?}) }
  | '{' Stmts '}'
      { Ok(Stmt::BlockStmt{ span : $span, body : $2? }) }
  | PackageName '(' Params ')' ';'
      { Ok(Stmt::CallStmt{ span : $span, name : $1?, tyArgs : vec![], args : $3? }) }
  | PackageName '::' '<' TypeExprs '>' '(' Params ')' ';'
      { Ok(Stmt::CallStmt{ span : $span, name : $1?, tyArgs : $4?, args : $7? }) }
  ;
Stmts -> Result<Vec<Stmt>, ()>
  :             { Ok(vec![]) }
  | Stmts Stmt  { flatten($1, $2) }
  ;

Cases -> Result<Vec<Case>, ()>
  : Case              { Ok(vec![]) }
  | '{' CaseList '}'  { $2 }
  ;
CaseList -> Result<Vec<Case>, ()>
  : Case          { Ok(vec![$1?]) }
  | CaseList Case { flatten($1, $2) }
  ;
Case -> Result<Case, ()>
  : Patterns '=>' Stmt { Ok(Case{ span : $span, pat : $1?, body : $3? }) };
Patterns -> Result<Vec<Pattern>, ()>
  : Pattern               { Ok(vec![$1?]) }
  | '|' Pattern           { Ok(vec![$2?]) }
  | Patterns '|' Pattern  { flatten($1, $3) }
  ;

LExpr -> Result<LExpr, ()>
  : 'ID'                { Ok(LExpr::VariableLExpr{ span : span_of_tok($1)? }) }
  | LExpr '.' 'ID'      { Ok(LExpr::FieldLExpr{ span : $span, lhs : Box::new($1?),
                                                rhs : span_of_tok($3)? }) }
  | LExpr 'DOT_NUM'     { Ok(LExpr::NumFieldLExpr { span : $span, lhs : Box::new($1?),
                                                    rhs : span_of_tok($2)? }) }
  | LExpr '[' Exprs ']' { Ok(LExpr::IndexLExpr{ span : $span, lhs : Box::new($1?), index : $3? }) }
  ;

IntLit -> Result<(Span, IntBase), ()>
  : 'INT'     { Ok(($span, IntBase::Decimal)) }
  | 'HEX_INT' { Ok(($span, IntBase::Hexadecimal)) }
  | 'BIN_INT' { Ok(($span, IntBase::Binary)) }
  | 'OCT_INT' { Ok(($span, IntBase::Octal)) }
  ;

Exprs -> Result<Vec<Expr>, ()>
  :                 { Ok(vec![]) }
  | Expr            { Ok(vec![$1?]) }
  | ExprsS ',' Expr { flatten($1, $3) }
  ;
ExprsS -> Result<Vec<Expr>, ()>
  : Expr            { Ok(vec![$1?]) }
  | ExprsS ',' Expr { flatten($1, $3) }
  ;

Expr -> Result<Expr, ()>
  : PackageName
      { Ok(Expr::Variable{ span : $span, name : $1? }) }
  | Expr '.' 'ID'
      { Ok(Expr::Field{ span : $span, lhs : Box::new($1?), rhs : span_of_tok($3)? }) }
  | Expr 'DOT_NUM'
      { Ok(Expr::NumField{ span : $span, lhs : Box::new($1?), rhs : span_of_tok($2)? }) }
  | Expr '[' Exprs ']'
      { Ok(Expr::ArrIndex{ span : $span, lhs : Box::new($1?), index : $3? }) }
  | '(' Exprs ')'
      { Ok(Expr::Tuple{ span : $span, exprs : $2? }) }
  | '{' Exprs '}'
      { Ok(Expr::OrderedStruct{ span : $span, exprs : $2? }) }
  | '{' IdExprs '}'
      { Ok(Expr::NamedStruct{ span : $span, exprs : $2? }) }
  | 'true'
      { Ok(Expr::BoolLit{ span : $span, value : true }) }
  | 'false'
      { Ok(Expr::BoolLit{ span : $span, value : false }) }
  | IntLit
      { let (span, base) = $1?;
        Ok(Expr::IntLit{ span : span, base : base }) }
  | 'FLOAT'
      { Ok(Expr::FloatLit{ span : $span }) }
  | '-' Expr %prec 'UNARY'
      { Ok(Expr::UnaryExpr{ span : $span, op : UnaryOp::Negation, expr : Box::new($2?)}) }
  | '~' Expr
      { Ok(Expr::UnaryExpr{ span : $span, op : UnaryOp::BitwiseNot, expr : Box::new($2?)}) }
  | '!' Expr
      { Ok(Expr::UnaryExpr{ span : $span, op : UnaryOp::LogicalNot, expr : Box::new($2?)}) }
  | Expr 'as' Type
      { Ok(Expr::CastExpr{ span : $span, expr : Box::new($1?), typ : $3?}) }
  | 'size' Expr
      { Ok(Expr::SizeExpr{ span : $span, expr : Box::new($2?)}) }
  | Expr '+' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Add, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '-' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Sub, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '*' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Mul, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '/' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Div, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '%' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Mod, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '&' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::BitAnd, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '&&' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::LogAnd, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '|' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::BitOr, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '||' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::LogOr, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '^' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Xor, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '<' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Lt, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '<=' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Le, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '>' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Gt, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '>=' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Ge, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '==' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Eq, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '!=' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Neq, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '<<' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::LShift, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | Expr '>>' Expr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::RShift, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | 'if' Expr 'then' Expr 'else' Expr
      { Ok(Expr::CondExpr{ span: $span, cond : Box::new($2?), thn : Box::new($4?), els : Box::new($6?) })}
  | PackageName '(' Params ')'
      { Ok(Expr::CallExpr{ span : $span, name : $1?, tyArgs : vec![], args: $3? }) }
  | PackageName '::' '<' TypeExprs '>' '(' Params ')'
      { Ok(Expr::CallExpr{ span : $span, name : $1?, tyArgs : $4?, args: $7? }) }
  ;
IdExprs -> Result<Vec<(Id, Expr)>, ()>
  : 'ID' '=' Expr               { Ok(vec![(span_of_tok($1)?, $3?)]) }
  | IdExprsS ',' 'ID' '=' Expr  { flatten($1, res_pair(span_of_tok($3), $5)) }
  ;
IdExprsS -> Result<Vec<(Id, Expr)>, ()>
  : 'ID' '=' Expr               { Ok(vec![(span_of_tok($1)?, $3?)]) }
  | IdExprsS ',' 'ID' '=' Expr  { flatten($1, res_pair(span_of_tok($3), $5)) }
  ;
Params -> Result<Vec<(bool, Expr)>, ()>
  :                       { Ok(vec![]) }
  | Expr                  { Ok(vec![(false, $1?)]) }
  | '&' Expr              { Ok(vec![(true, $2?)]) }
  | ParamsS ',' Expr      { flatten($1, Ok((false, $3?))) }
  | ParamsS ',' '&' Expr  { flatten($1, Ok((true, $4?))) }
  ;
ParamsS -> Result<Vec<(bool, Expr)>, ()>
  : Expr                  { Ok(vec![(false, $1?)]) }
  | '&' Expr              { Ok(vec![(true, $2?)]) }
  | ParamsS ',' Expr      { flatten($1, Ok((false, $3?))) }
  | ParamsS ',' '&' Expr  { flatten($1, Ok((true, $4?))) }
  ;

TypeExprs -> Result<Vec<TypeExpr>, ()>
  :                         { Ok(vec![]) }
  | TypeExpr                { Ok(vec![$1?]) }
  | TypeExprsS ',' TypeExpr { flatten($1, $3) }
  ;
TypeExprsS -> Result<Vec<TypeExpr>, ()>
  : TypeExpr                { Ok(vec![$1?]) }
  | TypeExprsS ',' TypeExpr { flatten($1, $3) }
  ;
TypeExpr -> Result<TypeExpr, ()>
  : PrimType
      { Ok(TypeExpr::PrimType{ span : $span, typ : $1? }) }
  | '(' Types ')'
      { Ok(TypeExpr::TupleType{ span : $span, tys : $2? }) }
  | PackageName
      { Ok(TypeExpr::NamedTypeExpr{ span : $span, name : $1?, args : vec![]}) }
  | PackageName '::' '<' TypeExprs '>'
      { Ok(TypeExpr::NamedTypeExpr{ span : $span, name : $1?, args : $4? }) }
  | TypeExpr '[' Exprs ']'
      { Ok(TypeExpr::ArrayTypeExpr{ span : $span, elem : Box::new($1?), dims : $3? }) }
  | 'true'
      { Ok(TypeExpr::BoolLiteral{ span : $span, val : true}) }
  | 'false'
      { Ok(TypeExpr::BoolLiteral{ span : $span, val : false}) }
  | IntLit
      { let (span, base) = $1?;
        Ok(TypeExpr::IntLiteral{ span : span, base : base }) }
  | '-' TypeExpr %prec UNARY
      { Ok(TypeExpr::Negative{ span : $span, expr : Box::new($2?)}) }
  | TypeExpr '+' TypeExpr
      { Ok(TypeExpr::Add{ span : $span, lhs : Box::new($1?), rhs : Box::new($3?) }) }
  | TypeExpr '-' TypeExpr
      { Ok(TypeExpr::Sub{ span : $span, lhs : Box::new($1?), rhs : Box::new($3?) }) }
  | TypeExpr '*' TypeExpr
      { Ok(TypeExpr::Mul{ span : $span, lhs : Box::new($1?), rhs : Box::new($3?) }) }
  ;

Unmatched -> (): 'UNMATCHED' { };
%%

use cfgrammar::Span;
use lrlex::DefaultLexeme;

fn flatten<T>(lhs: Result<Vec<T>, ()>, rhs: Result<T, ()>) -> Result<Vec<T>, ()> {
  let mut flt = lhs?;
  flt.push(rhs?);
  Ok(flt)
}

fn span_of_tok(t : Result<DefaultLexeme, DefaultLexeme>) -> Result<Span, ()> {
  t.map_err(|_| ()).map(|l| l.span())
}

fn res_pair<A, B>(x : Result<A, ()>, y : Result<B, ()>) -> Result<(A, B), ()> {
  Ok((x?, y?))
}

pub type Prg = Vec<Top>;
pub type Id = Span;
pub type PackageName = Vec<Span>;
pub type ImportName  = (PackageName, Option<Span>); // option is the wildcard *

#[derive(Debug)]
pub enum Kind { Type, USize, Number, Integer }
#[derive(Debug)]
pub enum Primitive { Bool, I8, U8, I16, U16, I32, U32, I64, U64, USize, F32, F64, Void }
#[derive(Debug)]
pub enum AssignOp { None, Add, Sub, Mul, Div, Mod, BitAnd, BitOr, Xor, LogAnd, LogOr,
                    LShift, RShift }
#[derive(Debug)]
pub enum IntBase { Binary, Octal, Decimal, Hexadecimal }
#[derive(Debug)]
pub enum UnaryOp { Negation, BitwiseNot, LogicalNot }
#[derive(Debug)]
pub enum BinaryOp { Add, Sub, Mul, Div, Mod, BitAnd, LogAnd, BitOr, LogOr, Xor,
                    Lt, Le, Gt, Ge, Eq, Neq, LShift, RShift }

#[derive(Debug)]
pub struct ObjField { span : Span, public : bool, name : Id, typ : Option<Type> }
#[derive(Debug)]
pub struct TypeVar { span : Span, name : Id, kind : Kind }
#[derive(Debug)]
pub struct VarBind { span : Span, pattern : SPattern, typ : Option<Type> }
#[derive(Debug)]
pub struct Case { span : Span, pat : Vec<Pattern>, body : Stmt }

#[derive(Debug)]
pub enum Top {
  Import    { span : Span, name : ImportName },
  TypeDecl  { span : Span, public : bool, name : Id, tyVars : Vec<TypeVar>, body : TyDef },
  ConstDecl { span : Span, public : bool, name : Id, ty : Option<Type>, body : Expr },
  FuncDecl  { span : Span, public : bool, attr : Option<Span>, name : Id, tyVars : Vec<TypeVar>,
              args : Vec<(Option<Span>, VarBind)>, // option is for inout
              ty : Option<Type>, body : Stmt },
  ModDecl   { span : Span, public : bool, name : Id, body : Vec<Top> },
}

#[derive(Debug)]
pub enum TyDef {
  TypeAlias { span : Span, body : Type },
  Struct    { span : Span, public : bool, fields : Vec<ObjField> },
  Union     { span : Span, public : bool, fields : Vec<ObjField> },
}

#[derive(Debug)]
pub enum Type {
  PrimType  { span : Span, typ : Primitive },
  TupleType { span : Span, tys : Vec<Type> },
  NamedType { span : Span, name : PackageName, args : Vec<TypeExpr> },
  ArrayType { span : Span, elem : Box<Type>, dims : Vec<Expr> },
}

#[derive(Debug)]
pub enum Stmt {
  LetStmt    { span : Span, var : VarBind, init : Option<Expr> },
  ConstStmt  { span : Span, var : VarBind, init : Expr },
  AssignStmt { span : Span, lhs : LExpr, assign : AssignOp, assignSpan : Span, rhs : Expr },
  IfStmt     { span : Span, cond : Expr, thn : Box<Stmt>, els : Option<Box<Stmt>> },
  MatchStmt  { span : Span, expr : Expr, body : Vec<Case> },
  ForStmt    { span : Span, var : VarBind, init : Expr, bound : Expr, step : Option<Expr>,
               body : Box<Stmt> },
  WhileStmt  { span : Span, cond : Expr, body : Box<Stmt> },
  ReturnStmt { span : Span, expr : Expr },
  BlockStmt  { span : Span, body : Vec<Stmt> },
  CallStmt   { span : Span, name : PackageName, tyArgs : Vec<TypeExpr>,
               args : Vec<(bool, Expr)> }, // bool indicates & (for inouts)
}

#[derive(Debug)]
pub enum Pattern {
  SimplePattern { span : Span, pat : SPattern },
  UnionPattern  { span : Span, name : PackageName, pat : SPattern },
}

#[derive(Debug)]
pub enum SPattern {
  Wildcard         { span : Span },
  IntLit           { span : Span, base : IntBase },
  Variable         { span : Span, name : PackageName },
  TuplePat         { span : Span, pats : Vec<Pattern> },
  OrderedStructPat { span : Span, pats : Vec<Pattern> },
  NamedStructPat   { span : Span, pats : Vec<(Id, Pattern)> },
}

#[derive(Debug)]
pub enum LExpr {
  VariableLExpr { span : Span },
  FieldLExpr    { span : Span, lhs : Box<LExpr>, rhs : Id },
  NumFieldLExpr { span : Span, lhs : Box<LExpr>, rhs : Span },
  IndexLExpr    { span : Span, lhs : Box<LExpr>, index : Vec<Expr> },
}

#[derive(Debug)]
pub enum Expr {
  Variable      { span : Span, name : PackageName },
  Field         { span : Span, lhs : Box<Expr>, rhs : Id },
  NumField      { span : Span, lhs : Box<Expr>, rhs : Span },
  ArrIndex      { span : Span, lhs : Box<Expr>, index : Vec<Expr> },
  Tuple         { span : Span, exprs : Vec<Expr> },
  OrderedStruct { span : Span, exprs : Vec<Expr> },
  NamedStruct   { span : Span, exprs : Vec<(Id, Expr)> },
  BoolLit       { span : Span, value : bool },
  IntLit        { span : Span, base : IntBase },
  FloatLit      { span : Span },
  UnaryExpr     { span : Span, op : UnaryOp, expr : Box<Expr> },
  BinaryExpr    { span : Span, op : BinaryOp, lhs : Box<Expr>, rhs : Box<Expr> },
  CastExpr      { span : Span, expr : Box<Expr>, typ : Type },
  SizeExpr      { span : Span, expr : Box<Expr> },
  CondExpr      { span : Span, cond : Box<Expr>, thn : Box<Expr>, els : Box<Expr> },
  CallExpr      { span : Span, name : PackageName, tyArgs : Vec<TypeExpr>,
                  args : Vec<(bool, Expr)> }, // bool indicates & (for inouts)
}

#[derive(Debug)]
pub enum TypeExpr {
  PrimType        { span : Span, typ : Primitive },
  TupleType       { span : Span, tys : Vec<Type> },
  NamedTypeExpr   { span : Span, name : PackageName, args : Vec<TypeExpr> },
  ArrayTypeExpr   { span : Span, elem : Box<TypeExpr>, dims : Vec<Expr> },
  BoolLiteral     { span : Span, val : bool },
  IntLiteral      { span : Span, base : IntBase },
  Negative        { span : Span, expr : Box<TypeExpr> },
  Add             { span : Span, lhs : Box<TypeExpr>, rhs : Box<TypeExpr> },
  Sub             { span : Span, lhs : Box<TypeExpr>, rhs : Box<TypeExpr> },
  Mul             { span : Span, lhs : Box<TypeExpr>, rhs : Box<TypeExpr> },
}

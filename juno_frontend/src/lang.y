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
  :       { Ok(false) }
  | 'pub' { Ok(true) }
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
  | '<' TypeVarsI '>' { Ok($2?.into_iter().flatten().collect()) }
  ;
TypeVarsI -> Result<VecDeque<Vec<TypeVar>>, ()>
  :                         { Ok(VecDeque::new()) }
  | TypeVar                 { Ok(VecDeque::from([$1?])) }
  | TypeVar ',' TypeVarsI   { cons_deque($1, $3) }
  ;
TypeVar -> Result<Vec<TypeVar>, ()>
  : IdList ':' Kind { let kind = $3?;
                      Ok($1?.into_iter()
                            .map(|n| TypeVar { span : $span, name : n, kind : kind })
                            .collect()) }
  ;
IdList -> Result<Vec<Id>, ()>
  : 'ID'            { Ok(vec![span_of_tok($1)?]) }
  | IdList ',' 'ID' { flatten($1, span_of_tok($3)) }
  ;

Kind -> Result<Kind, ()>
  : 'type'    { Ok(Kind::Type) }
  | 'usize'   { Ok(Kind::USize) }
  | 'number'  { Ok(Kind::Number) }
  | 'integer' { Ok(Kind::Integer) }
  ;

TypeDecl -> Result<Top, ()>
  : PubOption 'type' 'ID' TypeVars '=' TypeDef ';'
    { Ok(Top::TypeDecl{ span : $span, public : $1?, name : span_of_tok($3)?, ty_vars : $4?,
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
  | Type '[' TypeExprs ']'
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
  : PubOption 'fn' 'ID' TypeVars '(' Arguments ')' Stmts
      { Ok(Top::FuncDecl{ span : $span, public : $1?, attr : None, name : span_of_tok($3)?,
                          ty_vars : $4?, args : $6?, ty : None, body : $8? }) }
  | 'FUNC_ATTR' PubOption 'fn' 'ID' TypeVars '(' Arguments ')' Stmts
      { Ok(Top::FuncDecl{ span : $span, public : $2?, attr : Some(span_of_tok($1)?),
                          name : span_of_tok($4)?, ty_vars : $5?, args : $7?, ty : None,
                          body : $9? }) }
  | PubOption 'fn' 'ID' TypeVars '(' Arguments ')' '->' Type Stmts
      { Ok(Top::FuncDecl{ span : $span, public : $1?, attr : None, name : span_of_tok($3)?,
                          ty_vars : $4?, args : $6?, ty : Some($9?), body : $10? }) }
  | 'FUNC_ATTR' PubOption 'fn' 'ID' TypeVars '(' Arguments ')' '->' Type Stmts
      { Ok(Top::FuncDecl{ span : $span, public : $2?, attr : Some(span_of_tok($1)?),
                          name : span_of_tok($4)?, ty_vars : $5?, args : $7?, ty : Some($10?),
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
  : Pattern          { Ok(VarBind{ span : $span, pattern : $1?, typ : None }) }
  | Pattern ':' Type { Ok(VarBind{ span : $span, pattern : $1?, typ : Some($3?) }) }
  ;

Pattern -> Result<Pattern, ()>
  : '_'                   { Ok(Pattern::Wildcard { span : $span }) }
  | IntLit                { let (span, base) = $1?;
                            Ok(Pattern::IntLit { span : span, base : base }) }
  | PackageName           { Ok(Pattern::Variable { span : $span, name : $1? }) }
  | '(' PatternsComma ')' { Ok(Pattern::TuplePattern { span : $span, pats : $2? }) }
  | PackageName '{' NamePatterns '}'
                          { Ok(Pattern::StructPattern { span : $span, name : $1?, pats : $3? }) }
  | PackageName '(' PatternsComma ')'
                          { Ok(Pattern::UnionPattern { span : $span, name : $1?, pats : $3? }) }
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
  : 'ID' ':' Pattern                    { Ok(vec![(span_of_tok($1)?, $3?)]) }
  | NamePatternsS ',' 'ID' ':' Pattern  { flatten($1, res_pair(span_of_tok($3), $5)) }
  ;
NamePatternsS -> Result<Vec<(Id, Pattern)>, ()>
  : 'ID' ':' Pattern                    { Ok(vec![(span_of_tok($1)?, $3?)]) }
  | NamePatternsS ',' 'ID' ':' Pattern  { flatten($1, res_pair(span_of_tok($3), $5)) }
  ;

Stmt -> Result<Stmt, ()>
  : 'let' VarBind ';'
      { Ok(Stmt::LetStmt{ span : $span, var : $2?, init : None }) }
  | 'let' VarBind '=' Expr ';'
      { Ok(Stmt::LetStmt{ span : $span, var : $2?, init : Some($4?) }) }
  | 'const' VarBind ';'
      { Ok(Stmt::ConstStmt{ span : $span, var : $2?, init : None }) }
  | 'const' VarBind '=' Expr ';'
      { Ok(Stmt::ConstStmt{ span : $span, var : $2?, init : Some($4?) }) }
  | LExpr   '=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::None,
                             assign_span : span_of_tok($2)?, rhs : $3? }) }
  | LExpr  '+=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::Add,
                             assign_span : span_of_tok($2)?, rhs : $3? }) }
  | LExpr  '-=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::Sub,
                             assign_span : span_of_tok($2)?, rhs : $3? }) }
  | LExpr  '*=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::Mul,
                             assign_span : span_of_tok($2)?, rhs : $3? }) }
  | LExpr  '/=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::Div,
                             assign_span : span_of_tok($2)?, rhs : $3? }) }
  | LExpr  '%=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::Mod,
                             assign_span : span_of_tok($2)?, rhs : $3? }) }
  | LExpr  '&=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::BitAnd,
                             assign_span : span_of_tok($2)?, rhs : $3? }) }
  | LExpr  '|=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::BitOr,
                             assign_span : span_of_tok($2)?, rhs : $3? }) }
  | LExpr  '^=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::Xor,
                             assign_span : span_of_tok($2)?, rhs : $3? }) }
  | LExpr '&&=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::LogAnd,
                             assign_span : span_of_tok($2)?, rhs : $3? }) }
  | LExpr '||=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::LogOr,
                             assign_span : span_of_tok($2)?, rhs : $3? }) }
  | LExpr '<<=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::LShift,
                             assign_span : span_of_tok($2)?, rhs : $3? }) }
  | LExpr '>>=' Expr ';'
      { Ok(Stmt::AssignStmt{ span : $span, lhs : $1?, assign : AssignOp::RShift,
                             assign_span : span_of_tok($2)?, rhs : $3? }) }
  | IfStmt
      { $1 }
  | 'match' NonStructExpr Cases
      { Ok(Stmt::MatchStmt{ span : $span, expr : $2?, body : $3? }) }
  | 'for' VarBind '=' NonStructExpr 'to' NonStructExpr Stmts
      { Ok(Stmt::ForStmt{ span : $span, var : $2?, init : $4?, bound : $6?, step : None,
                          body : Box::new($7?) }) }
  | 'for' VarBind '=' NonStructExpr 'to' NonStructExpr 'by' SignedIntLit Stmts
      { Ok(Stmt::ForStmt{ span : $span, var : $2?, init : $4?, bound : $6?, step : Some($8?),
                          body : Box::new($9?) }) }
  | 'while' NonStructExpr Stmts
      { Ok(Stmt::WhileStmt{ span : $span, cond : $2?, body : Box::new($3?) }) }
  | 'return' ';'
      { Ok(Stmt::ReturnStmt{ span : $span, expr : None }) }
  | 'return' Expr ';'
      { Ok(Stmt::ReturnStmt{ span : $span, expr : Some($2?)}) }
  | 'break' ';'
      { Ok(Stmt::BreakStmt{ span : $span }) }
  | 'continue' ';'
      { Ok(Stmt::ContinueStmt{ span : $span }) }
  | Stmts
      { $1 }
  | PackageName '(' Params ')' ';'
      { Ok(Stmt::CallStmt{ span : $span, name : $1?, ty_args : vec![], args : $3? }) }
  | PackageName '::' '<' TypeExprs '>' '(' Params ')' ';'
      { Ok(Stmt::CallStmt{ span : $span, name : $1?, ty_args : $4?, args : $7? }) }
  ;
Stmts -> Result<Stmt, ()>
  : '{' StmtList '}'  { Ok(Stmt::BlockStmt{ span : $span, body : $2? }) };
StmtList -> Result<Vec<Stmt>, ()>
  :                { Ok(vec![]) }
  | StmtList Stmt  { flatten($1, $2) }
  ;

IfStmt -> Result<Stmt, ()>
  : 'if' NonStructExpr Stmts
      { Ok(Stmt::IfStmt{ span : $span, cond : $2?, thn : Box::new($3?), els : None }) }
  | 'if' NonStructExpr Stmts 'else' IfStmt
      { Ok(Stmt::IfStmt{ span : $span, cond : $2?, thn : Box::new($3?),
                         els : Some(Box::new($5?)) }) }
  | 'if' NonStructExpr Stmts 'else' Stmts
      { Ok(Stmt::IfStmt{ span : $span, cond : $2?, thn : Box::new($3?),
                         els : Some(Box::new($5?)) }) }
  ;

Cases -> Result<Vec<Case>, ()>
  : '{' CaseList '}'  { $2 }
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

SignedIntLit -> Result<(bool, Span, IntBase), ()>
  : '+' IntLit { Ok((false, $2?.0, $2?.1)) }
  |     IntLit { Ok((false, $1?.0, $1?.1)) }
  | '-' IntLit { Ok((true, $2?.0, $2?.1)) }
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
  | PackageName '{' IdExprs '}'
      { Ok(Expr::Struct{ span : $span, name : $1?, ty_args : vec![], exprs : $3? }) }
  | PackageName '::' '<' TypeExprs '>' '{' IdExprs '}'
      { Ok(Expr::Struct{ span : $span, name : $1?, ty_args : $4?, exprs : $7? }) }
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
      { Ok(Expr::CallExpr{ span : $span, name : $1?, ty_args : vec![], args: $3? }) }
  | PackageName '::' '<' TypeExprs '>' '(' Params ')'
      { Ok(Expr::CallExpr{ span : $span, name : $1?, ty_args : $4?, args: $7? }) }
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

NonStructExpr -> Result<Expr, ()>
  : PackageName
      { Ok(Expr::Variable{ span : $span, name : $1? }) }
  | NonStructExpr '.' 'ID'
      { Ok(Expr::Field{ span : $span, lhs : Box::new($1?), rhs : span_of_tok($3)? }) }
  | NonStructExpr 'DOT_NUM'
      { Ok(Expr::NumField{ span : $span, lhs : Box::new($1?), rhs : span_of_tok($2)? }) }
  | NonStructExpr '[' Exprs ']'
      { Ok(Expr::ArrIndex{ span : $span, lhs : Box::new($1?), index : $3? }) }
  | '(' Exprs ')'
      { Ok(Expr::Tuple{ span : $span, exprs : $2? }) }
  | 'true'
      { Ok(Expr::BoolLit{ span : $span, value : true }) }
  | 'false'
      { Ok(Expr::BoolLit{ span : $span, value : false }) }
  | IntLit
      { let (span, base) = $1?;
        Ok(Expr::IntLit{ span : span, base : base }) }
  | 'FLOAT'
      { Ok(Expr::FloatLit{ span : $span }) }
  | '-' NonStructExpr %prec 'UNARY'
      { Ok(Expr::UnaryExpr{ span : $span, op : UnaryOp::Negation, expr : Box::new($2?)}) }
  | '~' NonStructExpr
      { Ok(Expr::UnaryExpr{ span : $span, op : UnaryOp::BitwiseNot, expr : Box::new($2?)}) }
  | '!' NonStructExpr
      { Ok(Expr::UnaryExpr{ span : $span, op : UnaryOp::LogicalNot, expr : Box::new($2?)}) }
  | NonStructExpr 'as' Type
      { Ok(Expr::CastExpr{ span : $span, expr : Box::new($1?), typ : $3?}) }
  | NonStructExpr '+' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Add, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '-' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Sub, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '*' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Mul, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '/' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Div, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '%' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Mod, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '&' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::BitAnd, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '&&' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::LogAnd, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '|' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::BitOr, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '||' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::LogOr, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '^' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Xor, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '<' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Lt, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '<=' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Le, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '>' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Gt, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '>=' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Ge, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '==' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Eq, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '!=' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::Neq, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '<<' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::LShift, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | NonStructExpr '>>' NonStructExpr
      { Ok(Expr::BinaryExpr{ span: $span, op: BinaryOp::RShift, lhs: Box::new($1?), rhs: Box::new($3?)}) }
  | 'if' NonStructExpr 'then' NonStructExpr 'else' NonStructExpr
      { Ok(Expr::CondExpr{ span: $span, cond : Box::new($2?), thn : Box::new($4?), els : Box::new($6?) })}
  | PackageName '(' Params ')'
      { Ok(Expr::CallExpr{ span : $span, name : $1?, ty_args : vec![], args: $3? }) }
  | PackageName '::' '<' TypeExprs '>' '(' Params ')'
      { Ok(Expr::CallExpr{ span : $span, name : $1?, ty_args : $4?, args: $7? }) }
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
  | '(' TypeExprs ')'
      { Ok(TypeExpr::TupleType{ span : $span, tys : $2? }) }
  | PackageName
      { Ok(TypeExpr::NamedTypeExpr{ span : $span, name : $1?, args : vec![]}) }
  | PackageName '::' '<' TypeExprs '>'
      { Ok(TypeExpr::NamedTypeExpr{ span : $span, name : $1?, args : $4? }) }
  | TypeExpr '[' TypeExprs ']'
      { Ok(TypeExpr::ArrayTypeExpr{ span : $span, elem : Box::new($1?), dims : $3? }) }
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
use std::collections::VecDeque;

fn flatten<T>(lhs: Result<Vec<T>, ()>, rhs: Result<T, ()>) -> Result<Vec<T>, ()> {
  let mut flt = lhs?;
  flt.push(rhs?);
  Ok(flt)
}

fn cons_deque<T>(lhs : Result<T, ()>, rhs : Result<VecDeque<T>, ()>) -> Result<VecDeque<T>, ()> {
  let mut lst = rhs?;
  lst.push_front(lhs?);
  Ok(lst)
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

#[derive(Debug, Copy, Clone)]
pub enum Kind { Type, USize, Number, Integer }
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Primitive { Bool, I8, U8, I16, U16, I32, U32, I64, U64, USize, F32, F64, Void }
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AssignOp { None, Add, Sub, Mul, Div, Mod, BitAnd, BitOr, Xor, LogAnd, LogOr,
                    LShift, RShift }
#[derive(Debug, Copy, Clone)]
pub enum IntBase { Binary, Octal, Decimal, Hexadecimal }
#[derive(Debug, Copy, Clone)]
pub enum UnaryOp { Negation, BitwiseNot, LogicalNot }
#[derive(Debug, Copy, Clone)]
pub enum BinaryOp { Add, Sub, Mul, Div, Mod, BitAnd, LogAnd, BitOr, LogOr, Xor,
                    Lt, Le, Gt, Ge, Eq, Neq, LShift, RShift }

#[derive(Debug)]
pub struct ObjField { pub span : Span, pub public : bool, pub name : Id, pub typ : Option<Type> }
#[derive(Debug)]
pub struct TypeVar { pub span : Span, pub name : Id, pub kind : Kind }
#[derive(Debug)]
pub struct VarBind { pub span : Span, pub pattern : Pattern, pub typ : Option<Type> }
#[derive(Debug)]
pub struct Case { pub span : Span, pub pat : Vec<Pattern>, pub body : Stmt }

#[derive(Debug)]
pub enum Top {
  Import    { span : Span, name : ImportName },
  TypeDecl  { span : Span, public : bool, name : Id, ty_vars : Vec<TypeVar>, body : TyDef },
  ConstDecl { span : Span, public : bool, name : Id, ty : Option<Type>, body : Expr },
  FuncDecl  { span : Span, public : bool, attr : Option<Span>, name : Id, ty_vars : Vec<TypeVar>,
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
  ArrayType { span : Span, elem : Box<Type>, dims : Vec<TypeExpr> },
}

#[derive(Debug)]
pub enum Stmt {
  LetStmt    { span : Span, var : VarBind, init : Option<Expr> },
  ConstStmt  { span : Span, var : VarBind, init : Option<Expr> },
  AssignStmt { span : Span, lhs : LExpr, assign : AssignOp, assign_span : Span, rhs : Expr },
  IfStmt     { span : Span, cond : Expr, thn : Box<Stmt>, els : Option<Box<Stmt>> },
  MatchStmt  { span : Span, expr : Expr, body : Vec<Case> },
  // The step records: negative, number, base
  ForStmt    { span : Span, var : VarBind, init : Expr, bound : Expr, step : Option<(bool, Span, IntBase)>,
               body : Box<Stmt> },
  WhileStmt  { span : Span, cond : Expr, body : Box<Stmt> },
  ReturnStmt { span : Span, expr : Option<Expr> },
  BreakStmt  { span : Span },
  ContinueStmt { span : Span },
  BlockStmt    { span : Span, body : Vec<Stmt> },
  CallStmt     { span : Span, name : PackageName, ty_args : Vec<TypeExpr>,
                 args : Vec<(bool, Expr)> }, // bool indicates & (for inouts)
}

#[derive(Debug)]
pub enum Pattern {
  Wildcard         { span : Span },
  IntLit           { span : Span, base : IntBase },
  Variable         { span : Span, name : PackageName },
  TuplePattern     { span : Span, pats : Vec<Pattern> },
  StructPattern    { span : Span, name : PackageName, pats : Vec<(Id, Pattern)> },
  UnionPattern     { span : Span, name : PackageName, pats : Vec<Pattern> },
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
  Struct        { span : Span, name : PackageName, ty_args : Vec<TypeExpr>,
                  exprs : Vec<(Id, Expr)> },
  BoolLit       { span : Span, value : bool },
  IntLit        { span : Span, base : IntBase },
  FloatLit      { span : Span },
  UnaryExpr     { span : Span, op : UnaryOp, expr : Box<Expr> },
  BinaryExpr    { span : Span, op : BinaryOp, lhs : Box<Expr>, rhs : Box<Expr> },
  CastExpr      { span : Span, expr : Box<Expr>, typ : Type },
  CondExpr      { span : Span, cond : Box<Expr>, thn : Box<Expr>, els : Box<Expr> },
  CallExpr      { span : Span, name : PackageName, ty_args : Vec<TypeExpr>,
                  args : Vec<(bool, Expr)> }, // bool indicates & (for inouts)
}

#[derive(Debug)]
pub enum TypeExpr {
  PrimType        { span : Span, typ : Primitive },
  TupleType       { span : Span, tys : Vec<TypeExpr> },
  NamedTypeExpr   { span : Span, name : PackageName, args : Vec<TypeExpr> },
  ArrayTypeExpr   { span : Span, elem : Box<TypeExpr>, dims : Vec<TypeExpr> },
  IntLiteral      { span : Span, base : IntBase },
  Negative        { span : Span, expr : Box<TypeExpr> },
  Add             { span : Span, lhs : Box<TypeExpr>, rhs : Box<TypeExpr> },
  Sub             { span : Span, lhs : Box<TypeExpr>, rhs : Box<TypeExpr> },
  Mul             { span : Span, lhs : Box<TypeExpr>, rhs : Box<TypeExpr> },
}

pub trait Spans {
  fn span(&self) -> Span;
}

impl Spans for Expr {
  fn span(&self) -> Span {
    match self {
        Expr::Variable      { span, .. }
      | Expr::Field         { span, .. }
      | Expr::NumField      { span, .. }
      | Expr::ArrIndex      { span, .. }
      | Expr::Tuple         { span, .. }
      | Expr::Struct        { span, .. }
      | Expr::BoolLit       { span, .. }
      | Expr::IntLit        { span, .. }
      | Expr::FloatLit      { span }
      | Expr::UnaryExpr     { span, .. }
      | Expr::BinaryExpr    { span, .. }
      | Expr::CastExpr      { span, .. }
      | Expr::CondExpr      { span, .. }
      | Expr::CallExpr      { span, .. }
        => *span
    }
  }
}

impl Spans for Stmt {
  fn span(&self) -> Span {
    match self {
        Stmt::LetStmt      { span, .. }
      | Stmt::ConstStmt    { span, .. }
      | Stmt::AssignStmt   { span, .. }
      | Stmt::IfStmt       { span, .. }
      | Stmt::MatchStmt    { span, .. }
      | Stmt::ForStmt      { span, .. }
      | Stmt::WhileStmt    { span, .. }
      | Stmt::ReturnStmt   { span, .. }
      | Stmt::BreakStmt    { span, .. }
      | Stmt::ContinueStmt { span, .. }
      | Stmt::BlockStmt    { span, .. }
      | Stmt::CallStmt     { span, .. }
        => *span
    }
  }
}

impl Spans for TypeExpr {
  fn span(&self) -> Span {
    match self {
      TypeExpr::PrimType          { span, .. }
      | TypeExpr::TupleType       { span, .. }
      | TypeExpr::NamedTypeExpr   { span, .. }
      | TypeExpr::ArrayTypeExpr   { span, .. }
      | TypeExpr::IntLiteral      { span, .. }
      | TypeExpr::Negative        { span, .. }
      | TypeExpr::Add             { span, .. }
      | TypeExpr::Sub             { span, .. }
      | TypeExpr::Mul             { span, .. }
        => *span
    }
  }
}

impl IntBase {
  pub fn base(&self) -> u32 {
    match self {
      IntBase::Binary      => 2,
      IntBase::Octal       => 8,
      IntBase::Decimal     => 10,
      IntBase::Hexadecimal => 16,
    }
  }
}

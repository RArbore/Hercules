%start Program

%token ELSE "else"
%token ORS  "||"
%token ANDS "&&"
%token OR   "|"
%token AND  "&"
%token XOR  "^"
%token EQS  "=="
%token NEQ  "!="
%token LT   "<"
%token LE   "<="
%token GT   ">"
%token GE   ">="
%token LSHF "<<"
%token RSHF ">>"
%token ADD  "+"
%token SUB  "-"
%token MUL  "*"
%token DIV  "/"
%token MOD  "%"
%token AS   "as"
%token SIZE "size"
%token TILD "~"
%token BANG "!"

%nonassoc ")"
%nonassoc "else"
%left "||"
%left "&&"
%left "|"
%left "^"
%left "&"
%nonassoc "==" "!="
%nonassoc "<" "<=" ">" ">="
%left "<<" ">>"
%left "+" "-"
%left "*" "/" "%"
%left "as" "size"
%right "~" "!" UNARY
%left "." "DOT_NUM" "[" "]"

%%
Program
  : {}
  | Program Top {}
  ;

Top
  : Import {}
  | TypeDecl {}
  | ConstDecl {}
  | FuncDecl {}
  | Module {}
  ;

PubOption
  : {}
  | "pub" {}
  ;

Module : PubOption "module" "ID" "{" Program "}" {};

PackageName
  : "ID" {}
  | PackageName "::" "ID" {}
  ;

PackageId
  : PackageName {}
  | PackageName "::" "*" {}
  ;

Import : "use" PackageId ";" {};

TypeVars
  : {}
  | "<" TypeVarsI ">" {}
  ;
TypeVarsI
  : {}
  | TypeVar {}
  | TypeVarsIs "," TypeVar {}
  ;
TypeVarsIs
  : TypeVar {}
  | TypeVarsIs "," TypeVar {}
  ;
TypeVar
  : "ID" {}
  | "ID" ":" Kind {}
  ;

Kind
  : "type" {}
  | "size" {}
  | "number" {}
  | "integer" {}
  ;

TypeDecl : PubOption "type" "ID" TypeVars "=" TypeDef ";" {};

TypeDef
  : Type {}
  | PubOption "struct" "{" ObjFields "}" {}
  | PubOption "union"  "{" ObjFields "}" {}
  ;

ObjFields
  : {}
  | ObjFields ObjField {}
  ;
ObjField
  : PubOption "ID" ";" {}
  | PubOption "ID" ":" Type ";" {}
  ;

Type
  : PrimType {}
  | "(" Types ")" {}
  | PackageName {}
  | PackageName "::" "<" TypeExprs ">" {}
  | Type "[" Exprs "]" {}
  ;
Types
  : {}
  | Type {}
  | TypesS "," Type {}
  ;
TypesS
  : Type {}
  | TypesS "," Type {}
  ;
PrimType
  : "bool" {}
  | "i8" {}
  | "u8" {}
  | "i16" {}
  | "u16" {}
  | "i32" {}
  | "u32" {}
  | "i64" {}
  | "u64" {}
  | "f32" {}
  | "f64" {}
  | "void" {}
  ;

ConstDecl
  : PubOption "const" "ID" "=" Expr ";" {}
  | PubOption "const" "ID" ":" Type "=" Expr ";" {}
  ;

FuncDecl
  : PubOption "fn" "ID" TypeVars "(" Arguments ")" Stmt {}
  | "FUNC_ATTR" PubOption "fn" "ID" TypeVars "(" Arguments ")" Stmt {}
  | PubOption "fn" "ID" TypeVars "(" Arguments ")" ":" Type Stmt {}
  | "FUNC_ATTR" PubOption "fn" "ID" TypeVars "(" Arguments ")" ":" Type Stmt {}
  ;
Arguments
  : {}
  | ArgBind {}
  | ArgumentsS "," ArgBind {}
  ;
ArgumentsS
  : ArgBind {}
  | ArgumentsS "," ArgBind {}
  ;
ArgBind
  : "inout" VarBind {}
  | VarBind         {}
  ;

VarBind
  : SPattern {}
  | SPattern ":" Type {}
  ;

Pattern
  : SPattern {}
  | PackageName SPattern {}
  ;
SPattern
  : "_" {}
  | IntLit {}
  | PackageName {}
  | "(" PatternsComma ")" {}
  | "{" PatternsComma "}" {}
  | "{" NamePatterns  "}" {}
  ;
PatternsComma
  : {}
  | Pattern {}
  | PatternsCommaS "," Pattern {}
  ;
PatternsCommaS
  : Pattern {}
  | PatternsCommaS "," Pattern {}
  ;
NamePatterns
  : "ID" "=" Pattern {}
  | NamePatternsS "," "ID" "=" Pattern {}
  ;
NamePatternsS
  : "ID" "=" Pattern {}
  | NamePatternsS "," "ID" "=" Pattern {}
  ;

Stmt
  : "let" VarBind ";" {}
  | "let" VarBind "=" Expr ";" {}
  | "const" VarBind "=" Expr ";" {}
  | LExpr   "=" Expr ";" {}
  | LExpr  "+=" Expr ";" {}
  | LExpr  "-=" Expr ";" {}
  | LExpr  "*=" Expr ";" {}
  | LExpr  "/=" Expr ";" {}
  | LExpr  "%=" Expr ";" {}
  | LExpr  "&=" Expr ";" {}
  | LExpr  "|=" Expr ";" {}
  | LExpr  "^=" Expr ";" {}
  | LExpr "&&=" Expr ";" {}
  | LExpr "||=" Expr ";" {}
  | LExpr "<<=" Expr ";" {}
  | LExpr ">>=" Expr ";" {}
  | "if" "(" Expr ")" Stmt {}
  | "if" "(" Expr ")" Stmt "else" Stmt {}
  | "match" "(" Expr ")" Cases {}
  | "for" "(" VarBind "=" Expr "to" Expr ")" Stmt {}
  | "for" "(" VarBind "=" Expr "to" Expr "by" Expr ")" Stmt {}
  | "while" "(" Expr ")" Stmt {}
  | "return" Expr ";" {}
  | "{" Stmts "}" {}
  | PackageName "(" Params ")" ";" {}
  | PackageName "::" "<" TypeExprs ">" "(" Params ")" ";" {}
  ;
Stmts
  : {}
  | Stmts Stmt {}
  ;

Cases
  : Case {}
  | "{" CaseList "}" {}
  ;
CaseList
  : Case {}
  | CaseList Case {}
  ;
Case : Patterns "=>" Stmt {};
Patterns
  : Pattern {}
  | "|" Pattern {}
  | Patterns "|" Pattern {}
  ;

LExpr
  : "ID" {}
  | LExpr "." "ID" {}
  | LExpr "DOT_NUM" {}
  | LExpr "[" Exprs "]" {}
  ;

IntLit
  : "INT" {}
  | "HEX_INT" {}
  | "BIN_INT" {}
  | "OCT_INT" {}
  ;

Exprs
  : {}
  | Expr {}
  | ExprsS "," Expr {}
  ;
ExprsS
  : Expr {}
  | ExprsS "," Expr {}
  ;

Expr
  : PackageName {}
  | Expr "." "ID" {}
  | Expr "DOT_NUM" {}
  | Expr "[" Exprs "]" {}
  | "(" Exprs ")" {}
  | "{" Exprs "}" {}
  | "{" IdExprs "}" {}
  | "true" {}
  | "false" {}
  | IntLit {}
  | "FLOAT" {}
  | "-" Expr %prec UNARY {}
  | "~" Expr {}
  | "!" Expr {}
  | Expr "as" Type {}
  | "size" Expr {}
  | Expr "+" Expr {}
  | Expr "-" Expr {}
  | Expr "*" Expr {}
  | Expr "/" Expr {}
  | Expr "%" Expr {}
  | Expr "&" Expr {}
  | Expr "&&" Expr {}
  | Expr "|" Expr {}
  | Expr "||" Expr {}
  | Expr "^" Expr {}
  | Expr "<" Expr {}
  | Expr "<=" Expr {}
  | Expr ">" Expr {}
  | Expr ">=" Expr {}
  | Expr "==" Expr {}
  | Expr "!=" Expr {}
  | Expr "<<" Expr {}
  | Expr ">>" Expr {}
  | "if" Expr "then" Expr "else" Expr {}
  | PackageName "(" Params ")" {}
  | PackageName "::" "<" TypeExprs ">" "(" Params ")" {}
  ;
IdExprs
  : "ID" "=" Expr {}
  | IdExprsS "," "ID" "=" Expr {}
  ;
IdExprsS
  : "ID" "=" Expr {}
  | IdExprsS "," "ID" "=" Expr {}
  ;
Params
  : {}
  | Expr {}
  | "&" Expr {}
  | ParamsS "," Expr {}
  | ParamsS "," "&" Expr {}
  ;
ParamsS
  : Expr {}
  | "&" Expr {}
  | ParamsS "," Expr {}
  | ParamsS "," "&" Expr {}
  ;

TypeExprs
  :                         {}
  | TypeExpr                {}
  | TypeExprsS "," TypeExpr {}
  ;
TypeExprsS
  : TypeExpr                {}
  | TypeExprsS "," TypeExpr {}
  ;
TypeExpr
  : PrimType                            {}
  | "(" TypeExprs ")"                   {}
  | PackageName                         {}
  | PackageName "::" "<" TypeExprs ">"  {}
  | TypeExpr "[" Exprs "]"              {}
  | "true"                              {}
  | "false"                             {}
  | IntLit                              {}
  | "-" TypeExpr %prec UNARY            {}
  | TypeExpr "+" TypeExpr               {}
  | TypeExpr "-" TypeExpr               {}
  | TypeExpr "*" TypeExpr               {}
  ;

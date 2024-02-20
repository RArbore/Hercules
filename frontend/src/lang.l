%x comment
%%
/\*    <+comment>;

<comment>/\*    <+comment>;
<comment>\*+/   <-comment>;
<comment>\*+    ;
<comment>[\n\r] ;
<comment>.      ;

//[^\n\r]*   ;

[\t ]+ ;
[\n\r] ;

#\[[^\]]*\] "FUNC_ATTR"

as       "as"
break    "break"
by       "by"
const    "const"
continue "continue"
else     "else"
false    "false"
fn       "fn"
for      "for"
if       "if"
inout    "inout"
integer  "integer"
let      "let"
match    "match"
module   "module"
number   "number"
pub      "pub"
return   "return"
struct   "struct"
then     "then"
to       "to"
true     "true"
type     "type"
union    "union"
use      "use"
while    "while"

bool     "bool"
i8       "i8"
i16      "i16"
i32      "i32"
i64      "i64"
u8       "u8"
u16      "u16"
u32      "u32"
u64      "u64"
usize    "usize"
f32      "f32"
f64      "f64"
void     "void"

\+=      "+="
\+       "+"

&&=      "&&="
&&       "&&"
&=       "&="
&        "&"

\|\|=    "||="
\|\|     "||"
\|=      "|="
\|       "|"

/=       "/="
/        "/"

%=       "%="
%        "%"

\<<=      "<<="
\<<       "<<"
\<=       "<="
\<        "<"

>>=      ">>="
>>       ">>"
>=       ">="
>        ">"

\*=      "*="
\*       "*"

-=       "-="
-        "-"

\^=      "^="
\^       "^"

=>       "=>"
==       "=="
=        "="

!=       "!="
!        "!"

::       "::"
:        ":"

,        ","
\.       "."
;        ";"
~        "~"
_        "_"

\(       "("
\)       ")"
\{       "{"
\}       "}"
\[       "["
\]       "]"

\.[0-9]+                  "DOT_NUM"
[a-zA-Z][a-zA-Z0-9_]*     "ID"
[0-9]+                    "INT"
0x[0-9a-fA-F]+            "HEX_INT"
0b[0-1]+                  "BIN_INT"
0o[0-7]+                  "OCT_INT"
[0-9]+\.[0-9]*(|e[0-9]+)  "FLOAT"

. "UNMATCHED"
. "UNARY"

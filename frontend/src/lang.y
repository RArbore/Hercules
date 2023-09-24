%start Expr
%expect-unused Unrecognized 'UNRECOGNIZED'

%left '+'
%left '*'

%%
Expr -> Result<u64, ()>:
    Expr '+' Expr { Ok($1? + $3?) }
  | Expr '*' Expr { Ok($1? * $3?) }
  | '(' Expr ')'  { $2 }
  | 'INT'         { let v = $1.map_err(|_| ())?;
                    parse_int($lexer.span_str(v.span())) }
  ;

Unrecognized -> (): 'UNRECOGNIZED' { };
%%
fn parse_int(s: &str) -> Result<u64, ()> {
  match s.parse::<u64>() {
    Ok(val) => Ok(val),
    Err(_) => {
      eprintln!("{} cannot be represented as a u64", s);
      Err(())
    }
  }
}

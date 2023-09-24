use lexer::{Lexer, Token};

// Current lexer (for fetching lines of code), the last token that was lexed,
// and the error message itself
pub fn fatal_error(lex : &mut Lexer, tok : Token, message : String) {
    eprintln!("Fatal Error. At line {}, column {}: {}", tok.line, tok.col, message);
    eprintln!("{}", lex.get_line(tok.line));
}

pub fn error(lex : &mut Lexer, tok : &Token, message : String) {
    eprintln!("Error. At line {}, column {}: {}", tok.line, tok.col, message);
    eprintln!("{}", lex.get_line(tok.line));
}

pub fn warning(lex : &mut Lexer, tok : &Token, message : String) {
    eprintln!("Warning. At line {}, column {}: {}", tok.line, tok.col, message);
    eprintln!("{}", lex.get_line(tok.line));
}

pub fn message(lex : &mut Lexer, tok : &Token, message : String) {
    eprintln!("Message (line {}, column {}): {}", tok.line, tok.col, message);
}

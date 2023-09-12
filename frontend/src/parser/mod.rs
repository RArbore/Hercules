use lexer::Lexer;
use lexer::tokens::TokenType;

pub mod ast;
use parser::ast::*;

pub mod stringtab;
use parser::stringtab::*;

pub enum ParseResult<A> {
    Valid(A), Error(A), Fatal()
}

fn parse_top<'a>(public : bool, lex : &mut Lexer, str_tbl : &mut StringTab) 
    -> ParseResult<TopLevel> {
    ParseResult::Fatal()
}

fn parse_func_top<'a>(attr : u64, lex : &mut Lexer, str_tbl : &mut StringTab)
    -> ParseResult<TopLevel> {
    ParseResult::Fatal()
}

fn parse_tops<'a>(lex : &mut Lexer, str_tbl : &mut StringTab)
    -> ParseResult<Vec<TopLevel>> {
    let mut result : Vec<TopLevel> = vec![];
    let mut someError = false;
    loop {
        let tok = lex.next_token();
        let res : ParseResult<TopLevel>;
        match tok.tok {
            TokenType::EOF => { break; }
            TokenType::FuncAttr => {
                let attr = lex.get_lexeme(&tok);
                let attr_content = &attr[2..attr.len()-1];
                let attr_id = str_tbl.add_string(attr_content);
                res = parse_func_top(attr_id, lex, str_tbl);
            },
            TokenType::Pub => {
                res = parse_top(true, lex, str_tbl);
            },
            _ => {
                lex.unlex(tok);
                res = parse_top(false, lex, str_tbl);
            },
        }
        match res {
            ParseResult::Valid(t) => { result.push(t); }
            ParseResult::Error(t) => { someError = true; result.push(t); },
            ParseResult::Fatal()  => { return ParseResult::Fatal(); },
        }
    }
    if someError { ParseResult::Error(result) }
            else { ParseResult::Valid(result) }
}

pub fn parse<'a>(mut lex : Lexer) -> ParseResult<Program> {
    let mut string_table = StringTab::new();
    let prg = parse_tops(&mut lex, &mut string_table);
    match prg {
        ParseResult::Valid(body) =>
            ParseResult::Valid(Program { string_tab : string_table.str_tbl, program : body }),
        ParseResult::Error(body) =>
            ParseResult::Error(Program { string_tab : string_table.str_tbl, program : body }),
        ParseResult::Fatal()     => ParseResult::Fatal(),
    }
}

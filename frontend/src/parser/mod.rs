use lexer::{Lexer, print_token};
use lexer::tokens::TokenType;
use errors::*;

pub mod ast;
use parser::ast::*;

pub mod stringtab;
use parser::stringtab::*;

pub enum ParseResult<A> {
    Valid(A), Error(A), Fatal()
}

fn parse_package_name<'a>(lex : &mut Lexer, str_tbl : &mut StringTab) -> ParseResult<Vec<u64>> {
    ParseResult::Fatal()
}

fn parse_import_name<'a>(lex : &mut Lexer, str_tbl : &mut StringTab)
    -> ParseResult<(Vec<u64>, bool)> {
    let mut name : Vec<u64> = vec![];
    loop {
        let tok = lex.next_token();
        match tok.tok {
            TokenType::Ident => {
                name.push(str_tbl.add_string(lex.get_lexeme(&tok)));
            },
            TokenType::Semicolon => {
                if name.is_empty() {
                    error(lex, &tok, format!("empty package name not valid"));
                } else {
                    error(lex, &tok, format!("no name following '::'"));
                }
                return ParseResult::Error((name, false));
            },
            TokenType::Star => {
                let tok = lex.next_token();
                match tok.tok {
                    TokenType::Semicolon => {
                        if name.is_empty() {
                            error(lex, &tok, format!("cannot use *"));
                            return ParseResult::Error((name, true));
                        } else {
                            return ParseResult::Valid((name, true));
                        }
                    },
                    TokenType::Error(_) => {
                        let msg = format!("{}", print_token(&tok, lex));
                        fatal_error(lex, tok, msg);
                        return ParseResult::Fatal();
                    },
                    _ => {
                        // TODO: recovery
                        let msg = format!("expected ';', found {}", print_token(&tok, lex));
                        fatal_error(lex, tok, msg);
                        return ParseResult::Fatal();
                    },
                }
            },
            TokenType::Error(_) => {
                let msg = format!("{}", print_token(&tok, lex));
                fatal_error(lex, tok, msg);
                return ParseResult::Fatal();
            },
            _ => {
                // TODO: recovery
                let msg = format!("expected package name or '*', found {}",
                                  print_token(&tok, lex));
                fatal_error(lex, tok, msg);
                return ParseResult::Fatal();
            },
        }
        let tok = lex.next_token();
        match tok.tok {
            TokenType::Colons => {},
            TokenType::Semicolon => {
                return ParseResult::Valid((name, false));
            },
            TokenType::Error(_) => {
                let msg = format!("{}", print_token(&tok, lex));
                fatal_error(lex, tok, msg);
                return ParseResult::Fatal();
            },
            _ => {
                // TODO: recovery
                let msg = format!("expected package name, '::', or ';', found {}",
                                  print_token(&tok, lex));
                fatal_error(lex, tok, msg);
                return ParseResult::Fatal();
            },
        }
    }
}

fn parse_func_def<'a>(attr : Option<u64>, public : bool, lex : &mut Lexer,
                      str_tbl : &mut StringTab) -> ParseResult<TopLevel> {
    ParseResult::Fatal()
}

fn parse_type_decl<'a>(public : bool, lex : &mut Lexer, str_tbl : &mut StringTab)
    -> ParseResult<TopLevel> {
    ParseResult::Fatal()
}

fn parse_const_decl<'a>(public : bool, lex : &mut Lexer, str_tbl : &mut StringTab)
    -> ParseResult<TopLevel> {
    ParseResult::Fatal()
}

fn parse_module<'a>(public : bool, lex : &mut Lexer, str_tbl : &mut StringTab)
    -> ParseResult<TopLevel> {
    let tok = lex.next_token();
    match tok.tok {
        TokenType::Ident => {
            let name = str_tbl.add_string(lex.get_lexeme(&tok));
            let body : Vec<TopLevel>;
            let error : bool;
            match parse_tops(lex, str_tbl) {
                ParseResult::Valid(b) => { body = b; error = false; },
                ParseResult::Error(b) => { body = b; error = true; },
                ParseResult::Fatal()  => { return ParseResult::Fatal(); },
            }
            match lex.next_token().tok {
                TokenType::BraceClose => {
                    if error { ParseResult::Error(TopLevel::Module(public, name, b)) }
                    else { ParseResult::Valid(TopLevel::Module(public, name, b)) }
                },
                TokenType::Error(_) => {
                    let msg = format!("{}", print_token(&tok, lex));
                    fatal_error(lex, tok, msg);
                    ParseResult::Fatal()
                },
                _ => {
                    // TODO: Recovery
                    fatal_error(lex, tok, "Expected '}' ending module");
                    ParseResult::Fatal()
                },
            }
        }
        TokenType::Error(_) => {
            let msg = format!("{}", print_token(&tok, lex));
            fatal_error(lex, tok, msg);
            ParseResult::Fatal()
        },
        _ => {
            // TODO: Recovery
            fatal_error(lex, tok, "Expected '{' ending module");
            ParseResult::Fatal()
        },
        // TODO: Errors
    }
}

fn parse_top<'a>(public : bool, lex : &mut Lexer, str_tbl : &mut StringTab) 
    -> ParseResult<TopLevel> {
    let tok = lex.next_token();
    match tok.tok {
        TokenType::EOF => {
            fatal_error(lex, tok, format!("Reachabled end-of-file, expected top-level declaration"));
            ParseResult::Fatal()
        },
        TokenType::Use => {
            let err : bool;
            if public {
                error(lex, &tok, format!("'use' directives cannot be declared 'pub'"));
                err = true;
            } else {
                err = false;
            }
            match parse_import_name(lex, str_tbl) {
                ParseResult::Valid((nm, star)) =>
                    if err {
                        ParseResult::Error(TopLevel::Import(nm, star))
                    } else {
                        ParseResult::Valid(TopLevel::Import(nm, star))
                    },
                ParseResult::Error((nm, star)) =>
                    ParseResult::Error(TopLevel::Import(nm, star)),
                ParseResult::Fatal() => ParseResult::Fatal(),
            }
        },
        TokenType::Fn => parse_func_def(None, public, lex, str_tbl),
        TokenType::Type => parse_type_decl(public, lex, str_tbl),
        TokenType::Const => parse_const_decl(public, lex, str_tbl),
        TokenType::Module => parse_module(public, lex, str_tbl),
        TokenType::Error(_) => {
            let msg = format!("{}", print_token(&tok, lex));
            fatal_error(lex, tok, msg);
            ParseResult::Fatal()
        },
        _ => {
            // TODO: Try to recover... This is hard to guess from here since
            // it could be that they forgot any of the keywords 'use', 'fn',
            // 'type', 'const', or 'module' and the next thing you'd read would
            // be an Identifer
            ParseResult::Fatal()
        },
    }
}

fn parse_func_top<'a>(attr : u64, lex : &mut Lexer, str_tbl : &mut StringTab)
    -> ParseResult<TopLevel> {
    let tok = lex.next_token();
    let public : bool;
    match tok.tok {
        TokenType::EOF => {
            fatal_error(lex, tok, format!("Reached end-of-file, expected function definition"));
            return ParseResult::Fatal();
        },
        TokenType::Pub => { public = true; },
        _ => { lex.unlex(tok); public = false; },
    }
    let tok = lex.next_token();
    match tok.tok {
        TokenType::EOF => {
            fatal_error(lex, tok, format!("Reached end-of-file, expected function definition"));
            ParseResult::Fatal()
        },
        TokenType::Fn => parse_func_def(Some(attr), public, lex, str_tbl),
        TokenType::Error(_) => {
            let msg = format!("{}", print_token(&tok, lex));
            fatal_error(lex, tok, msg);
            ParseResult::Fatal()
        },
        _ => {
            let msg = format!("Expected function definition, found '{}'", print_token(&tok, lex));
            error(lex, &tok, msg);
            let msg = format!("Continuing as if you forgot 'fn'");
            message(lex, &tok, msg);
            lex.unlex(tok);
            match parse_func_def(Some(attr), public, lex, str_tbl) {
                ParseResult::Valid(f) | ParseResult::Error(f) => ParseResult::Error(f),
                ParseResult::Fatal() => ParseResult::Fatal(),
            }
        },
    }
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
            TokenType::BraceClose => { lex.unlex(tok); break; }
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

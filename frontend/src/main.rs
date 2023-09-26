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
        Some(Ok(r)) => println!("Result: {:?}", r),
        _ => eprintln!("Fatal Error.")
    }
    /*
    match ast {
        ParseResult::Valid(prg) => println!("{}\n", unparse_program(&prg)),
        ParseResult::Error(_)   => eprintln!("Parse Error"),
        ParseResult::Fatal()    => eprintln!("Fatal Parse Error"),
    }
    */
}

extern crate clap;
extern crate cfgrammar;
extern crate lrlex;
extern crate lrpar;

use std::fs::File;

use clap::Parser;

use lrlex::lrlex_mod;
use lrpar::lrpar_mod;

lrlex_mod!("lang.l");
lrpar_mod!("lang.y");

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    src_file : String,
}

fn main() {
    let args = Cli::parse();
    let file = File::open(args.src_file).expect("PANIC: Unable to open input file.");

    let lexerdef = lang_l::lexerdef();
    let lexer = lexerdef.lexer(file);
    let (res, errs) = lang_y::parse(&lexer);
    for e in errs {
        eprintln!("ERROR: {}", e.pp(&lexer, &calc_y::token_epp));
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

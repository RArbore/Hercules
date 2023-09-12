extern crate clap;

use std::fs::File;

use clap::Parser;

mod lexer;
use lexer::Lexer;

mod parser;
use parser::{parse, ParseResult};
use parser::ast::unparse_program;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    src_file : String,
}

fn main() {
    let args = Cli::parse();
    let file = File::open(args.src_file).expect("PANIC: Unable to open input file.");
    let lexer = Lexer::new(file).expect("PANIC: Unable to read input file");
    let ast = parse(lexer);
    match ast {
        ParseResult::Valid(prg) => println!("{}\n", unparse_program(&prg)),
        ParseResult::Error(_)   => eprintln!("Parse Error"),
        ParseResult::Fatal()    => eprintln!("Fatal Parse Error"),
    }
}

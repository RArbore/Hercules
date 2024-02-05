extern crate clap;

use clap::Parser;

mod parser;
mod env;
use parser::*;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    src_file : String,
}

fn main() {
    let args = Cli::parse();
    let (main, strtab) = parse_program(args.src_file);
    match main {
        Some(main) => println!("Success"),
        _ => eprintln!("Fatal Error"),
    }
}

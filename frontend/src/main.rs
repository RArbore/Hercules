extern crate clap;

use clap::Parser;

mod codegen;
mod env;
mod ssa;
mod types;
mod semant;

use semant::ErrorMessage;
use codegen::*;

extern crate hercules_ir;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    src_file : String,
}

fn main() {
    let args = Cli::parse();
    let prg = semant::parse_and_analyze(args.src_file);
    match prg {
        Ok(prg) => {
            let mut module = codegen_program(prg);
            println!("{}", module);

            let _
                = hercules_ir::verify::verify(&mut module)
                    .expect("PANIC: Failed to verify Hercules IR modules.");
            println!("Verification Succeeded");
        },
        Err(errs) => {
            for err in errs{
                eprintln!("{}", err);
            }
        },
    }
}

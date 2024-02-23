extern crate clap;

use clap::Parser;

mod codegen;
mod env;
mod ssa;
mod types;
mod semant;
use codegen::*;

extern crate hercules_ir;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    src_file : String,
}

fn main() {
    let args = Cli::parse();
    let module = codegen::process_program(args.src_file);
    match module {
        Ok(mut module) => {
            println!("{}", module);

            let _
                = hercules_ir::verify::verify(&mut module)
                    .expect("PANIC: Failed to verify Hercules IR modules.");

            //let mut file = File::create("output.dot").expect("PANIC: Unable to open output file.");
            //let mut contents = String::new();
            //write_dot(&module, &reverse_postorders, &typing, &doms, &fork_join_maps,
            //          &mut contents)
            //    .expect("PANIC: Unable to generate output file contents.");
            //file.write_all(contents.as_bytes())
            //    .expect("PANIC: Unable to write output file contents.");
            println!("Verification Succeeded");
        },
        Err(errs) => {
            for err in errs {
                error_message(err);
            }
        },
    }
}

fn error_message(msg : ErrorMessage) {
    match msg {
        ErrorMessage::NotImplemented(((s_line, s_col), (e_line, e_col)), msg) => {
            eprintln!("Error ({s_line}, {s_col} -- {e_line}, {e_col}). Feature not implemented : {msg}");
        },
        ErrorMessage::SyntaxError(msg) => {
            eprintln!("Syntax Error : {msg}");
        },
        ErrorMessage::SemanticError(((s_line, s_col), (e_line, e_col)), msg) => {
            eprintln!("Error ({s_line}, {s_col} -- {e_line}, {e_col}). {msg}");
        },
        ErrorMessage::UndefinedVariable(((s_line, s_col), (e_line, e_col)), name) => {
            eprintln!("Error ({s_line}, {s_col} -- {e_line}, {e_col}). Undefined variable '{name}'");
        },
        ErrorMessage::KindError(((s_line, s_col), (e_line, e_col)), expected, actual) => {
            eprintln!("Error ({s_line}, {s_col} -- {e_line}, {e_col}). Expected {expected} but found {actual}");
        },
        ErrorMessage::TypeError(((s_line, s_col), (e_line, e_col)), expected, actual) => {
            eprintln!("Error ({s_line}, {s_col} -- {e_line}, {e_col}). Type error, expected {expected} but found {actual}");
        },
    }
}

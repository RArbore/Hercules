extern crate clap;

use clap::Parser;

mod codegen;
mod env;
mod ssa;
use codegen::*;

extern crate hercules_ir;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    src_file : String,
}

fn main() {
    let args = Cli::parse();
    let module = process_program(args.src_file);
    match module {
        Ok(mut module) => {
            println!("{}", module);

            let (_def_uses, reverse_postorders, typing, _subgraphs, doms,
                 _postdoms, fork_join_maps)
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
        ErrorMessage::NotImplemented(((sLine, sCol), (eLine, eCol)), msg) => {
            eprintln!("Error ({sLine}, {sCol} -- {eLine}, {eCol}). Feature not implemented : {msg}");
        },
        ErrorMessage::SyntaxError(msg) => {
            eprintln!("Syntax Error : {msg}");
        },
        ErrorMessage::SemanticError(((sLine, sCol), (eLine, eCol)), msg) => {
            eprintln!("Error ({sLine}, {sCol} -- {eLine}, {eCol}). {msg}");
        },
        ErrorMessage::UndefinedVariable(((sLine, sCol), (eLine, eCol)), name) => {
            eprintln!("Error ({sLine}, {sCol} -- {eLine}, {eCol}). Undefined variable '{name}'");
        },
        ErrorMessage::KindError(((sLine, sCol), (eLine, eCol)), expected, actual) => {
            eprintln!("Error ({sLine}, {sCol} -- {eLine}, {eCol}). Expected {expected} but found {actual}");
        },
        ErrorMessage::TypeError(((sLine, sCol), (eLine, eCol)), expected, actual) => {
            eprintln!("Error ({sLine}, {sCol} -- {eLine}, {eCol}). Type error, expected {expected} but found {actual}");
        },
    }
}

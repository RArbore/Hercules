extern crate clap;

use clap::Parser;

mod codegen;
mod env;
mod ssa;
use codegen::*;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    src_file : String,
}

fn main() {
    let args = Cli::parse();
    let module = process_program(args.src_file);
    match module {
        Ok(module) => println!("Success"),
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

extern crate clap;

use std::env::temp_dir;
use std::fs::File;
use std::io::prelude::*;
use std::process::Command;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    hir_file: String,

    #[arg(short, long, default_value_t = String::new())]
    output: String,
}

fn main() {
    let args = Args::parse();
    if !args.hir_file.ends_with(".hir") {
        eprintln!("WARNING: Running hercules_dot on a file without a .hir extension - interpreting as a textual Hercules IR file.");
    }

    let mut file = File::open(args.hir_file).expect("PANIC: Unable to open input file.");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("PANIC: Unable to read input file contents.");
    let module = hercules_ir::parse::parse(&contents);
    if args.output.is_empty() {
        let mut tmp_path = temp_dir();
        tmp_path.push("hercules_dot.dot");
        let mut file = File::create(tmp_path.clone()).expect("PANIC: Unable to open output file.");
        let mut contents = String::new();
        hercules_ir::dot::write_dot(&module, &mut contents)
            .expect("PANIC: Unable to generate output file contents.");
        file.write_all(contents.as_bytes())
            .expect("PANIC: Unable to write output file contents.");
        Command::new("xdot")
            .args([tmp_path])
            .output()
            .expect("PANIC: Couldn't execute xdot.");
    } else {
        let mut file = File::create(args.output).expect("PANIC: Unable to open output file.");
        let mut contents = String::new();
        hercules_ir::dot::write_dot(&module, &mut contents)
            .expect("PANIC: Unable to generate output file contents.");
        file.write_all(contents.as_bytes())
            .expect("PANIC: Unable to write output file contents.");
    }
}
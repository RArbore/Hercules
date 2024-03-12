extern crate clap;

use std::fs::File;
use std::io::prelude::*;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    hir_file: String,

    #[arg(short, long, default_value_t = String::new())]
    passes: String,
}

fn main() {
    let args = Args::parse();
    if !args.hir_file.ends_with(".hir") {
        eprintln!("WARNING: Running hercules_driver on a file without a .hir extension - interpreting as a textual Hercules IR file.");
    }

    let mut file = File::open(args.hir_file).expect("PANIC: Unable to open input file.");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("PANIC: Unable to read input file contents.");
    let module =
        hercules_ir::parse::parse(&contents).expect("PANIC: Failed to parse Hercules IR file.");

    let mut pm = hercules_opt::pass::PassManager::new(module);
    let passes: Vec<hercules_opt::pass::Pass> = args
        .passes
        .parse()
        .expect("PANIC: Couldn't parse list of passes.");
    for pass in passes {
        pm.add_pass(pass);
    }
    pm.run_passes();
}

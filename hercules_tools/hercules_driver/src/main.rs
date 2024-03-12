extern crate clap;

use std::fs::File;
use std::io::prelude::*;

use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    hir_file: String,
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
        .split(char::is_whitespace)
        .map(|pass_str| {
            assert_ne!(
                pass_str, "",
                "PANIC: Can't interpret empty pass name. Try giving a list of pass names."
            );
            ron::from_str(pass_str).expect("PANIC: Couldn't parse list of passes.")
        })
        .collect();
    for pass in passes {
        pm.add_pass(pass);
    }
    pm.run_passes();
}

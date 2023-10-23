extern crate clap;

use std::fs::File;
use std::io::prelude::*;

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
        eprintln!("WARNING: Running hercules_cpu on a file without a .hir extension - interpreting as a textual Hercules IR file.");
    }

    let mut file = File::open(args.hir_file).expect("PANIC: Unable to open input file.");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("PANIC: Unable to read input file contents.");
    let mut module =
        hercules_ir::parse::parse(&contents).expect("PANIC: Failed to parse Hercules IR file.");
    let (def_uses, reverse_postorders, _typing, _doms, _postdoms, _fork_join_maps) =
        hercules_ir::verify::verify(&mut module)
            .expect("PANIC: Failed to verify Hercules IR module.");

    let mut module = module.map(
        |(mut function, id), (types, mut constants, dynamic_constants)| {
            hercules_opt::ccp::ccp(
                &mut function,
                &mut constants,
                &def_uses[id.idx()],
                &reverse_postorders[id.idx()],
            );
            hercules_opt::dce::dce(&mut function);
            function.delete_gravestones();

            let def_use = hercules_ir::def_use::def_use(&function);
            hercules_opt::gvn::gvn(&mut function, &constants, &def_use);
            hercules_opt::dce::dce(&mut function);
            function.delete_gravestones();

            (function, (types, constants, dynamic_constants))
        },
    );
    let (def_uses, reverse_postorders, _typing, _doms, _postdoms, _fork_join_maps) =
        hercules_ir::verify::verify(&mut module)
            .expect("PANIC: Failed to verify Hercules IR module.");

    let _bbs: Vec<_> = module
        .functions
        .iter()
        .enumerate()
        .map(|(idx, function)| {
            hercules_codegen::gcm::gcm(function, &def_uses[idx], &reverse_postorders[idx])
        })
        .collect();
}

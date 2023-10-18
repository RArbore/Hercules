extern crate clap;
extern crate rand;

use std::env::temp_dir;
use std::fs::File;
use std::io::prelude::*;
use std::process::Command;

use clap::Parser;

use rand::Rng;

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
    let mut module =
        hercules_ir::parse::parse(&contents).expect("PANIC: Failed to parse Hercules IR file.");
    let (_def_use, reverse_postorders, _typing, _doms, _postdoms, _fork_join_maps) =
        hercules_ir::verify::verify(&mut module)
            .expect("PANIC: Failed to verify Hercules IR module.");

    let module = module.map(
        |(mut function, id), (types, mut constants, dynamic_constants)| {
            hercules_ir::ccp::ccp(&mut function, &mut constants, &reverse_postorders[id.idx()]);
            hercules_ir::dce::dce(&mut function);
            function.delete_gravestones();

            let def_use = hercules_ir::def_use::def_use(&function);
            hercules_ir::gvn::gvn(&mut function, &constants, &def_use);
            hercules_ir::dce::dce(&mut function);
            function.delete_gravestones();

            (function, (types, constants, dynamic_constants))
        },
    );

    if args.output.is_empty() {
        let mut tmp_path = temp_dir();
        let mut rng = rand::thread_rng();
        let num: u64 = rng.gen();
        tmp_path.push(format!("hercules_dot_{}.dot", num));
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

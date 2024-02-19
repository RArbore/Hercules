extern crate clap;
extern crate rand;

use std::env::temp_dir;
use std::fs::File;
use std::io::prelude::*;
use std::process::Command;

use clap::Parser;

use rand::Rng;

pub mod dot;
use dot::*;

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
    let (def_uses, reverse_postorders, _typing, _subgraphs, _doms, _postdoms, _fork_join_maps) =
        hercules_ir::verify::verify(&mut module)
            .expect("PANIC: Failed to verify Hercules IR module.");

    let mut module = module.map(
        |(mut function, id), (types, mut constants, dynamic_constants)| {
            hercules_opt::ccp::ccp(
                &mut function,
                &types,
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
    let (_def_uses, reverse_postorders, typing, _subgraphs, doms, _postdoms, fork_join_maps) =
        hercules_ir::verify::verify(&mut module)
            .expect("PANIC: Failed to verify Hercules IR module.");

    println!(
        "{:?}",
        hercules_ir::schedule::default_plan(
            &module.functions[0],
            &reverse_postorders[0],
            &fork_join_maps[0]
        )
    );

    if args.output.is_empty() {
        let mut tmp_path = temp_dir();
        let mut rng = rand::thread_rng();
        let num: u64 = rng.gen();
        tmp_path.push(format!("hercules_dot_{}.dot", num));
        let mut file = File::create(tmp_path.clone()).expect("PANIC: Unable to open output file.");
        let mut contents = String::new();
        write_dot(
            &module,
            &reverse_postorders,
            &typing,
            &doms,
            &fork_join_maps,
            &mut contents,
        )
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
        write_dot(
            &module,
            &reverse_postorders,
            &typing,
            &doms,
            &fork_join_maps,
            &mut contents,
        )
        .expect("PANIC: Unable to generate output file contents.");
        file.write_all(contents.as_bytes())
            .expect("PANIC: Unable to write output file contents.");
    }
}

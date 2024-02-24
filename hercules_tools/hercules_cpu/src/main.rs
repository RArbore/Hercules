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
    let (_def_uses, _reverse_postorders, _typing, _subgraphs, _doms, _postdoms, _fork_join_maps) =
        hercules_ir::verify::verify(&mut module)
            .expect("PANIC: Failed to verify Hercules IR module.");

    let mut pm = hercules_opt::pass::PassManager::new(module);
    pm.add_pass(hercules_opt::pass::Pass::CCP);
    pm.add_pass(hercules_opt::pass::Pass::DCE);
    pm.add_pass(hercules_opt::pass::Pass::GVN);
    pm.add_pass(hercules_opt::pass::Pass::DCE);
    let mut module = pm.run_passes();

    let (def_uses, reverse_postorders, typing, subgraphs, doms, _postdoms, fork_join_maps) =
        hercules_ir::verify::verify(&mut module)
            .expect("PANIC: Failed to verify Hercules IR module.");
    let antideps: Vec<_> = module
        .functions
        .iter()
        .enumerate()
        .map(|(idx, function)| {
            hercules_cg::antideps::array_antideps(
                function,
                &def_uses[idx],
                &module.types,
                &typing[idx],
            )
        })
        .collect();

    let bbs: Vec<_> = module
        .functions
        .iter()
        .enumerate()
        .map(|(idx, function)| {
            hercules_cg::gcm::gcm(
                function,
                &def_uses[idx],
                &reverse_postorders[idx],
                &subgraphs[idx],
                &doms[idx],
                &fork_join_maps[idx],
                &antideps[idx],
            )
        })
        .collect();

    let fork_join_nests: Vec<_> = module
        .functions
        .iter()
        .enumerate()
        .map(|(idx, function)| {
            hercules_cg::gcm::compute_fork_join_nesting(function, &doms[idx], &fork_join_maps[idx])
        })
        .collect();

    let mut file = File::create("test.ll").unwrap();
    let mut contents = String::new();
    hercules_cg::cpu_beta::cpu_beta_codegen(
        &module,
        &typing,
        &reverse_postorders,
        &def_uses,
        &bbs,
        &antideps,
        &fork_join_maps,
        &fork_join_nests,
        &mut contents,
    )
    .unwrap();
    file.write_all(contents.as_bytes()).unwrap();
}

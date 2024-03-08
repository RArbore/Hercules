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
    let module =
        hercules_ir::parse::parse(&contents).expect("PANIC: Failed to parse Hercules IR file.");

    let mut pm = hercules_opt::pass::PassManager::new(module);
    pm.add_pass(hercules_opt::pass::Pass::Verify);
    pm.add_pass(hercules_opt::pass::Pass::CCP);
    pm.add_pass(hercules_opt::pass::Pass::DCE);
    pm.add_pass(hercules_opt::pass::Pass::GVN);
    pm.add_pass(hercules_opt::pass::Pass::DCE);
    pm.add_pass(hercules_opt::pass::Pass::Forkify);
    pm.add_pass(hercules_opt::pass::Pass::DCE);
    pm.add_pass(hercules_opt::pass::Pass::Predication);
    pm.add_pass(hercules_opt::pass::Pass::DCE);

    pm.run_passes();
    pm.make_typing();
    pm.make_reverse_postorders();
    pm.make_def_uses();
    pm.make_bbs();
    pm.make_antideps();
    pm.make_fork_join_maps();
    pm.make_fork_join_nests();

    let typing = pm.typing.as_ref().unwrap().clone();
    let reverse_postorders = pm.reverse_postorders.as_ref().unwrap().clone();
    let def_uses = pm.def_uses.as_ref().unwrap().clone();
    let bbs = pm.bbs.as_ref().unwrap().clone();
    let antideps = pm.antideps.as_ref().unwrap().clone();
    let fork_join_maps = pm.fork_join_maps.as_ref().unwrap().clone();
    let fork_join_nests = pm.fork_join_nests.as_ref().unwrap().clone();

    let module = pm.get_module();

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

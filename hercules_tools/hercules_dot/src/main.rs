extern crate clap;
extern crate rand;

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
        eprintln!("WARNING: Running hercules_dot on a file without a .hir extension - interpreting as a textual Hercules IR file.");
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

    if args.output.is_empty() {
        pm.add_pass(hercules_opt::pass::Pass::Xdot(true));
        pm.run_passes();
    } else {
        let mut file = File::create(args.output).expect("PANIC: Unable to open output file.");
        let mut contents = String::new();

        pm.make_reverse_postorders();
        pm.make_doms();
        pm.make_fork_join_maps();
        pm.make_plans();

        let reverse_postorders = pm.reverse_postorders.as_ref().unwrap().clone();
        let doms = pm.doms.as_ref().unwrap().clone();
        let fork_join_maps = pm.fork_join_maps.as_ref().unwrap().clone();
        let plans = pm.plans.as_ref().unwrap().clone();

        let module = pm.run_passes();

        hercules_ir::dot::write_dot(
            &module,
            &reverse_postorders,
            Some(&doms),
            Some(&fork_join_maps),
            Some(&plans),
            &mut contents,
        )
        .expect("PANIC: Unable to generate output file contents.");
        file.write_all(contents.as_bytes())
            .expect("PANIC: Unable to write output file contents.");
    }
}

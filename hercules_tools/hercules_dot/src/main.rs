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
    let mut module = pm.run_passes();

    let (def_uses, reverse_postorders, typing, subgraphs, doms, _postdoms, fork_join_maps) =
        hercules_ir::verify::verify(&mut module)
            .expect("PANIC: Failed to verify Hercules IR module.");
    let plans: Vec<_> = module
        .functions
        .iter()
        .enumerate()
        .map(|(idx, function)| {
            hercules_ir::schedule::default_plan(
                function,
                &reverse_postorders[idx],
                &fork_join_maps[idx],
                &hercules_cg::gcm::gcm(
                    function,
                    &def_uses[idx],
                    &reverse_postorders[idx],
                    &subgraphs[idx],
                    &doms[idx],
                    &fork_join_maps[idx],
                    &hercules_cg::antideps::array_antideps(
                        function,
                        &def_uses[idx],
                        &module.types,
                        &typing[idx],
                    ),
                ),
            )
        })
        .collect();

    if args.output.is_empty() {
        hercules_ir::dot::xdot_module(
            &module,
            &reverse_postorders,
            Some(&doms),
            Some(&fork_join_maps),
            Some(&plans),
        );
    } else {
        let mut file = File::create(args.output).expect("PANIC: Unable to open output file.");
        let mut contents = String::new();
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

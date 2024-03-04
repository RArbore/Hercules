extern crate clap;

use clap::Parser;

mod cleanup;
mod codegen;
mod env;
mod semant;
mod ssa;
mod types;

use codegen::*;
use cleanup::clean_ir;

extern crate hercules_ir;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    src_file : String,
}

fn main() {
    let args = Cli::parse();
    let prg = semant::parse_and_analyze(args.src_file);
    match prg {
        Ok(prg) => {
            let module = codegen_program(prg);

            let mut pm = hercules_opt::pass::PassManager::new(module);
            pm.add_pass(hercules_opt::pass::Pass::Verify);
            pm.add_pass(hercules_opt::pass::Pass::Xdot);
            let mut module = pm.run_passes();

            clean_ir(&mut module);

            let mut pm = hercules_opt::pass::PassManager::new(module);
            pm.add_pass(hercules_opt::pass::Pass::Verify);
            pm.add_pass(hercules_opt::pass::Pass::CCP);
            pm.add_pass(hercules_opt::pass::Pass::DCE);
            pm.add_pass(hercules_opt::pass::Pass::GVN);
            pm.add_pass(hercules_opt::pass::Pass::DCE);
            pm.add_pass(hercules_opt::pass::Pass::Xdot);
            pm.add_pass(hercules_opt::pass::Pass::Forkify);
            pm.add_pass(hercules_opt::pass::Pass::DCE);
            pm.add_pass(hercules_opt::pass::Pass::Xdot);
            let _ = pm.run_passes();
        },
        Err(errs) => {
            for err in errs{
                eprintln!("{}", err);
            }
        },
    }
}

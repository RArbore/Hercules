extern crate clap;

use clap::Parser;

mod codegen;
mod env;
mod ssa;
mod types;
mod semant;

use codegen::*;

extern crate hercules_ir;
use hercules_opt::pass;

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
            let mut module = codegen_program(prg);
            println!("{}", module);

            let mut pm = hercules_opt::pass::PassManager::new(module);
            pm.add_pass(hercules_opt::pass::Pass::Verify);
            pm.add_pass(hercules_opt::pass::Pass::Xdot);
            pm.add_pass(hercules_opt::pass::Pass::CCP);
            pm.add_pass(hercules_opt::pass::Pass::Xdot);
            pm.add_pass(hercules_opt::pass::Pass::DCE);
            pm.add_pass(hercules_opt::pass::Pass::Xdot);
            pm.add_pass(hercules_opt::pass::Pass::GVN);
            pm.add_pass(hercules_opt::pass::Pass::Xdot);
            pm.add_pass(hercules_opt::pass::Pass::DCE);
            pm.add_pass(hercules_opt::pass::Pass::Xdot);
            pm.add_pass(hercules_opt::pass::Pass::Forkify);
            pm.add_pass(hercules_opt::pass::Pass::Xdot);
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

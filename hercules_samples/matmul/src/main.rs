extern crate clap;

use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

use clap::Parser;

fn main() {
    hercules_rt::exec::load_binary(Path::new("test.o"));
}

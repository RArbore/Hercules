fn main() {
    println!("cargo:rerun-if-changed=src/load.c");
    cc::Build::new().file("src/load.c").compile("load");
}

use std::path::Path;

extern "C" {
    pub fn load_elf();
}

pub fn load_binary(path: &Path) {
    println!("load_binary: {:?}", path);
    unsafe { load_elf() };
}

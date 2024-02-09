use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

use crate::*;

#[derive(Debug)]
pub struct Module {
    elf: Elf,
}

impl Module {
    pub fn get_function_ptr(&self, name: &str) -> *mut u8 {
        unsafe {
            self.elf.program_section.offset(
                self.elf.function_pointers[self
                    .elf
                    .function_names
                    .iter()
                    .position(|s| s == name)
                    .unwrap()],
            )
        }
    }
}

pub fn load_binary(path: &Path) -> Module {
    let mut f = File::open(path).unwrap();
    let mut buffer = vec![];
    f.read_to_end(&mut buffer).unwrap();
    let elf = unsafe { parse_elf(buffer.as_slice()) };
    Module { elf }
}

#[macro_export]
macro_rules! lookup_function {
    ($module:expr, $function:expr, $($param_ty:ty),*, => $ret_ty:ty) => {
        {
            let fn_ptr: fn($($param_ty),*) -> $ret_ty = unsafe { std::mem::transmute($module.get_function_ptr($function)) };
            fn_ptr
        }
    };
}

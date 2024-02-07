use std::ffi::CString;
use std::os::raw::*;
use std::path::Path;

extern "C" {
    pub fn load_elf(path: *const c_char) -> *const c_void;
}

pub fn load_binary(path: &Path) {
    let path_cstring = CString::new(path.as_os_str().as_encoded_bytes()).unwrap();
    let matmul = unsafe { load_elf(path_cstring.as_ptr()) };
    let matmul: fn(
        *const c_float,
        *const c_float,
        *mut c_float,
        c_ulong,
        c_ulong,
        c_ulong,
    ) -> *mut c_float = unsafe { std::mem::transmute(matmul) };
    let a = [[1.0f32, 2.0f32], [3.0f32, 4.0f32]];
    let b = [[5.0f32, 6.0f32], [7.0f32, 8.0f32]];
    let mut c = [[0.0f32, 0.0f32], [0.0f32, 0.0f32]];
    unsafe {
        matmul(
            std::mem::transmute(a.as_ptr()),
            std::mem::transmute(b.as_ptr()),
            std::mem::transmute(c.as_mut_ptr()),
            2,
            2,
            2,
        )
    };
    println!("{} {}\n{} {}\n", c[0][0], c[0][1], c[1][0], c[1][1]);
}

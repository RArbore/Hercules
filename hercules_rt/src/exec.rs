use std::ffi::CString;
use std::os::raw::c_char;
use std::path::Path;

extern "C" {
    pub fn load_elf(path: *const c_char);
}

pub fn load_binary(path: &Path) {
    let path_cstring = CString::new(path.as_os_str().as_encoded_bytes()).unwrap();
    unsafe { load_elf(path_cstring.as_ptr()) };
}

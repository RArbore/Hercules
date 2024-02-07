extern crate libc;

use std::ffi::CStr;
use std::fs::File;
use std::io::prelude::*;
use std::mem::size_of;
use std::path::Path;
use std::ptr::copy_nonoverlapping;
use std::ptr::null_mut;
use std::ptr::read_unaligned;

use self::libc::*;

#[derive(Debug)]
struct Elf {
    function_names: Vec<String>,
    function_pointers: Vec<isize>,
    text_section: *mut u8,
    text_size: usize,
}

impl Drop for Elf {
    fn drop(&mut self) {
        unsafe { munmap(self.text_section as *mut _, self.text_size) };
    }
}

pub fn load_binary(path: &Path) {
    let mut f = File::open(path).unwrap();
    let mut buffer = vec![];
    f.read_to_end(&mut buffer).unwrap();
    let elf = unsafe { parse_elf(buffer.as_slice()) };
    println!("{:?}", elf);
    let matmul: fn(
        *const c_float,
        *const c_float,
        *mut c_float,
        c_ulong,
        c_ulong,
        c_ulong,
    ) -> *mut c_float =
        unsafe { std::mem::transmute(elf.text_section.offset(elf.function_pointers[0])) };
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

fn page_align(n: usize) -> usize {
    (n + (4096 - 1)) & !(4096 - 1)
}

unsafe fn parse_elf(elf: &[u8]) -> Elf {
    let header: Elf64_Ehdr = read_unaligned(elf.as_ptr() as *const _);
    assert!(header.e_shentsize as usize == size_of::<Elf64_Shdr>());
    let section_header_table: Box<[_]> = (0..header.e_shnum)
        .map(|idx| {
            read_unaligned(
                (elf.as_ptr().offset(header.e_shoff as isize) as *const Elf64_Shdr)
                    .offset(idx as isize),
            )
        })
        .collect();

    let mut symtab_ndx = -1;
    let mut strtab_ndx = -1;
    let mut text_ndx = -1;
    let shstrtab = &elf[section_header_table[header.e_shstrndx as usize].sh_offset as usize..];
    for i in 0..header.e_shnum as usize {
        let section_name = &shstrtab[section_header_table[i].sh_name as usize..];
        if section_name.starts_with(b".symtab") {
            symtab_ndx = i as i32;
        } else if section_name.starts_with(b".strtab") {
            strtab_ndx = i as i32;
        } else if section_name.starts_with(b".text") {
            text_ndx = i as i32;
        }
    }
    assert!(symtab_ndx != -1);
    assert!(strtab_ndx != -1);
    assert!(text_ndx != -1);

    let symtab_hdr = section_header_table[symtab_ndx as usize];
    let strtab_hdr = section_header_table[strtab_ndx as usize];
    let text_hdr = section_header_table[text_ndx as usize];
    let strtab = &elf[strtab_hdr.sh_offset as usize..];

    assert!(symtab_hdr.sh_entsize as usize == size_of::<Elf64_Sym>());
    let num_symbols = symtab_hdr.sh_size as usize / size_of::<Elf64_Sym>();
    let symbol_table: Box<[_]> = (0..num_symbols)
        .map(|idx| {
            read_unaligned(
                (elf.as_ptr().offset(symtab_hdr.sh_offset as isize) as *const Elf64_Sym)
                    .offset(idx as isize),
            )
        })
        .collect();

    let text_size = page_align(text_hdr.sh_size as usize);
    let text_base = mmap(
        null_mut(),
        text_size,
        PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANONYMOUS,
        -1,
        0,
    ) as *mut u8;
    copy_nonoverlapping(
        elf.as_ptr().offset(text_hdr.sh_offset as isize),
        text_base,
        text_hdr.sh_size as usize,
    );
    mprotect(
        text_base as *mut c_void,
        page_align(text_hdr.sh_size as usize),
        PROT_READ | PROT_EXEC,
    );

    let mut elf = Elf {
        function_names: vec![],
        function_pointers: vec![],
        text_section: text_base,
        text_size,
    };
    for i in 0..num_symbols {
        if symbol_table[i].st_info & 0xF == 2 {
            let function_name_base = &strtab[symbol_table[i].st_name as usize..];
            let function_name = CStr::from_ptr(function_name_base.as_ptr() as *const _)
                .to_str()
                .unwrap()
                .to_owned();
            elf.function_names.push(function_name);
            elf.function_pointers
                .push(symbol_table[i].st_value as isize);
        }
    }

    elf
}

extern crate libc;

use std::ffi::CStr;
use std::mem::size_of;
use std::ptr::copy_nonoverlapping;
use std::ptr::null_mut;
use std::ptr::read_unaligned;

use self::libc::*;

/*
 * The libc crate doesn't have everything from elf.h, so these things need to be
 * manually defined.
 */

#[repr(C)]
#[derive(Debug)]
struct Elf64_Rela {
    r_offset: Elf64_Addr,
    r_info: Elf64_Xword,
    r_addend: Elf64_Sxword,
}

const R_X86_64_PC32: u64 = 2;
const R_X86_64_PLT32: u64 = 4;
const STT_FUNC: u8 = 2;

/*
 * Holds a mmaped copy of .text + .bss for direct execution, plus metadata for
 * each function. The .bss section holds a table storing addresses to internal
 * runtime functions, since this is literally easier than patching the object
 * code to directly jump to those runtime functions.
 */
#[derive(Debug)]
pub(crate) struct Elf {
    pub(crate) function_names: Vec<String>,
    pub(crate) function_pointers: Vec<isize>,
    pub(crate) program_section: *mut u8,
    pub(crate) program_size: usize,
}

impl Drop for Elf {
    fn drop(&mut self) {
        unsafe { munmap(self.program_section as *mut _, self.program_size) };
    }
}

/*
 * Function for parsing our internal memory representation of an ELF file from
 * the raw bytes of an ELF file. This includes creating a executable section of
 * code, and relocating function calls and global variables. This whole thing is
 * very unsafe, and is predicated on the elf parameter referencing properly
 * formatted bytes.
 */
pub(crate) unsafe fn parse_elf(elf: &[u8]) -> Elf {
    fn page_align(n: usize) -> usize {
        (n + (4096 - 1)) & !(4096 - 1)
    }

    // read_unaligned corresponds to memcpys in C - we need to memcpy structs
    // out of the file's bytes, since they may be stored without proper
    // alignment.
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

    // Look for the .symtab, .strtab, .text, .bss, and .rela.text sections. Only
    // the .rela.text section is not necessary.
    let mut symtab_ndx = -1;
    let mut strtab_ndx = -1;
    let mut text_ndx = -1;
    let mut bss_ndx = -1;
    let mut rela_text_ndx = -1;
    let shstrtab = &elf[section_header_table[header.e_shstrndx as usize].sh_offset as usize..];
    for i in 0..header.e_shnum as usize {
        let section_name = &shstrtab[section_header_table[i].sh_name as usize..];
        if section_name.starts_with(b".symtab") {
            symtab_ndx = i as i32;
        } else if section_name.starts_with(b".strtab") {
            strtab_ndx = i as i32;
        } else if section_name.starts_with(b".text") {
            text_ndx = i as i32;
        } else if section_name.starts_with(b".bss") {
            bss_ndx = i as i32;
        } else if section_name.starts_with(b".rela.text") {
            rela_text_ndx = i as i32;
        }
    }
    assert!(symtab_ndx != -1);
    assert!(strtab_ndx != -1);
    assert!(text_ndx != -1);
    assert!(bss_ndx != -1);

    // Get the headers for the required sections.
    let symtab_hdr = section_header_table[symtab_ndx as usize];
    let strtab_hdr = section_header_table[strtab_ndx as usize];
    let text_hdr = section_header_table[text_ndx as usize];
    let bss_hdr = section_header_table[bss_ndx as usize];

    // Collect the symbols in the symbol table.
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

    // The mmaped region includes both the .text and .bss sections.
    let program_size = page_align(text_hdr.sh_size as usize) + page_align(bss_hdr.sh_size as usize);
    let program_base = mmap(
        null_mut(),
        program_size,
        PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANONYMOUS,
        -1,
        0,
    ) as *mut u8;
    let text_base = program_base;
    let bss_base = text_base.offset(page_align(text_hdr.sh_size as usize) as isize);

    // Copy the object code into the mmaped region.
    copy_nonoverlapping(
        elf.as_ptr().offset(text_hdr.sh_offset as isize),
        text_base,
        text_hdr.sh_size as usize,
    );

    // If there are relocations, we process them here.
    if rela_text_ndx != -1 {
        let rela_text_hdr = section_header_table[rela_text_ndx as usize];
        let num_relocations = rela_text_hdr.sh_size / rela_text_hdr.sh_entsize;

        // We only iterate the relocations in order, so no need to collect.
        let relocations = (0..num_relocations).map(|idx| {
            read_unaligned(
                (elf.as_ptr().offset(rela_text_hdr.sh_offset as isize) as *const Elf64_Rela)
                    .offset(idx as isize),
            )
        });
        for relocation in relocations {
            let symbol_idx = relocation.r_info >> 32;
            let ty = relocation.r_info & 0xFFFFFFFF;
            let patch_offset = text_base.offset(relocation.r_offset as isize);

            // We support PLT32 relocations only in the .text section, and PC32
            // relocations only in the .bss section.
            match ty {
                R_X86_64_PLT32 => {
                    let symbol_address =
                        text_base.offset(symbol_table[symbol_idx as usize].st_value as isize);
                    let patch = symbol_address
                        .offset(relocation.r_addend as isize)
                        .offset_from(patch_offset);
                    (patch_offset as *mut u32).write_unaligned(patch as u32);
                }
                R_X86_64_PC32 => {
                    let symbol_address =
                        bss_base.offset(symbol_table[symbol_idx as usize].st_value as isize);
                    let patch = symbol_address
                        .offset(relocation.r_addend as isize)
                        .offset_from(patch_offset);
                    (patch_offset as *mut u32).write_unaligned(patch as u32);
                }
                _ => panic!("ERROR: Unrecognized relocation type: {}.", ty),
            }
        }
    }

    // Make the .text section readable and executable. The .bss section should
    // still be readable and writable.
    mprotect(
        text_base as *mut c_void,
        page_align(text_hdr.sh_size as usize),
        PROT_READ | PROT_EXEC,
    );

    // Construct the final in-memory ELF representation. Look up the names of
    // function symbols in the string table.
    let strtab = &elf[strtab_hdr.sh_offset as usize..];
    let mut elf = Elf {
        function_names: vec![],
        function_pointers: vec![],
        program_section: program_base,
        program_size,
    };
    for i in 0..num_symbols {
        if symbol_table[i].st_info & 0xF == STT_FUNC {
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

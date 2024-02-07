#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <elf.h>

static uint64_t page_size = 4096;

static uint64_t page_align(uint64_t n) {
    return (n + (page_size - 1)) & ~(page_size - 1);
}

/*
 * This function is used for loading ELF object files. The goal is to take the
 * machine code for a Hercules function and invoke it in the runtime. This
 * function returns a structure containing heap allocated data, which needs to
 * be deallocated using free_elf.
 */
void load_elf(const char *path) {
    printf("load_elf: %s\n", path);

    int fd = open(path, O_RDONLY);
    if (fd == -1) {
	fprintf(stderr, "ERROR: Couldn't open file at %s.\n", path);
	exit(1);
    }

    struct stat sb;
    if (fstat(fd, &sb) == -1) {
        fprintf(stderr, "ERROR: Couldn't fstat the file at %s.\n", path);
        exit(1);
    }

    char *base = mmap(NULL, sb.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    if (base == MAP_FAILED) {
        fprintf(stderr, "ERROR: Couldn't map the file at %s.\n", path);
        exit(1);
    }

    Elf64_Ehdr header;
    memcpy(&header, base, sizeof(header));

    assert(header.e_shentsize == sizeof(Elf64_Shdr));
    Elf64_Shdr section_header_table[header.e_shnum];
    memcpy(section_header_table, base + header.e_shoff, header.e_shnum * sizeof(Elf64_Shdr));

    char *shstrtab = base + section_header_table[header.e_shstrndx].sh_offset;
    int symtab_ndx = -1;
    int strtab_ndx = -1;
    int text_ndx = -1;
    for (int i = 0; i < header.e_shnum; ++i) {
	char *section_name = shstrtab + section_header_table[i].sh_name;
	if (!strcmp(section_name, ".symtab")) {
	    symtab_ndx = i;
	} else if (!strcmp(section_name, ".strtab")) {
	    strtab_ndx = i;
	} else if (!strcmp(section_name, ".text")) {
	    text_ndx = i;
	}
    }
    if (symtab_ndx == -1) {
	fprintf(stderr, "ERROR: Couldn't find .symtab section in file at %s.\n", path);
	exit(1);
    }
    if (strtab_ndx == -1) {
	fprintf(stderr, "ERROR: Couldn't find .strtab section in file at %s.\n", path);
	exit(1);
    }
    if (text_ndx == -1) {
	fprintf(stderr, "ERROR: Couldn't find .text section in file at %s.\n", path);
	exit(1);
    }
    Elf64_Shdr symtab_hdr = section_header_table[symtab_ndx];
    Elf64_Shdr strtab_hdr = section_header_table[strtab_ndx];
    Elf64_Shdr text_hdr = section_header_table[text_ndx];
    char *strtab = base + strtab_hdr.sh_offset;

    assert(symtab_hdr.sh_entsize == sizeof(Elf64_Sym));
    int num_symbols = symtab_hdr.sh_size / sizeof(Elf64_Sym);
    Elf64_Sym symbol_table[num_symbols];
    memcpy(symbol_table, base + symtab_hdr.sh_offset, num_symbols * sizeof(Elf64_Sym));

    char *text_base = mmap(NULL, page_align(text_hdr.sh_size), PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (text_base == MAP_FAILED) {
	fprintf(stderr, "ERROR: Couldn't allocate memory for .text section of file at %s.\n", path);
    }

    memcpy(text_base, base + text_hdr.sh_offset, text_hdr.sh_size);

    if (mprotect(text_base, page_align(text_hdr.sh_size), PROT_READ | PROT_EXEC) == -1) {
	fprintf(stderr, "ERROR: Couldn't protect memory for .text section of file at %s.\n", path);
        exit(1);
    }

    

    if (munmap(base, sb.st_size) == -1) {
	fprintf(stderr, "ERROR: Couldn't unmap the file at %s.\n", path);
	exit(1);
    }

    close(fd);
}

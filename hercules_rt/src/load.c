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
        fprintf(stderr, "ERROR, Couldn't map the file at %s.\n", path);
        exit(1);
    }

    Elf64_Ehdr header;
    memcpy(&header, base, sizeof(header));

    assert(header.e_shentsize == sizeof(Elf64_Shdr));
    Elf64_Shdr section_header_table[header.e_shnum];
    memcpy(section_header_table, base + header.e_shoff, header.e_shnum * sizeof(Elf64_Shdr));

    char *shstrtab = base + section_header_table[header.e_shstrndx].sh_offset;
    for (int i = 0; i < header.e_shnum; ++i) {
	printf("%s\n", shstrtab + section_header_table[i].sh_name);
    }

    if (munmap(base, sb.st_size) == -1) {
	fprintf(stderr, "ERROR: Couldn't unmap the file at %s.\n", path);
	exit(1);
    }

    close(fd);
}

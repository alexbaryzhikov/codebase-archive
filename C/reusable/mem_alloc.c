#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
#include <fcntl.h>

#define MEM_LEN 10

int main(int argc, char** argv) {
    char *pt;
    int  fd;

    /* method 1 */
    // pt = mmap(NULL, MEM_LEN, PROT_READ|PROT_WRITE, MAP_ANON|MAP_PRIVATE, -1, 0);

    /* method 2 */
    fd = open("/dev/zero", O_RDWR);
    pt = mmap(NULL, MEM_LEN, PROT_READ|PROT_WRITE, MAP_PRIVATE, fd, 0);
    close(fd);

    strcpy(pt, "hello!");
    printf("%s\n", pt);
    munmap(pt, MEM_LEN);
}


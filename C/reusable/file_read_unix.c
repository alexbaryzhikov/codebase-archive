/* Open file and read its contents into memory. */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>     /* read, write, close */
#include <fcntl.h>      /* open, creat */
#include <sys/stat.h>   /* fstat */

/* Print and error message and die. */
void error(char *fmt, ...) {
    va_list args;

    va_start(args, fmt);
    fprintf(stderr, "\x1b[91m" "error: " "\x1b[0m");
    vfprintf(stderr, fmt, args);
    va_end(args);
    exit(EXIT_FAILURE);
}

int main(int argc, char *argv[]) {
    char *fname, *buf;
    int fd;
    struct stat st;
    size_t fsize, rdlen;

    if (argc < 2)
        error("expected file name");

    fname = argv[1];

    /* Open file */
    if ((fd = open(fname, O_RDONLY)) == -1)
        error("failed opening '%s'", fname);

    /* Get file size */
    if (fstat(fd, &st) == -1)
        error("failed reading size of '%s'", fname);
    fsize = st.st_size;

    /* Read contents into buffer */
    buf = malloc(fsize);
    if ((rdlen = read(fd, buf, fsize)) < fsize)
        error("failed reading '%s'", fname);

    close(fd);
  
    /* code */
    
    free(buf);
}

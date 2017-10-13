/* Open file and read its contents into memory. */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

/* Print and error message and die. */
void error(char *fmt, ...) {
    va_list args;

    va_start(args, fmt);
    fprintf(stderr, "\x1b[91m" "error: " "\x1b[0m");
    vfprintf(stderr, fmt, args);
    va_end(args);
    exit(EXIT_FAILURE);
}

int main(int argc, char **argv) {
    char *fname, *buf;
    FILE* fp;
    size_t fsize;

    if (argc < 2)
        error("no file name is given.");

    fname = argv[1];

    /* Open file */
    if ((fp = fopen(fname, "r")) == NULL)
        error("can't open '%s'.\n", fname);

    /* Get file size */
    fseek(fp, 0L, SEEK_END);
    if ((fsize = ftell(fp)) == -1)
        error("failed to seek EOF of '%s'.\n", fname);

    /* Read contents into buffer */
    buf = malloc(fsize);
    fseek(fp, 0L, SEEK_SET);
    if ((fread(buf, 1, fsize, fp)) < fsize)
        error("failed to read '%s'", fname);
    
    fclose(fp);

    /* code */
    
    free(buf);
}

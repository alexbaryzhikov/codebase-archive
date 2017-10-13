#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

/* Number of permutations. */
size_t nPk(size_t n, size_t r) {
    size_t i, res;

    res = 1;
    for (i = 0; i < r; i++)
        res *= n - i;
    return res;
}

int main(int argc, const char *argv[]) {
    printf("%lu\n", nPk(atoi(argv[1]), atoi(argv[2])));
}

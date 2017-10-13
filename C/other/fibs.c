#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

/* Fibonacci -- recursive version               */
uint32_t rfib(uint32_t n) {
    if (n > 1)
        return rfib(n-1) + rfib(n-2);
    else
        return n;
}

/* Fibonacci -- iterative version               */
uint32_t ifib(uint32_t n) {
    if (n < 2)
        return n;
    else {
        uint32_t val0, val1 = 0, val2 = 1, i;
        for (i = 1; i < n; i++) {
            val0 = val2;
            val2 = val2 + val1;
            val1 = val0;
        }
        return val2;
    }
}

void main(int argc, char** argv) {
    if (argc > 1)
        printf("%d\n", ifib(atoi(argv[1])));
    else
        puts("Usage: ./fibs N");
}

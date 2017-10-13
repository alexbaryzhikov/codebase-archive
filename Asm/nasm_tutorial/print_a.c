#include <stdio.h>
#include <stdint.h>

#define n 0xF00D

int main(int argc, char **argv) {
    int16_t a = n;
    printf("%d\n", a);
    return 0;
}
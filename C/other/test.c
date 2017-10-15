#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

int main(int argc, const char *argv[]) {
    {
        int i = 99;
        printf("inside block - %d\n", i);
    }
    printf("outside block - %d\n", i);
}

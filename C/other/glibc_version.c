#include <stdio.h>
#include <gnu/libc-version.h>

int main(void) {
    printf("%s\n", gnu_get_libc_version());
}

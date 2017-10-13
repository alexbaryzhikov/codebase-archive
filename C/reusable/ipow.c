/* Fast integer exponentiation */

#include <stdio.h>

typedef unsigned int u_int;

u_int ipow(u_int base, u_int exp) {
    u_int result = 1;

    for (; exp; exp >>= 1, base *= base)
        if (exp & 1)
            result *= base;
    return result;
}

int main() {
    u_int b = 2, e = 10;

    printf("%u\n", ipow(b, e));
}

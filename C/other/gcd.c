#include <stdio.h>
#include <stdlib.h>

int gcd(int a, int b) {
    int x = a;
    int y = b;
    while (x != y) {
        if (x > y) {
            x -= y;
        } else {
            y -= x;
        }
    }
    return x;
}

main(int argc, char **argv) {
    if (argc > 2) printf("%d\n", gcd(atoi(argv[1]), atoi(argv[2])));
}

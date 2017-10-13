#include <stdio.h>
#include <stdlib.h>

int count() {
    static int n = 0;
    return n++;
}

main() {
    for (int i = 0; i < 10; i++) {
        printf("%d\n", count());
    }
}

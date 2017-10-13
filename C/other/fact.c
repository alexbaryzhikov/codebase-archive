#include <stdio.h>

int main() {

    int n = 5;
    int r = 0;
    r = 1;

    while (n > 0) {
        r *= n;
        n--;
    }

    printf("%d\n", r);
}

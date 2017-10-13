#include <stdio.h>

#define swap(x, y)  { register int t = x; x = y; y = t; }

int main() {
    int a = 1, b = 4;
    swap(a, b);
    printf("%d %d\n", a, b);
}

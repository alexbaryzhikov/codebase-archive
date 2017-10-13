#include <stdio.h>

#define forever             for (;;)
#define max(A, B)           ((A) > (B) ? (A) : (B))
#define square(x)           ((x) * (x))
#define dprint(expr)        printf(#expr " = %g\n", expr)
#define paste(front, back)  front ## back
#define swap(type, x, y)    { type tmp = x; x = y; y = tmp; }

int main() {
    printf("%d\n", max(1, 4));
    printf("%d\n", square(1+4));
    dprint(2.0 + 2.0);
    int name1 = 10;
    double k = 3.0, l = 0.4;
    swap(double, k, l);
    printf("%g %g\n", k, l);
    printf("%d\n", paste(name, 1));
    printf("Press ^C to continue...\n");
    forever;
}

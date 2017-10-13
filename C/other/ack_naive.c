#include <stdio.h>
#include <stdlib.h>

int ack(int m, int n) {
    if (!m) return n+1;
    if (!n) return ack(m-1, 1);
    return ack(m-1, ack(m, n-1));
}

main(int argc, char **argv) {
    if (argc > 2) printf("%d\n", ack(atoi(argv[1]), atoi(argv[2])));
}

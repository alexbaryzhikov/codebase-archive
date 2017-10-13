/* Pseudo-random number generator. */

#include <stdio.h>
#include <stdlib.h>

unsigned long next = 1;

/* randint: return pseudo-random integer on 0..a */
unsigned randint(unsigned a) {
    next = next * 1103515245 + 12345;
    return (unsigned)((next / 65536UL) % (a + 1));
}

/* randint: return pseudo-random float on 0..1 */
double randflt() {
    unsigned long tmp;

    next = next * 1103515245 + 12345;
    tmp = (unsigned long)(next / 65536UL) % 4294967296UL;
    return (double)tmp / 4294967295.0;
}

/* randseed: set seed for randint() */
void randseed(unsigned seed) {
    next = seed;
}

int main(int argc, const char *argv[]) {
    if (argc < 3) {
        printf("Usage:\n");
        printf("  rand [SEED] [SAMPLE_SIZE]           floats in range 0..1\n");
        printf("  rand i [SEED] [MAX] [SAMPLE_SIZE]   integers in range 0..MAX\n");
        return 0;
    }

    if (argv[1][0] == 'i') {
        randseed(atoi(argv[2]));
        int i, j = atoi(argv[3]), n = atoi(argv[4]);
        for (i = 0; i < n; i++) {
            printf("%u\n", randint(j));
        }
        return 0;
    }

    randseed(atoi(argv[1]));
    int i, j, n = atoi(argv[2]);
    int hist[25];
    int histn[25];
    long double max = 0;

    /* initialize */
    for (i = 0; i < 25; i++)
        hist[i] = 0;

    /* generate random sample */    
    for (i = 0; i < n; i++) {
        j = randflt() * 25.0;
        hist[j]++;
    }

    /* find max */
    j = 0;
    for (i = 0; i < 25; i++)
        if (hist[i] > j)
            j = hist[i];

    max = j;

    /* normalize values for bar lengths */
    for (i = 0; i < 25; i++)
        histn[i] = (int)(((long double)hist[i] / max) * 50.0);

    /* print out */
    for (i = 0; i < 25; i++) {
        printf("%.2f%7d ", 0.04 * (i + 1.0), hist[i]);
        for (j = 0; j < histn[i]; j++)
            putchar('=');
        putchar('\n');
    }

    return 0;
}

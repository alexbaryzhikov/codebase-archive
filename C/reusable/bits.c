#include <stdio.h>
#include <stdlib.h>


unsigned getbits(unsigned, int, int);
unsigned setbits(unsigned, int, int, unsigned);
unsigned invert(unsigned, int, int);
unsigned rightrot(unsigned, int);
int bitcount(unsigned);

unsigned bintoi(char*);
char* itobin(unsigned);


int main(int argc, char *argv[]) {
    if ((argc < 2) || (argv[1][0] == 'g' && argc < 5) || (argv[1][0] == 's' && argc < 6) ||
        (argv[1][0] == 'i' && argc < 5) || (argv[1][0] == 'r' && argc < 4) ||
        (argv[1][0] == 'b' && argc < 3)) {
        printf("Usage:\n"
            "\tbits arg0\n\tReturns binary representation of a non-negative integer arg0.\n\n"
            "\tbits MODE arg0 arg1...\n"
            "Modes:\n"
            "\tg\tget bits (X P N)    returns N bits of X from position P \n"
            "\ts\tset bits (X P N Y)  returns X with the N bits at P set to N rightmost bits of Y\n"
            "\ti\tinvert (X P N)      returns X with the N bits at P inverted\n"
            "\tr\trotate right (X N)  returns X rotated to the right by N bit positions\n"
            "\tb\tcount bits (X)      returns the number of 1 bits in X.\n");
        return 0;
    }
    
    if (argc == 2) {
        printf("%s\n", itobin(atoi(argv[1])));
        return 0;
    }

    int i, j, sz = sizeof(unsigned) * 8 + 3;

    char* a[argc-2];
    for (i = 0; i < argc-2; i++) {
        a[i] = malloc(sz);           // allocate array for numeric arguments
        for (j = 0; argv[i+2][j] != '\0' && j < (sz); j++) {
            a[i][j] = argv[i+2][j];  // copy arguments to the new array
        }
        a[i][j] = '\0';
    }

    unsigned x, y;
    int p, n;

    if (a[0][0] == '0' && a[0][1] == 'b')
        x = bintoi(a[0]);  // convert from binary representation
    else
        x = atoi(a[0]);    // convert from decimal

    // mode dispatch
    if (argv[1][0] == 'g') {
        p = atoi(a[1]);
        n = atoi(a[2]);
        printf("%s\n", itobin(getbits(x, p, n)));
    } else if (argv[1][0] == 's') {
        p = atoi(a[1]);
        n = atoi(a[2]);
        if (a[3][0] == '0' && a[3][1] == 'b')
            y = bintoi(a[3]);
        else
            y = atoi(a[3]);
        printf("%s\n", itobin(setbits(x, p, n, y)));
    } else if (argv[1][0] == 'i') {
        p = atoi(a[1]);
        n = atoi(a[2]);
        printf("%s\n", itobin(invert(x, p, n)));
    } else if (argv[1][0] == 'r') {
        n = atoi(a[1]);
        printf("%s\n", itobin(rightrot(x, n)));
    } else if (argv[1][0] == 'b') {
        printf("%d\n", bitcount(x));
    } else
        printf("Invalid mode: %c\n", argv[1][0]);
    
    return 0;
}


/* ------------------------------------------------------------------------- */
/* Bitwise methods */


/* Get N bits from position P. */
unsigned getbits(unsigned x, int p, int n) {
    return (x >> (p + 1 - n)) & ~(~0 << n);
}


/* Returns X with the N bits at P set to N rightmost bits of Y.*/
unsigned setbits(unsigned x, int p, int n, unsigned y) {
    x = x & ~(~(~0 << n) << (p + 1 - n));
    y = (y & ~(~0 << n)) << (p+1-n);
    return x & y;
}


/* Returns X with the N bits at P inverted.*/
unsigned invert(unsigned x, int p, int n) {
    return x ^ (~(~0 << n) << (p+1-n));
}


/* Returns X rotated to the right by N bit positions.*/
unsigned rightrot(unsigned x, int n) {
    int i = 0;
    unsigned tmp = x;
    while (tmp){
        i++;
        tmp >>= 1;
    }
    n %= i;
    return (x >> n | x << (i - n)) & ~(~0 << i);
}


/* Returns the number of 1 bits in X. */
int bitcount(unsigned x) {
    int b;
    for (b = 0; x; x &= x - 1)
        b++;
    return b;
}


/* ------------------------------------------------------------------------- */
/* Utilities */


/* Converts string to unsigned integer, assuming binary form */
unsigned bintoi(char s[]) {
    unsigned i = 2, n = 0;
    while (1) {
        if (s[i] == '1')
            n++;
        if (s[++i] == '\0')
            break;
        n <<= 1;
    }
    return n;
}


/* Converts unsigned integer (binary form) to string */
char* itobin(unsigned n) {
    int i = 0, j = 0, sz = sizeof(unsigned) * 8 + 3;
    char* s = malloc(sz);
    char srev[sz-3];
    srev[0] = '0';
    for (i = 0; n && i < sz-3; i++) {
        if (n & 1)
            srev[i] = '1';
        else
            srev[i] = '0';
        n >>= 1;
    }
    s[0] = '0';
    s[1] = 'b';
    s[2] = '0';
    if (i) {
        i--;
        for (j = 0; j <= i; j++) {
            s[j+2] = srev[i-j];
        }
    }
    s[i+3] = '\0';
    return s;
}


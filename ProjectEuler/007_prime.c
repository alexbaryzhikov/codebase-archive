/*
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
*/
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define DICT_SIZE   4096

const size_t start_primes[4] = {2, 3, 5, 7};

/* ------------------------------------------------------------ */
/* Sieve state */
struct Sieve {
    size_t prime;                   // last "base" prime
    size_t prime_sq;                // square of the "base" prime
    size_t sieve[DICT_SIZE][2];     // sieve dict {number : step}
    size_t n;                       // candidate number
    size_t idx;                     // last returned prime index
    struct Sieve *base_primes;      // supply of base primes
};

typedef struct Sieve sieve_t;

/* ------------------------------------------------------------ */
/* Methods declarations */
void    sieve_init(sieve_t *s);
size_t  primes(sieve_t *s);
bool    d_contains(size_t d[][2], size_t n);
size_t  d_getslot(size_t d[][2]);
size_t  d_getidx(size_t d[][2], size_t k);

/* ---------------------------------------------------------- */
/* Main */
int main() {
    sieve_t a;
    sieve_init(&a);
    while (a.idx < 10000)
        primes(&a);
    printf("%10lu)%15lu\n", a.idx, primes(&a));
    return 0;
}

/* ---------------------------------------------------------- */
/* Sieve state initialize */
void sieve_init(sieve_t *s) {
    s->prime = 3;
    s->prime_sq = 9;
    for (int i = 0; i < DICT_SIZE; i++) {
        s->sieve[i][0] = 0;
        s->sieve[i][1] = 0;
    }
    s->n = 9;
    s->idx = 0;
    s->base_primes = NULL;
}

/* ---------------------------------------------------------- */
/* Primes generator */
size_t primes(sieve_t *s) {
    if (s->idx < 4)
        // yield pre-defined primes
        return start_primes[s->idx++];
    else if (s->base_primes == NULL) {
        // create and setup new supply of primes
        s->base_primes = malloc(sizeof(sieve_t));
        sieve_init(s->base_primes);
        primes(s->base_primes); // skip 2, we will work only with odds
        primes(s->base_primes); // skip 3, because s.prime is already set to 3
    }

    while (true) {
        if (!d_contains(s->sieve, s->n)) {
            if (s->n < s->prime_sq) {               // next prime found!
                size_t res = s->n;
                s->n += 2;                          // increment n in advance
                s->idx++;                           // increment prime counter
                return res;                         // return prime
            } else {
                size_t step = s->prime * 2;
                size_t k = d_getslot(s->sieve);
                s->sieve[k][0] = s->prime_sq + step;
                s->sieve[k][1] = step;
                s->prime = primes(s->base_primes);  // pull next base prime
                s->prime_sq = s->prime * s->prime;  // and get its square
            }
        } else {
            size_t k = d_getidx(s->sieve, s->n);
            size_t step = s->sieve[k][1];
            size_t next = s->n + step;              // compute next sieve entry
            while (d_contains(s->sieve, next))
                next += step;                       // ensure it's unique
            s->sieve[k][0] = next;
            s->sieve[k][1] = step;
        }
        s->n += 2;
    }
}

/* ---------------------------------------------------------- */
/* Is key in the dict? */
bool d_contains(size_t d[][2], size_t k) {
    for (int i = 0; i < DICT_SIZE; i++)
        if (k == d[i][0])
            return true;
    return false;
}

/* ---------------------------------------------------------- */
/* Returns index of the first free slot in the dict */
size_t d_getslot(size_t d[][2]) {
    for (int i = 0; i < DICT_SIZE; i++)
        if (!d[i][0])
            return i;
    printf("Dictionary is full.\n");
    exit(1);
}

/* ---------------------------------------------------------- */
/* Returns index of the key */
size_t d_getidx(size_t d[][2], size_t k) {
    for (int i = 0; i < DICT_SIZE; i++)
        if (k == d[i][0])
            return i;
    printf("Key not found.\n");
    exit(1);
}

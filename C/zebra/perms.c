/*
def permutations(iterable, r=None):
    'permutations(range(3), 2) --> (0,1) (0,2) (1,0) (1,2) (2,0) (2,1)'
    pool = tuple(iterable)
    n = len(pool)
    r = n if r is None else r
    indices = list(range(n))
    cycles = list(range(n-r+1, n+1)[::-1])
    yield tuple(pool[i] for i in indices[:r])
    while n:
        for i in reversed(range(r)):
            cycles[i] -= 1
            if cycles[i] == 0:
                indices[i:] = indices[i+1:] + indices[i:i+1]
                cycles[i] = n - i
            else:
                j = cycles[i]
                indices[i], indices[-j] = indices[-j], indices[i]
                yield tuple(pool[i] for i in indices[:r])
                break
        else:
            return
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "defs.h"


/* Return array of r-permutations of list a. */
int **perms(int a[], size_t n, size_t r) {
    r = r ? r : n;
    size_t indices[n];          /* one index per element in the pool */
    size_t cycles[r];           /* one rollover counter per element in the result */
    int    *tuples;             /* array of result tuples */
    int    **res;               /* array of pointers to result tuples */
    size_t res_idx;         
    size_t res_len;
    size_t i, j, k, index;
    bool   yield;

    for (i = 0; i < n; i++)
        indices[i] = i;
    for (i = 0; i < r; i++)
        cycles[i] = n - i;

    /* allocate output array */
    res_len = nPk(n, r);
    tuples = (int *) malloc(res_len * r * sizeof(int));
    res = (int**) malloc(res_len * sizeof(int *));
    for (i = 0; i < res_len; i++)
        res[i] = tuples + (r * i);

    /* initialize first tuple */
    for (i = 0; i < r; i++)
        res[0][i] = a[i];
    
    res_idx = 1;
    yield = true;
    while (n) {
        /* decrement rightmost cycle, moving leftward upon zero rollover */
        for (i = r-1; yield; i--) {
            cycles[i]--;
            if (cycles[i] == 0) {
                yield = i;
                /* rotation: indices[i:] = indices[i+1:] + indices[i:i+1] */
                index = indices[i];
                for (j = i; j < n-1; j++)
                    indices[j] = indices[j+1];
                indices[n-1] = index;
                cycles[i] = n - i;
            } else {
                j = cycles[i];
                index = indices[i];
                indices[i] = indices[n-j];
                indices[n-j] = index;
                for (k = 0; k < r; k++) {
                    /* yield tuple(pool[k] for k in indices[:r]) */
                    index = indices[k];
                    res[res_idx][k] = a[index];
                }
                res_idx++;
                break;
            }
        }
        /* if yield is false, then the cycles have all rolled-over and we're done. */
        if (!yield)
            break;
    }
    return res;
}


/* Deallocate permutations array. */
void perms_free(int **a) {
    free(a[0]);
    free(a);
}


/* Number of permutations. */
size_t nPk(size_t n, size_t r) {
    size_t i, res;

    res = 1;
    for (i = 0; i < r; i++)
        res *= n - i;
    return res;
}



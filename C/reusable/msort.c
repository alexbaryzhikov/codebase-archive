#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define ARRAY_SIZE  100000

#define msort(base, len, size, cmp)  msort_r(base, NULL, 0, len-1, size, (int (*)(void*, void*))cmp)

/*
Sort an array.

    base    pointer to the first element
    tmp     pointer to temporary array
    left    left index
    right   right index
    cmp     compare function
*/
void
msort_r(void* base, void* tmp, size_t left, size_t right, size_t sz, int cmp(void*, void*)) {
    size_t len, mid, i, j, k;
    int allocd;

    if (left >= right)
        return;

    // allocate temporary array
    len = right - left + 1;
    if (!tmp) {
        tmp = malloc(len * sz);
        allocd = 1;
    } else
        allocd = 0;

    // partition
    mid = left + (right - left) / 2;
    msort_r(base, tmp, left, mid, sz, cmp);
    msort_r(base, tmp, mid + 1, right, sz, cmp);

    // merge
    memcpy(tmp + left * sz, base + left * sz, len * sz);

    for (i = left, j = mid + 1, k = left; k <= right; k++)
        // dump the rest of left side
        if (j > right)
            memcpy(base + k * sz, tmp + i++ * sz, sz);
        // dump the rest of right side
        else if (i > mid)
            memcpy(base + k * sz, tmp + j++ * sz, sz);
        // compare
        else if (cmp(tmp + i * sz, tmp + j * sz) <= 0)
            memcpy(base + k * sz, tmp + i++ * sz, sz);
        else
            memcpy(base + k * sz, tmp + j++ * sz, sz);

    // deallocate temporary array
    if (allocd)
        free(tmp);
}

int cmpint(int* a, int* b) {
    if (*a < *b)
        return -1;
    if (*a > *b)
        return 1;
    return 0;
}

int main(int argc, char** argv) {
    int i, j, a[10][ARRAY_SIZE];
    clock_t t1, t2, tsum;

    srand((unsigned)clock());
    for (j = 0; j < 10; j++)
        for (i = 0; i < ARRAY_SIZE; i++)
            a[j][i] = rand();

    for (tsum = 0, j = 0; j < 10; tsum += t2 - t1, j++) {
        t1 = clock();
        msort(a[j], sizeof(a[j]) / sizeof(a[j][0]), sizeof(a[j][0]), cmpint);
        t2 = clock();
    }
    
    printf("time to sort: %ld\n", tsum / 10);

    /* check sorting */
    for (j = 0; j < 10; j++)
        for (i = 1; i < ARRAY_SIZE; i++)
            if (a[j][i] < a[j][i-1])
                fprintf(stderr, "Sorting failed\n");
}

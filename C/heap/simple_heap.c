/*
Integer Heap Queue.

Usage:

heap_clear(heap)            Clear the heap items.
heap_delete(&heap)          Deallocate heap object.
heap_equal(heap1, heap2)    Test heap1 and heap2 for equality.
heap_fromarray(array, n)    New heap from an array.
heap_peek(heap)             Return top item on the heap without popping it.
heap_pop(heap)              Pop the top item from the heap.
heap_print(heap)            Print the heap contents.
heap_push(heap, item)       Push a new item on the heap.
heap_pushpop(heap, item)    Push an item on the heap then pop an item (fast).
heap_replace(heap, item)    Pop the top item, then push a new item; retain heap size.
heap_sort(array, n)         Return sorted array.

Heap_max subclass:

heap_fromarray_max(array, n)
heap_pop_max(heap)
heap_push_max(heap, item)
heap_pushpop_max(heap, item)
heap_replace_max(heap, item)
heap_sort_max(array, n)
*/

#include <assert.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include "simple_heap.h"

/* Some useful macros. */
#define EXTEND(h)      _extend(h, powoftwo_above(SIZE(h)))
#define LEN(h)         h->length
#define NEW(n)         _new(powoftwo_above(n))
#define SETLEN(h, n)   h->length = n
#define SETSIZE(h, n)  h->size = n
#define SIZE(h)        h->size

/* Return the smallest power of 2 greater than or equal to n. */
size_t powoftwo_above(size_t n) {
    size_t x = 1;
    while ((x <<= 1) <= n);
    return x;
}

/* ---------------------------------------------------------------------------- */
/* Heap private methods */

/* Allocate a new heap object of size n. */
heap_t *_new(size_t n) {
    heap_t *heap = malloc(sizeof(heap_t));
    SETSIZE(heap, n);  /* max number of elements */
    SETLEN(heap, 0);   /* number of elements */
    heap->items = malloc(sizeof(int) * n);
    return heap;
}

/* Extend heap items array to size n. */
heap_t *_extend(heap_t *heap, size_t n) {
    size_t sz = SIZE(heap);
    heap->items = realloc(heap->items, sizeof(int) * n);
    /* clear the new chunk of memory */
    if (n > sz)
        memset((char*)(heap->items + sz), 0, sizeof(int) * (n - sz));
    SETSIZE(heap, n);
    return heap;
}

/*
'heap' is a heap at all indices >= startpos, except possibly for pos.
pos is the index of a leaf with a possibly out-of-order value.
Restore the heap invariant.
*/
void _siftdown(heap_t *heap, size_t startpos, size_t pos) {
    int    *h = heap->items;
    int    newitem = h[pos];
    int    parent;
    size_t parentpos;
    
    /* Follow the path to the root, moving parents down until finding a place */
    /* newitem fits. */
    while (pos > startpos) {
        parentpos = (pos - 1) >> 1;
        parent = h[parentpos];
        if (newitem < parent) {
            h[pos] = parent;
            pos = parentpos;
            continue;
        }
        break;
    }
    h[pos] = newitem;
}

/*
The child indices of heap index pos are already heaps, and we want to make
a heap at index pos too. We do this by bubbling the smaller child of
pos up (and so on with that child's children, etc) until hitting a leaf,
then using siftdown to move the oddball originally at index pos into place.
*/
void _siftup(heap_t *heap, size_t pos) {
    int    *h = heap->items;
    int    newitem = h[pos];
    size_t startpos = pos;
    size_t endpos = LEN(heap);
    size_t childpos = 2*pos + 1; /* leftmost child position */

    /* Bubble up the smaller child until hitting a leaf. */
    while (childpos < endpos) {
        /* Set childpos to index of smaller child. */
        size_t rightpos = childpos + 1;
        if (rightpos < endpos && !(h[childpos] < h[rightpos]))
            childpos = rightpos;
        /* Move the smaller child up. */
        h[pos] = h[childpos];
        pos = childpos;
        childpos = 2*pos + 1;
    }
    /* The leaf at pos is empty now. Put newitem there, and bubble it up */
    /* to its final resting place (by sifting its parents down). */
    h[pos] = newitem;
    _siftdown(heap, startpos, pos);
}

/*
Transform array into a heap, in-place, in O(len(x)) time.

Transform bottom-up. The largest index there's any point to looking at
is the largest with a child index in-range, so must have 2*i + 1 < n,
or i < (n-1)/2. If n is even = 2*j, this is (2*j-1)/2 = j-1/2 so
j-1 is the largest, which is n//2 - 1. If n is odd = 2*j+1, this is
(2*j+1-1)/2 = j so j-1 is the largest, and that's again n//2-1.
*/
void _heapify(heap_t *heap) {
    size_t i = (LEN(heap) >> 1);
    while (i > 0)
        _siftup(heap, --i);
}

/* ---------------------------------------------------------------------------- */
/* Heap API */

/* Clear heap items. */
heap_t *heap_clear(heap_t *heap) {
    memset(heap->items, 0, sizeof(int) * SIZE(heap));
    SETLEN(heap, 0);
    return heap;
}

/* Deallocate heap object. */
void heap_delete(heap_t **pheap) {
    if (*pheap) {
        free((*pheap)->items);
        free(*pheap);
        *pheap = NULL;
    }
}

/* Test heap1 and heap2 for equality. */
int heap_equal(const heap_t *heap1, const heap_t *heap2) {
    if (LEN(heap1) != LEN(heap2))
        return 0;
    for (size_t i = 0; i < LEN(heap1); i++)
        if (heap1->items[i] != heap2->items[i])
            return 0;
    return 1;
}

/* New heap from an array. */
heap_t *heap_fromarray(const int *array, size_t len) {
    heap_t *heap = NEW(len);
    if (len)
        memcpy(heap->items, array, sizeof(int) * len);
    SETLEN(heap, len);
    _heapify(heap);
    return heap;
}

/* Return top item on the heap without popping it. */
int heap_peek(const heap_t *heap) {
    return heap->items[0];
}

/* Pop the smallest item off the heap, maintaining the heap invariant. */
int heap_pop(heap_t *heap) {
    int *h = heap->items;
    int lastitem;
    int returnitem;
    size_t pos;

    if (LEN(heap) > 0) {
        pos = LEN(heap) - 1;
        lastitem = h[pos];
        SETLEN(heap, pos);
        if (LEN(heap) > 0) {
            returnitem = h[0];
            h[0] = lastitem;
            _siftup(heap, 0);
            return returnitem;
        }
        return lastitem;
    } else {
        fprintf(stderr, "Can't pop an item: heap is empty.\n");
        exit(EXIT_FAILURE);
    }
}

/* Print heap contents. */
void heap_print(const heap_t *heap) {
    size_t i;
    int *h = heap->items;

    printf("[");
    if (LEN(heap)) {
        for (i = 0; i < LEN(heap)-1; i++)
            printf("%d, ", h[i]);
        printf("%d", h[i]);
    }
    printf("] size = %lu, len = %lu\n", SIZE(heap), LEN(heap));
}

/* Push item onto heap, maintaining the heap invariant. */
void heap_push(heap_t *heap, int item) {
    size_t pos = LEN(heap);
    
    if (LEN(heap) == SIZE(heap))
        EXTEND(heap);
    heap->items[pos] = item;  /* append item to the rightmost leaf */
    SETLEN(heap, pos + 1);
    _siftdown(heap, 0, pos);
}

/* Fast version of a heap_push followed by a heap_pop. */
int heap_pushpop(heap_t *heap, int item) {
    int tmp;
    int *h = heap->items;
    
    if (LEN(heap) && h[0] < item) {
        tmp = h[0];
        h[0] = item;
        item = tmp;
        _siftup(heap, 0);
    }
    return item;
}

/*
Pop and return the current smallest value, and add the new item.

This is more efficient than heap_pop() followed by heap_push(), and can be
more appropriate when using a fixed-size heap. Note that the value
returned may be larger than item! That constrains reasonable uses of
this routine unless written as part of a conditional replacement:

    if (item > heap->items[0]) item = heap_replace(heap, item);
*/
int heap_replace(heap_t *heap, int item) {
    int returnitem;

    if (LEN(heap)) {
        returnitem = heap->items[0];
        heap->items[0] = item;
        _siftup(heap, 0);
        return returnitem;
    } else {
        fprintf(stderr, "Can't replace an item '%d': heap is empty.\n", item);
        exit(EXIT_FAILURE);
    }
}

/* Return sorted array. */
int *heap_sort(const int *array, size_t n) {
    heap_t *heap = heap_fromarray(array, n);
    int *res = malloc(sizeof(int) * n);
    for (size_t i = 0; i < n; i++)
        res[i] = heap_pop(heap);
    heap_delete(&heap);
    return res;
}

/* ---------------------------------------------------------------------------- */
/* Heap_max private methods */

void _siftdown_max(heap_t *heap, size_t startpos, size_t pos) {
    int    *h = heap->items;
    int    newitem = h[pos];
    int    parent;
    size_t parentpos;
    
    while (pos > startpos) {
        parentpos = (pos - 1) >> 1;
        parent = h[parentpos];
        if (newitem > parent) {
            h[pos] = parent;
            pos = parentpos;
            continue;
        }
        break;
    }
    h[pos] = newitem;
}

void _siftup_max(heap_t *heap, size_t pos) {
    int    *h = heap->items;
    int    newitem = h[pos];
    size_t startpos = pos;
    size_t endpos = LEN(heap);
    size_t childpos = 2*pos + 1;

    while (childpos < endpos) {
        size_t rightpos = childpos + 1;
        if (rightpos < endpos && !(h[childpos] > h[rightpos]))
            childpos = rightpos;
        h[pos] = h[childpos];
        pos = childpos;
        childpos = 2*pos + 1;
    }
    h[pos] = newitem;
    _siftdown_max(heap, startpos, pos);
}

void _heapify_max(heap_t *heap) {
    size_t i = (LEN(heap) >> 1);
    while (i > 0)
        _siftup_max(heap, --i);
}

/* ---------------------------------------------------------------------------- */
/* Heap_max API */

heap_t *heap_fromarray_max(const int *array, size_t len) {
    heap_t *heap = NEW(len);
    if (len)
        memcpy(heap->items, array, sizeof(int) * len);
    SETLEN(heap, len);
    _heapify_max(heap);
    return heap;
}

int heap_pop_max(heap_t *heap) {
    int *h = heap->items;
    int lastitem;
    int returnitem;
    size_t pos;

    if (LEN(heap) > 0) {
        pos = LEN(heap) - 1;
        lastitem = h[pos];
        SETLEN(heap, pos);
        if (LEN(heap) > 0) {
            returnitem = h[0];
            h[0] = lastitem;
            _siftup_max(heap, 0);
            return returnitem;
        }
        return lastitem;
    } else {
        fprintf(stderr, "Can't pop an item: heap is empty.\n");
        exit(EXIT_FAILURE);
    }
}

void heap_push_max(heap_t *heap, int item) {
    size_t pos = LEN(heap);
    
    if (LEN(heap) == SIZE(heap))
        EXTEND(heap);
    heap->items[pos] = item;
    SETLEN(heap, pos + 1);
    _siftdown_max(heap, 0, pos);
}

int heap_pushpop_max(heap_t *heap, int item) {
    int tmp;
    int *h = heap->items;
    
    if (LEN(heap) && h[0] > item) {
        tmp = h[0];
        h[0] = item;
        item = tmp;
        _siftup_max(heap, 0);
    }
    return item;
}

int heap_replace_max(heap_t *heap, int item) {
    int returnitem;

    if (LEN(heap)) {
        returnitem = heap->items[0];
        heap->items[0] = item;
        _siftup_max(heap, 0);
        return returnitem;
    } else {
        fprintf(stderr, "Can't replace an item '%d': heap is empty.\n", item);
        exit(EXIT_FAILURE);
    }
}

int *heap_sort_max(const int *array, size_t n) {
    heap_t *heap = heap_fromarray_max(array, n);
    int *res = malloc(sizeof(int) * n);
    for (size_t i = 0; i < n; i++)
        res[i] = heap_pop_max(heap);
    heap_delete(&heap);
    return res;
}

/* ---------------------------------------------------------------------------- */
/* Array methods */

/* Test array1 and array2 for equality. */
int array_equal(const int *array1, size_t len1, const int *array2, size_t len2) {
    if (len1 != len2)
        return 0;
    for (size_t i = 0; i < len1; i++)
        if (array1[i] != array2[i])
            return 0;
    return 1;
}

/*
Read array from a file containing "n1, n2, n3, ..." to *parray.
Return array length.
*/
size_t array_fromfile(const char *filename, int **parray) {
    char *in;
    int *res;
    int fd;
    size_t i, j, len;
    struct stat st;

    /* open file and read its contents */
    fd = open(filename, O_RDONLY);
    if (fd == -1) {
        fprintf(stderr, "Can't read file.\n");
        exit(EXIT_FAILURE);
    }
    fstat(fd, &st);
    in = malloc(st.st_size);
    read(fd, in, st.st_size);
    close(fd);

    /* count number of elements */
    for (i = 0, len = 1; i < st.st_size; i++)
        if (in[i] == ',') len++;  /* ',' is a delimiter */

    /* convert input to heap */
    res = malloc(sizeof(int) * len);
    for (i = 0, j = 0; i < st.st_size; i++) {
        res[j++] = atoi(in + i);
        for (; in[i] != ',' && i < st.st_size; i++);
    }
    *parray = res;
    free(in);
    return len;
}

/* Print array contents. */
void array_print(const int *array, size_t n) {
    size_t i;

    printf("[");
    if (n) {
        for (i = 0; i < n-1; i++)
            printf("%d, ", array[i]);
        printf("%d", array[i]);
    }
    printf("] len = %lu\n", n);
}

/* ---------------------------------------------------------------------------- */
/* Tests */

#define NA INT_MIN

void test(void) {
    /* correct test results */
    const int a[5][17] = {
        {17, 26, 38, 32, 55, 60, 51, 72, 43, 57, 92, 67, 87, 78, 99, NA, NA},
        { 2, 17, 38, 26, 55, 60, 51, 32, 43, 57, 92, 67, 87, 78, 99, 72, NA},
        { 2, 17, 38, 26, 55, 60, 51, 32, 43, 57, 92, 67, 87, 78, 99, 72, 40},
        { 2, 17, 26, 32, 38, 40, 43, 51, 55, 57, 60, 67, 72, 78, 87, 92, 99},
        {99, 92, 87, 78, 72, 67, 60, 57, 55, 51, 43, 40, 38, 32, 26, 17,  2}
    };
    
    const int test_array[17] = {60, 57, 99, 43, 92, 38, 17, 72, 32, 55, 26, 67, 87, 78, 51, 2, 40};

    /* create heap from array */
    heap_t *b = heap_fromarray(test_array, 15);
    printf("New heap:\n");
    heap_print(b);
    putchar('\n');
    assert(array_equal(a[0], 15, b->items, 15));
    assert(LEN(b) == 15);
    assert(SIZE(b) == 16);

    /* push a new item, now the heap len = size = 16 */
    heap_push(b, 2);
    printf("Appended item '2':\n");
    heap_print(b);
    putchar('\n');
    assert(array_equal(a[1], 16, b->items, 16));
    assert(LEN(b) == 16);
    assert(SIZE(b) == 16);

    /* push the next item provoking heap size extension to 32 */
    heap_push(b, 40);
    printf("Appended item '40', size extension:\n");
    heap_print(b);
    putchar('\n');
    assert(array_equal(a[2], 17, b->items, 17));
    assert(LEN(b) == 17);
    assert(SIZE(b) == 32);

    /* sort */
    int *sorted = heap_sort(test_array, 17);
    printf("Sorted array:\n");
    array_print(sorted, 17);
    putchar('\n');
    assert(array_equal(a[3], 17, sorted, 17));

    /* sort (reverse) */
    int *sorted_max = heap_sort_max(test_array, 17);
    printf("Sorted array (descending order):\n");
    array_print(sorted_max, 17);
    putchar('\n');
    assert(array_equal(a[4], 17, sorted_max, 17));

    /* read array from file */
    int *test_array2;
    size_t n = array_fromfile("array.txt", &test_array2);
    printf("Reading array from file:\n");
    array_print(test_array2, n);
    putchar('\n');

    /* don't forget to collect garbage */
    heap_delete(&b);
    free(sorted);
    free(sorted_max);
    free(test_array2);

    printf("Tests pass\n");
}

int main(int argc, char **argv) {
    test();
    exit(EXIT_SUCCESS);
}

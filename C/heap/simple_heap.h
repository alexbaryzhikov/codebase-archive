#ifndef SIMPLE_HEAP_H
#define SIMPLE_HEAP_H

/* Heap Queue */
typedef struct Heap {
    size_t size;
    size_t length;
    int *items;
} heap_t;

/* API */
heap_t *heap_clear(heap_t *);
void   heap_delete(heap_t **);
int    heap_equal(const heap_t *, const heap_t *);
heap_t *heap_fromarray(const int *, size_t);
int    heap_peek(const heap_t *);
int    heap_pop(heap_t *);
void   heap_print(const heap_t *);
void   heap_push(heap_t *, int);
int    heap_pushpop(heap_t *, int);
int    heap_replace(heap_t *, int);
int    *heap_sort(const int *, size_t);

heap_t *heap_fromarray_max(const int *, size_t);
int    heap_pop_max(heap_t *);
void   heap_push_max(heap_t *, int);
int    heap_pushpop_max(heap_t *, int);
int    heap_replace_max(heap_t *, int);
int    *heap_sort_max(const int *, size_t);

#endif

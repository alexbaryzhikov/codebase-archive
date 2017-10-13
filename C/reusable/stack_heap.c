/* Change stack limit, explore stack and heap addresses. */

#include <stdio.h>
#include <stdlib.h>
#include <sys/resource.h>

void print_limit(struct rlimit*);
void check_r(int);

int main() {
    const rlim_t kStackSize = 16 * 1024 * 1024;  // new stack limit
    struct rlimit limit;
    int result;

    getrlimit(RLIMIT_STACK, &limit);
    print_limit(&limit);

    // change stack limit
    if (limit.rlim_cur < kStackSize) {
        printf("Setting stack limit...\n");
        limit.rlim_cur = kStackSize;
        result = setrlimit( RLIMIT_STACK, &limit );
        if (result)
            fprintf( stderr, "error: setrlimit returned %d\n", result);
        print_limit(&limit);
    }

    // explore addresses
    check_r(10);
}

void print_limit(struct rlimit* limit) {
    printf("Stack Limit = %lu and %lu max\n\n", limit->rlim_cur, limit->rlim_max);
}

void check_r( int depth ) {
    char c;
    char *ptr = malloc(24);
    printf("stack at %p, heap at %p\n", &c, ptr);
    if (depth > 0)
        check_r(depth - 1);
}

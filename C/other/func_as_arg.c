#include <stdio.h>
#include <stdlib.h>

/* See if an x,y pair exists for which f(x) = g(y);
 * search each function's domain between 1 and the value for
 * which it returns 0.                                               */
int* search(int f(), int g()) {
    int x, y, fx, gy;
    int* res = malloc(2*sizeof(int));
    
    for (x=0; (fx = f(x)) != 0; x++) {
        for (y=0;; y++) {
            if (!(gy = g(y))) break;
            if (fx != gy) continue;
            else goto GotOne;     /* Found an answer!    */
        }
    }

    return res;     /* No answer found.    */
GotOne:
    res[0] = fx;
    res[1] = gy;
    return res;   /* Found an x,y pair.  */
}

int foo(int x) {
    if      (!x)        return 1;
    else if (x < 10)    return x*4+1;
    return 0;
}

int bar(int x) {
    if      (!x)        return 2;
    else if (x < 5)     return x*2+1;
    return 0;
}

main() {
    int* t = search(foo, bar);
    printf("%d %d\n", t[0], t[1]);
}

/*
Function detects the type of an argument.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum { NUM, STR };

struct Object {
    char type;
};

struct Number {
    char type;
    int  val;
};

struct String {
    char  type;
    char* val;
};

void foo(struct Object* a) {
    if (a->type == NUM)
        printf("%d\n", ((struct Number*)a)->val);
    else
        printf("%s\n", ((struct String*)a)->val);
}


int main() {

    struct Number* n = malloc(sizeof(struct Number));
    n->type = NUM;
    n->val = 99;
    
    struct String* s = malloc(sizeof(struct String));
    s->type = STR;
    s->val = "this is a string";

    foo((struct Object*)n);
    foo((struct Object*)s);

}

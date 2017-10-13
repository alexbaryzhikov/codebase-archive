#include <stdio.h>
#include <string.h>


/* Guarantees safety of s. */
int array_traverse_safe(const char s[]) {
    int n = 0;
    const char** tmp_s = &s;
    while (**tmp_s) {
        putchar(*((*tmp_s)++));
        n++;
    }
    return n;
}


/* Can inadvertently modify s. */
int array_traverse_unsafe(char s[]) {
    int n = 0;
    char* tmp_s = s;
    while (*tmp_s) {
        putchar(*tmp_s++);
        n++;
    }
    return n;
}


int main(int argc, char* argv[]) {
    char s[] = "Lorem ipsum dolor sit amet, consectetur adipiscing elit.";
    printf("\n%d\n", array_traverse_safe(s));
    printf("%s\n", s);
    return 0;
}

/* Read words and make them a sorted quoted list, separated by commas. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAXLINE  1000
#define MAXWORDS 2000

int   getword(char**);
char* lowercase(char*);
int   pstrcmp(char**, char**);

int main() {
    FILE *fp;
    char *word, *words[MAXWORDS];
    int nwords, i;

    // Open input file
    if ((fp = fopen("out.txt", "w")) == NULL) {
        printf("Error: can't open 'out.txt'.\n");
        exit(EXIT_FAILURE);
    }

    // Read words
    nwords = 0;
    while (getword(&word) != EOF) {
        if (isalnum(word[0]))
            words[nwords++] = lowercase(word);
        else
            free(word);
    }

    qsort(words, nwords, sizeof(char*), (int (*)(const void*, const void*))pstrcmp);

    for (i = 0; i < nwords; i++)
        fprintf(fp, "\"%s\", ", words[i]);
    fclose(fp);
}

/* Get word from stdin */
int getword(char** s) {
    static char buf[MAXLINE + 1] = {};
    static char *pbuf, *pend;
    int wlen;

    while (1) {
        // read input string
        if (!strlen(buf))
            if ((pbuf = fgets(buf, MAXLINE, stdin)) == NULL)
                return EOF;
        if (strlen(buf) == MAXLINE)
            printf("warning: getword: input buffer is full\n");
        // search start of word
        for (; isspace(*pbuf); pbuf++);
        // keep reading until word or EOF is found
        if (*pbuf == '\0')
            buf[0] = '\0';
        else
            break;
    }
    
    // search end of word
    for (pend = pbuf; isalnum(*pend) || *pend == '_'; pend++);
    if (!(wlen = pend - pbuf))
        wlen++, pend++;

    // get word
    *s = malloc(wlen + 1);
    strncpy(*s, pbuf, wlen);
    (*s)[wlen] = '\0';
    
    // reset buf if it's consumed
    if (*(pbuf = pend) == '\0')
        buf[0] = '\0';
    
    return **s;
}

/* Convert s to lowercase. */
char* lowercase(char* s) {
    char* p;
    
    for (p = s; *p; p++)
        *p = tolower(*p);
    return s;
}

/* Compare strings pointed to by s1 and s2. */
int pstrcmp(char** s1, char** s2) {
    return strcmp(*s1, *s2);
}

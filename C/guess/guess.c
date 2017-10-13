/*
Guess game. Player thinks about a number (1 or 0), lets agent to guess and
reports true number. Then thinks about the next number and so on.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// #define MANUAL_INPUT
#define STATS_LEN 100
#define _A(i0, i1, i2, i3, i4)  A[i0][i1][i2][i3][i4]

int A[2][3][2][3][2];   /* model */
int a[3];               /* latest player picks */
int b[3];               /* latest teller responses */
int stats[STATS_LEN];   /* results of latest guesses */
int stats_i;
int verbose;
int ibuf[BUFSIZ];
int ibuf_i;


/* Print stats and exit. */
void bailout(void) {
    int i;
    double stat_percent = 0.0;

    for (i = 0; i < STATS_LEN; i++)
        stat_percent += stats[i];
    stat_percent = (stat_percent / (double)STATS_LEN) * 100.0;
    printf("%.1f%% guessed of last %d tries\n", stat_percent, STATS_LEN);
    exit(EXIT_SUCCESS);
}


/* Update model. */
void update(void) {
    _A(a[0], b[0], a[1], b[1], a[2])++;
}


/* Shift latest guesses and picks left, free space for a new round. */
void shift(void) {
    a[0] = a[1];
    a[1] = a[2];
    b[0] = b[1];
    b[1] = b[2];
}


/* Wait for player to think about the next number. */
void wait(void) {
    int c;

    while ((c = getchar()) != '\n' && c != EOF);
    if (c == EOF)
        bailout();
}


/* Do nothing [sic]. */
void nop(void) {}


/* Make verbose guesses, when input is manual. */
int tell(void) {
    int diff;

    diff = _A(a[0], b[0], a[1], b[1], 0) - _A(a[0], b[0], a[1], b[1], 1);
    if (abs(diff) < 2) {
        printf("guess: ?  ");
        return 2;
    }
    if (diff < 0) {
        printf("guess: 1  ");
        return 1;
    }
    printf("guess: 0  ");
    return 0;
}


/* Make guesses in silent mode. */
int tell_auto(void) {
    int diff;

    diff = _A(a[0], b[0], a[1], b[1], 0) - _A(a[0], b[0], a[1], b[1], 1);
    if (diff == 0) 
        return 2;
    if (diff < 0)
        return 1;
    return 0;
}


/* Input numbers from keyboard. */
int input(void) {
    int res, c;
    char buf[2];

    printf("number: ");
    if (fgets(buf, 2, stdin) == NULL)
        bailout();
    while ((c = getchar()) != '\n' && c != EOF);  /* flush stdin */
    if ((res = atoi(buf)))  /* any nonzero value becomes 1 */
        res = 1;
    return res;
}


/* Input numbers from text file via stdin (redirection). */
int input_auto(void) {
    int res;
    char buf[2];

    if (fgets(buf, 2, stdin) == NULL || buf[0] == '\n') {
        ibuf[ibuf_i] = EOF;
        return EOF;
    }
    if ((res = atoi(buf)))  /* any nonzero value becomes 1 */
        res = 1;
    ibuf[ibuf_i++] = res;
    return res;
}


/* Record stats. */
void stat(int a, int b) {
    static int n;

    stats[stats_i++] = a == b;
    stats_i = (stats_i == STATS_LEN) ? 0 : stats_i;
    if (verbose) {
        if (b != 2)
            printf("%d%d ", b, a);
        else
            printf("?%d ", a);
        if (++n == 10) {
            n = 0;
            putchar('\n');
        }
    }
}


/* Play guess game. */
void play(void) {
    int i;
#ifndef MANUAL_INPUT
    void (*wait)(void)  = nop;
    int  (*tell)(void)  = tell_auto;
    int  (*input)(void) = input_auto;
#endif

    /* priming stage */
    for (i = 0; i < 3; i++) {
        wait();
#ifdef MANUAL_INPUT
        printf("guess: ?  ");
#endif
        a[i] = input();
        b[i] = 2;
        stat(a[i], b[i]);
    }
    /* main loop */
    while (1) {
        update();
        shift();
        wait();
        b[2] = tell();
        if ((a[2] = input()) == EOF)
            break;
#ifdef MANUAL_INPUT
        stat(a[2], b[2]);
#endif
    }
}

void test(void) {
    int i, matches;

    /* priming stage */
    for (i = 0; i < 3; i++) {
        a[i] = ibuf[i];
        b[i] = 2;
    }
    /* main loop */
    matches = 0;
    for (; i < ibuf_i; i++) {
        shift();
        b[2] = tell_auto();
        a[2] = ibuf[i];
        printf("%d %d\n", b[2], a[2]);
        if (a[2] == b[2])
            matches++;
    }
    printf("%.1f%% guessed\n", (matches / (ibuf_i - 3.0)) * 100.0);
}


int main(int argc, char *argv[]) {
#ifndef MANUAL_INPUT
    if (argv[1] && strchr(argv[1], 'v'))
        verbose = 1;
#endif
    play();
    test();
}

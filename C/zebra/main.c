/*
The following version of the puzzle appeared in Life International in 1962:

There are five houses.
The Englishman lives in the red house.
The Spaniard owns the dog.
Coffee is drunk in the green house.
The Ukrainian drinks tea.
The green house is immediately to the right of the ivory house.
The Old Gold smoker owns snails.
Kools are smoked in the yellow house.
Milk is drunk in the middle house.
The Norwegian lives in the first house.
The man who smokes Chesterfields lives in the house next to the man with the fox.
Kools are smoked in the house next to the house where the horse is kept.
The Lucky Strike smoker drinks orange juice.
The Japanese smokes Parliaments.
The Norwegian lives next to the blue house.
Now, who drinks water? Who owns the zebra?

In the interest of clarity, it must be added that each of the five houses is
painted a different color, and their inhabitants are of different national
extractions, own different pets, drink different beverages and smoke different
brands of American cigarets [sic]. One other thing: in statement 6, right means
your right.
*/

#include <stdio.h>
#include <stdbool.h>
#include <time.h>
#include "defs.h"


/* House h1 is immediately right of h2 if h1 - h2 == 1. */
bool imright(int h1, int h2) {
    return h1 - h2 == 1;
}


/* Two houses are next to each other if they differ by 1. */
bool nextto(int h1, int h2) {
    return (h1 - h2 == 1) || (h2 - h1 == 1);
}


/* Assign corresponding members of source to v1...v5. */
void assign(int *v1, int *v2, int *v3, int *v4, int *v5, int source[]) {
    *v1 = source[0];
    *v2 = source[1];
    *v3 = source[2];
    *v4 = source[3];
    *v5 = source[4];
}


/* For v1...v5 in orderings: evaluate 'next'. */
#define ROLL(v1, v2, v3, v4, v5, next)  { \
    size_t i; \
    for (i = 0; i < orderings_len; i++) { \
        assign(&v1, &v2, &v3, &v4, &v5, orderings[i]); \
        next \
    } \
}


int main(void) {
    int    houses[] = { 1, 2, 3, 4, 5 };
    int    first = 1, middle = 3;
    int    **orderings = perms(houses, 5, 0);
    size_t orderings_len = nPk(5, 5);
    int    red, green, ivory, yellow, blue;
    int    Englishman, Spaniard, Ukrainian, Japanese, Norwegian;
    int    coffee, tea, milk, oj, water;
    int    OldGold, Kools, Chesterfields, LuckyStrike, Parliaments;
    int    dog, snails, fox, horse, zebra;
    clock_t t1, t2;

    t1 = clock();
    ROLL (red, green, ivory, yellow, blue, {
    if   (imright(green, ivory))
    ROLL (Englishman, Spaniard, Ukrainian, Japanese, Norwegian, {
    if   (Englishman == red)
    if   (Norwegian == first)
    if   (nextto(Norwegian, blue))
    ROLL (coffee, tea, milk, oj, water, {
    if   (coffee == green)
    if   (Ukrainian == tea)
    if   (milk == middle)
    ROLL (OldGold, Kools, Chesterfields, LuckyStrike, Parliaments, {
    if   (Kools == yellow)
    if   (LuckyStrike == oj)
    if   (Japanese == Parliaments)
    ROLL (dog, snails, fox, horse, zebra, {
    if   (Spaniard == dog)
    if   (OldGold == snails)
    if   (nextto(Chesterfields, fox))
    if   (nextto(Kools, horse))
    goto found;
    }) }) }) }) })

found:
    t2 = clock();
    perms_free(orderings);
    printf("water = %d, zebra = %d\n", water, zebra);
    printf("time: %ldus\n", t2-t1);
}

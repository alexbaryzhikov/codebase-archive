/*
A Regular Expression Matcher

(1) Add other metacharacters, like + for one or more occurrences of the previous
character, or ? for zero or one matches. Add some way to quote metacharacters,
like \$ to stand for a literal occurrence of $.

(2) Separate regular expression processing into a "compilation" phase and an
"execution" phase. Compilation converts the regular expression into an internal
form that makes the matching code simpler or such that subsequent matching runs
faster. This separation is not necessary for the simple class of regular
expressions in the original design, but it makes sense in grep-like applications
where the class is richer and the same regular expression is used for a large
number of input lines.

(3) Add character classes like [abc] and [0-9], which in conventional grep
notation RE_match a or b or c and a digit respectively. This can be done in several
ways, the most natural of which seems to be replacing the char*'s of the
original code with a structure:

    typedef struct RE {
            int     type;   // CHAR, STAR, etc.
            char    ch;     // the character itself
            char    *ccl;   // for [...] instead
            int     nccl;   // true if class is negated [^...]
    } RE;

and modifying the basic code to handle an array of these instead of an array of
characters. It's not strictly necessary to separate compilation from execution
for this situation, but it turns out to be a lot easier. Students who follow the
advice to pre-compile into such a structure invariably do better than those who
try to interpret some complicated pattern data structure on the fly.
Writing clear and unambiguous specifications for character classes is tough, and
implementing them perfectly is worse, requiring a lot of tedious and
uninstructive coding. I have simplified this assignment over time, and today
most often ask for Perl-like shorthands such as \d for digit and \D for
non-digit instead of the original bracketed ranges.

(4) Use an opaque type to hide the RE structure and all the implementation
details. This is a good way to show object-oriented programming in C, which
doesn't support much beyond this. In effect, one makes a regular expression
class but with function names like RE_new() and RE_match() for the methods
instead of the syntactic sugar of an object-oriented language.

TODO:
(5) Modify the class of regular expressions to be like the wild cards in various
shells: matches are implicitly anchored at both ends, * matches any number of
characters, and ? matches any single character. One can modify the algorithm or
map the input into the existing algorithm.
*/

#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* ---------------------------------------------------------------------------- */
/* RE class */

#define MATCH_LONGEST_CLOSURE   /* use re_matchstar that matches leftmost longest c* sequence */
#define RE_SIZE         100     /* maximum tokens in RE list */

enum Char_types { NULL_C, CHAR_C, BEG_C, END_C, STAR_C, PLUS_C, QMARK_C, DOT_C, SEQ_C };

typedef struct RE {
    int     type;   /* CHAR_C, STAR_C, etc. */
    char    ch;     /* the character itself */
    char    *ccl;   /* for [...] instead */
    bool    nccl;   /* true if class is negated [^...] */
} re_t;

re_t *RE_new(void);
void RE_free(re_t *);
bool RE_match(char *, char *);  /* user interface */
bool RE_matchhere(re_t *, char *);
bool RE_matchsym(re_t *, int);
bool RE_matchstar(re_t *, re_t *, char *);
re_t *RE_convert(const char *);
char *RE_expand(const char *);


/* RE_new: create new RE list */
re_t *RE_new(void) {
    re_t *re;

    re = malloc(RE_SIZE * sizeof(re_t));
    memset(re, 0, RE_SIZE * sizeof(re_t));
    return re;
}


/* RE_free: free re resources */
void RE_free(re_t *re) {
    for (re_t *p = re; p->type != NULL_C; p++)
        if (p->ccl)
            free(p->ccl);
    free(re);
}


/* RE_match: search for regexp anywhere in text */
bool RE_match(char *regexp, char *text) {
    re_t *re;
    bool res;

    re = RE_convert(regexp);
    if (re[0].type == BEG_C) {
        res = RE_matchhere(re+1, text);
        RE_free(re);
        return res;
    }
    do {    /* must look even if string is empty */
        if (RE_matchhere(re, text)) {
            RE_free(re);
            return true;
        }
    } while (*text++ != '\0');
    RE_free(re);
    return false;
}


/* RE_matchhere: search for regexp at beginning of text */
bool RE_matchhere(re_t *re, char *text) {
    if (re[0].type == NULL_C)
        return true;
    if (re[0].type == END_C && re[1].type == NULL_C)
        return *text == '\0';
    if (re[1].type == STAR_C)
        return RE_matchstar(re, re+2, text);
    if (re[1].type == PLUS_C)
        return RE_matchsym(re, *text) && RE_matchstar(re, re+2, text+1);
    if (re[1].type == QMARK_C) {
        if (RE_matchsym(re, *text) && RE_matchhere(re+2, text+1))
            return true;
        else
            return RE_matchhere(re+2, text);
    }
    if (RE_matchsym(re, *text))
        return RE_matchhere(re+1, text+1);
    return false;
}


/* RE_matchsym: RE_match re symbol to character */
bool RE_matchsym(re_t *re, int c) {
    if (c == '\0')
        return false;
    switch (re->type) {
    case DOT_C:
        return true;
    case CHAR_C:
        return re->ch == c;
    case SEQ_C:
        if (re->nccl)
            return !strchr(re->ccl, c);
        else
            return strchr(re->ccl, c);
    default:
        fprintf(stderr, "RE_matchsym: Invalid symbol type: %d", re->type);
        break;
    }
    return false;
}


#ifndef MATCH_LONGEST_CLOSURE

/* RE_matchstar: search for c*regexp at beginning of text */
bool RE_matchstar(re_t *c, re_t *re, char *text) {
    do {    /* a * matches zero or more instances */
        if (RE_matchhere(re, text))
            return true;
    } while (RE_matchsym(c, *text++));
    return false;
}

#else

/* RE_matchstar: leftmost longest search for c*regexp */
bool RE_matchstar(re_t *c, re_t *re, char *text) {
    char *t;

    for (t = text; RE_matchsym(c, *t); t++);
    do {    /* * matches zero or more */
        if (RE_matchhere(re, t))
            return true;
    } while (t-- > text);
    return false;
}

#endif


/* RE_convert: Converts the regular expression into an internal form */
re_t *RE_convert(const char *regexp) {
    int   i;
    re_t* re = RE_new();
    
    for (i = 0; i < RE_SIZE-1 && *regexp; regexp++, i++)
        switch (*regexp) {
        case '^':
            re[i].type = BEG_C;
            break;
        case '$':
            re[i].type = END_C;
            break;
        case '*':
            re[i].type = STAR_C;
            break;
        case '+':
            re[i].type = PLUS_C;
            break;
        case '?':
            re[i].type = QMARK_C;
            break;
        case '.':
            re[i].type = DOT_C;
            break;
        case '\\':
            re[i].type = CHAR_C;
            re[i].ch = *++regexp;
            break;
        case '[':
            re[i].type = SEQ_C;
            re[i].ccl = RE_expand(regexp);
            re[i].nccl = (regexp[1] == '^');
            for (; *regexp != ']'; regexp++);
            break;
        default:
            re[i].type = CHAR_C;
            re[i].ch = *regexp;
            break;
        }
    return re;
}


/* RE_expand: convert ranges within brackets into sequence of characters */
char *RE_expand(const char *regexp) {
    char *ccl, *p;
    int c, step;

    ccl = malloc(256);
    p = ccl;
    if (*++regexp == '^')
        regexp++;
    if (*regexp == '\0' || *regexp == ']')
        goto end;
    *(p++) = *(regexp++);           /* copy first character */
    if (*regexp == '\0' || *regexp == ']')
        goto end;
    for (; *regexp != '\0' && *regexp != ']'; regexp++) {
        if (*regexp == '-' &&
            ((isupper(regexp[-1]) && isupper(regexp[1])) ||
             (islower(regexp[-1]) && islower(regexp[1])) ||
             (isdigit(regexp[-1]) && isdigit(regexp[1])))) {
            if (regexp[-1] != regexp[1]) {
                step = (regexp[-1] < regexp[1]) ? 1 : -1;
                for (c = regexp[-1] + step; c != regexp[1]; c += step)
                    *(p++) = c;     /* expand sequence */
            } else
                regexp++;           /* skip duplicate characters */
        } else
            *(p++) = regexp[0];     /* copy current character */
    }
end:
    *p = '\0';
    return ccl;
}


/* ---------------------------------------------------------------------------- */
/* Tests */


#define ASSERT(pat, s) { \
    for (int i = 0; i < (sizeof(s) / sizeof(s[0])); i++) \
        assert(RE_match(pat, s[i])); \
}


#define ASSERTN(pat, s) { \
    for (int i = 0; i < (sizeof(s) / sizeof(s[0])); i++) \
        assert(RE_match(pat, s[i]) == false); \
}


void test_match(void) {
    char *s1[] = {"abc", "aaabbccc", "aaaabcccc"};
    char *s2[] = {"ac", "aaabbcccf", "aaaa-b-cccc"};
    char *s3[] = {"abracadabra", "abacaa", "about-acacia-ferma"};
    char *s4[] = {"tip", "top", "tap", "atypical", "tepid", "stop"};
    char *s5[] = {"TYPE", "teepee", "tp"};

    assert(RE_match("baa*!", "Sheep said baaaa!"));
    assert(RE_match("baa*!", "Sheep said baaaa humbug") == false);
    assert(RE_match("^baa*!", "Sheep said baaaa!") == false);
    assert(RE_match("^baa*!", "baaaaaaa! said the sheep"));
    assert(RE_match("^ba+!$", "baaaaaaa!"));
    assert(RE_match("^ba+!$", "baaaaaaa!!") == false);
    assert(RE_match("def", "abcdefg"));
    assert(RE_match("def$", "abcdef"));
    assert(RE_match("def$", "abcdefg") == false);
    assert(RE_match("^start", "not the start") == false);
    assert(RE_match("^a*b*c*", "just anything"));
    assert(RE_match("^x?", "text"));
    assert(RE_match("^x?t", "text"));
    assert(RE_match("^text?", "text"));
    assert(RE_match("^text?", "tex"));
    assert(RE_match("a+bb", "123bbcc") == false);
    assert(RE_match("a+bb", "123abbcc"));
    assert(RE_match("a+bb", "123aaaababbcc"));
    assert(RE_match("a+b", "bbcca") == false);
    assert(RE_match("ab\\$", "aba aab$bcc"));
    assert(RE_match("ab\\a", "aba aababcc"));
    ASSERT("^aa*bb*cc*$", s1)
    ASSERTN("^aa*bb*cc*$", s2)
    ASSERT("^ab.*aca.*a$", s3)
    ASSERT("t.p", s4)
    ASSERTN("t.p", s5)
    ASSERT("[ab]*c", s1)
    ASSERT("t[a-z]p", s4)
    assert(RE_match("t[^io]p", "tip") == 0);
    assert(RE_match("t[^io]p", "tap"));
    printf("[ \x1b[92mOK\x1b[0m ] test_match\n");
}


void test_expand(void) {
    /* these tests leak memory (voluntarily) */
    assert(!strcmp(RE_expand("[abc]"), "abc"));
    assert(!strcmp(RE_expand("[]"), ""));
    assert(!strcmp(RE_expand("[^]"), ""));
    assert(!strcmp(RE_expand("[a]"), "a"));
    assert(!strcmp(RE_expand("[-a]"), "-a"));
    assert(!strcmp(RE_expand("[a-]"), "a-"));
    assert(!strcmp(RE_expand("[-abc-]"), "-abc-"));
    assert(!strcmp(RE_expand("[a-d]"), "abcd"));
    assert(!strcmp(RE_expand("[d-a]"), "dcba"));
    assert(!strcmp(RE_expand("[1-5a-c]"), "12345abc"));
    assert(!strcmp(RE_expand("[A-C-F]"), "ABCDEF"));
    assert(!strcmp(RE_expand("[a-C]"), "a-C"));
    assert(!strcmp(RE_expand("[a-a]"), "a"));
    assert(!strcmp(RE_expand("[a-b-c]"), "abc"));
    printf("[ \x1b[92mOK\x1b[0m ] test_expand\n");
}


void test_matchsym(void) {
    re_t r;

    r.type = DOT_C;
    r.ch = 'a';
    r.ccl = "abc";
    r.nccl = 0;
    assert(RE_matchsym(&r, '\0') == 0);
    assert(RE_matchsym(&r, 'a'));
    r.type = CHAR_C;
    assert(RE_matchsym(&r, 'a'));
    assert(RE_matchsym(&r, 'b') == 0);
    r.type = SEQ_C;
    assert(RE_matchsym(&r, 'b'));
    assert(RE_matchsym(&r, 'x') == 0);
    printf("[ \x1b[92mOK\x1b[0m ] test_matchsym\n");
}


int main(void) {
    test_expand();
    test_matchsym();
    test_match();
}

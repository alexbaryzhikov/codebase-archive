/*
Prints out a table of standard types, their sizes, minimums and maximums.
*/
#include <stdio.h>      // size_t type
#include <limits.h>     // limits of basic integral types
#include <float.h>      // limits of basic floating point types
#include <stdint.h>     // fixed-width integer types
#include <math.h>       // float_t and double_t
#include <stdbool.h>    // boolean type

bool contains(char*, int);

int main(int argc, char** argv) {
    if (argc < 2) {
        printf("USAGE\n\ttype_ranges [OPTIONS]\n");
        printf("OPTIONS\n");
        printf("\ta\tall types\n");
        printf("\tb\tbase types\n");
        printf("\ti\tstdint.h types\n");
        printf("\tm\tmath.h types\n");
        return 0;
    }

    // basic types
    if (contains(argv[1], 'b') || contains(argv[1], 'a')) {
        printf("\nBasic integral types\n");
        printf("--------------------------------------------------------------------\n");
        printf("type              size                    min                    max\n\n");
        printf("char               %2lu%23d%24d\n", sizeof(char), CHAR_MIN, CHAR_MAX);
        printf("\n");
        // signed types
        printf("signed char        %2lu%23d%24d\n", sizeof(signed char), SCHAR_MIN, SCHAR_MAX);
        printf("short              %2lu%23d%24d\n", sizeof(short), SHRT_MIN, SHRT_MAX);
        printf("int                %2lu%23d%24d\n", sizeof(int), INT_MIN, INT_MAX);
        printf("long               %2lu%23li%24li\n", sizeof(long), LONG_MIN, LONG_MAX);
        printf("long long          %2lu%23lli%24lli\n", sizeof(long long), LLONG_MIN, LLONG_MAX);
        printf("\n");
        // unsigned types
        printf("unsigned char      %2lu%23d%24u\n", sizeof(unsigned char), 0, UCHAR_MAX);
        printf("unsigned short     %2lu%23d%24u\n", sizeof(unsigned short), 0, USHRT_MAX);
        printf("unsigned int       %2lu%23d%24u\n", sizeof(unsigned int), 0, UINT_MAX);
        printf("unsigned long      %2lu%23d%24lu\n", sizeof(unsigned long), 0, ULONG_MAX);
        printf("unsigned long long %2lu%23d%24llu\n", sizeof(unsigned long long), 0, ULLONG_MAX);
        printf("size_t             %2lu%23d%24llu\n", sizeof(size_t), 0, ULLONG_MAX);
        printf("\n");
        // floating point types
        printf("\nBasic floating point types\n");
        printf("--------------------------------------------------------------------\n");
        printf("type          size    digits                 min                 max\n\n");
        printf("float       %6lu%10d%20e%20e\n", sizeof(float), FLT_DIG, FLT_MIN, FLT_MAX);
        printf("double      %6lu%10d%20e%20e\n", sizeof(double), DBL_DIG, DBL_MIN, DBL_MAX);
        printf("long double %6lu%10d%20Le%20Le\n", sizeof(long double), LDBL_DIG, LDBL_MIN, LDBL_MAX);
        printf("\n");
    }
    
    // stdint.h types
    if (contains(argv[1], 'i') || contains(argv[1], 'a')) {
        printf("\nsdtint.h types\n");
        printf("--------------------------------------------------------------------\n");
        printf("type          size                      min                      max\n\n");
        // signed types
        printf("int8_t         %3lu%25d%25d\n", sizeof(int8_t), INT8_MIN, INT8_MAX);
        printf("int16_t        %3lu%25d%25d\n", sizeof(int16_t), INT16_MIN, INT16_MAX);
        printf("int32_t        %3lu%25d%25d\n", sizeof(int32_t), INT32_MIN, INT32_MAX);
        printf("int64_t        %3lu%25li%25li\n", sizeof(int64_t), INT64_MIN, INT64_MAX);
        printf("\n");
        printf("int_least8_t   %3lu%25d%25d\n", sizeof(int_least8_t), INT_LEAST8_MIN, INT_LEAST8_MAX);
        printf("int_least16_t  %3lu%25d%25d\n", sizeof(int_least16_t), INT_LEAST16_MIN, INT_LEAST16_MAX);
        printf("int_least32_t  %3lu%25d%25d\n", sizeof(int_least32_t), INT_LEAST32_MIN, INT_LEAST32_MAX);
        printf("int_least64_t  %3lu%25li%25li\n", sizeof(int_least64_t), INT_LEAST64_MIN, INT_LEAST64_MAX);
        printf("\n");
        printf("int_fast8_t    %3lu%25d%25d\n", sizeof(int_fast8_t), INT_FAST8_MIN, INT_FAST8_MAX);
        printf("int_fast16_t   %3lu%25li%25li\n", sizeof(int_fast16_t), INT_FAST16_MIN, INT_FAST16_MAX);
        printf("int_fast32_t   %3lu%25li%25li\n", sizeof(int_fast32_t), INT_FAST32_MIN, INT_FAST32_MAX);
        printf("int_fast64_t   %3lu%25li%25li\n", sizeof(int_fast64_t), INT_FAST64_MIN, INT_FAST64_MAX);
        printf("\n");
        printf("intptr_t       %3lu%25li%25li\n", sizeof(intptr_t), INTPTR_MIN, INTPTR_MAX);
        printf("intmax_t       %3lu%25li%25li\n", sizeof(intmax_t), INTMAX_MIN, INTMAX_MAX);
        printf("\n");
        // unsigned types
        printf("uint8_t        %3lu%25d%25u\n", sizeof(uint8_t), 0, UINT8_MAX);
        printf("uint16_t       %3lu%25d%25u\n", sizeof(uint16_t), 0, UINT16_MAX);
        printf("uint32_t       %3lu%25d%25u\n", sizeof(uint32_t), 0, UINT32_MAX);
        printf("uint64_t       %3lu%25d%25lu\n", sizeof(uint64_t), 0, UINT64_MAX);
        printf("\n");
        printf("uint_least8_t  %3lu%25d%25u\n", sizeof(uint_least8_t), 0, UINT_LEAST8_MAX);
        printf("uint_least16_t %3lu%25d%25u\n", sizeof(uint_least16_t), 0, UINT_LEAST16_MAX);
        printf("uint_least32_t %3lu%25d%25u\n", sizeof(uint_least32_t), 0, UINT_LEAST32_MAX);
        printf("uint_least64_t %3lu%25d%25lu\n", sizeof(uint_least64_t), 0, UINT_LEAST64_MAX);
        printf("\n");
        printf("uint_fast8_t   %3lu%25d%25u\n", sizeof(uint_fast8_t), 0, UINT_FAST8_MAX);
        printf("uint_fast16_t  %3lu%25d%25lu\n", sizeof(uint_fast16_t), 0, UINT_FAST16_MAX);
        printf("uint_fast32_t  %3lu%25d%25lu\n", sizeof(uint_fast32_t), 0, UINT_FAST32_MAX);
        printf("uint_fast64_t  %3lu%25d%25lu\n", sizeof(uint_fast64_t), 0, UINT_FAST64_MAX);
        printf("\n");
        printf("uintptr_t      %3lu%25d%25lu\n", sizeof(uintptr_t), 0, UINTPTR_MAX);
        printf("uintmax_t      %3lu%25d%25lu\n", sizeof(uintmax_t), 0, UINTMAX_MAX);
        printf("\n");
    }

    // math.h types
    if (contains(argv[1], 'm') || contains(argv[1], 'a')) {
        printf("\nmath.h types\n");
        printf("--------------------------------------------------------------------\n");
        printf("type          size    digits                 min                 max\n\n");
        if (FLT_EVAL_METHOD == 0) {
            printf("float_t   %8lu%10d%20e%20e\n", sizeof(float_t), FLT_DIG, FLT_MIN, FLT_MAX);
            printf("double_t  %8lu%10d%20e%20e\n", sizeof(double_t), DBL_DIG, DBL_MIN, DBL_MAX);
        } else if (FLT_EVAL_METHOD == 1) {
            printf("float_t   %8lu%10d%20e%20e\n", sizeof(float_t), DBL_DIG, DBL_MIN, DBL_MAX);
            printf("double_t  %8lu%10d%20e%20e\n", sizeof(double_t), DBL_DIG, DBL_MIN, DBL_MAX);
        } else if (FLT_EVAL_METHOD == 2) {
            printf("float_t   %8lu%10d%20Le%20Le\n", sizeof(float_t), LDBL_DIG, LDBL_MIN, LDBL_MAX);
            printf("double_t  %8lu%10d%20Le%20Le\n", sizeof(double_t), LDBL_DIG, LDBL_MIN, LDBL_MAX);
        } else {
            printf("float_t and double_t are undefined\n");
            printf("FLT_EVAL_METHOD = %d\n", FLT_EVAL_METHOD);
        }
        printf("\n");
    }
    return 0;
}

bool contains(char* str, int c) {
    /* Returns TRUE if string contains given member. */
    for (char* p = str; *p != '\0'; p++)
        if (c == *p)
            return true;
    return false;
}

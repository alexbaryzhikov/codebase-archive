#include <stdio.h>
#include <stdlib.h>

int main()
{
    float a = 0.375;

    printf( "%#x\n", &a );
    printf( "%.10f\n", a );

    printf( "%d\n", 0.2 == 0.2f ); // false due to float error

    return 0;
}

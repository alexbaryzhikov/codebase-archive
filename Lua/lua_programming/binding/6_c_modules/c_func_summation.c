/*
Write a summation function, in C, that computes the sum of its
variable number of numeric arguments:
print( summation() ) 					--> 0
print( summation( 2.3, 5.4 ) )			--> 7.7
print( summation( 2.3, 5.4, -34 ) )		--> -26.3
print( summation( 2.3, 5.4, {} ) )		--> stdin:1: bad argument #3 to 'summation'
										(number expected, got table)
*/
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <errno.h>
#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

enum mytypes { INT, DOUBLE };

double summation( int num, ... ) {
	va_list argp;
	double acc = 0;

	va_start( argp, num );

	for ( int i = 0; i < num; i++ ) {
		int type = va_arg( argp, enum mytypes );
		switch ( type ) {
			case INT:
			acc += ( double ) va_arg( argp, int );
			break;
			case DOUBLE:
			acc += va_arg( argp, double );
			break;
			default:
			puts( "Whoopsie" );
			break;
		}
	}

	va_end( argp );

	return acc;
}

int main( int argc, char **argv ) {

	printf( "%f\n", summation( 3, DOUBLE, 2.3, DOUBLE, 5.4, INT, -34 ) );

	return 0;
}

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
#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

static int summation( lua_State *L ) {
	// double acc = 0;
	int argc = lua_gettop( L );
	double argv[ argc ];
	// get arguments
	for ( int i = 0; i < argc; i++ ) {
		argv[ i ] = lua_tonumber( L, i );
		printf( "argv[ %d ] = %f\n", i, argv[ i ] );
	}

	return 1;
}

static const struct luaL_Reg c_func_summation[] = {
	{ "summation", summation },
	{ NULL, NULL } // sentinel
};

int luaopen_c_func_summation( lua_State *L ) {
	luaL_newlib( L, c_func_summation );
	return 1;
}

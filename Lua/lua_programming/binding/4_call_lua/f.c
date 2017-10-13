#include <stdarg.h>
#include <stdlib.h>
#include <errno.h>
#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

//===================================
// Error
//===================================
void Error( lua_State *L, const char *fmt, ... ) {
	va_list argp;
	va_start( argp, fmt );
	vfprintf( stderr, fmt, argp );
	va_end( argp );
	lua_close( L );
	exit( EXIT_FAILURE );
}

//===================================
// CallF
//
//   Call a function 'f' defined in Lua
//===================================
double CallF( lua_State *L, double x, double y ) {
	int isnum;
	double z;

	// push functions and arguments
	lua_getglobal( L, "f" );	// function to be called
	lua_pushnumber( L, x );		// push 1st argument
	lua_pushnumber( L, y );		// push 2nd argument

	// do the call (2 arguments, 1 result)
	if ( lua_pcall( L, 2, 1, 0 ) != LUA_OK )
		Error( L, "Error running function 'f': %s", lua_tostring( L, -1) );

	// retrieve result
	z = lua_tonumberx( L, -1, &isnum );
	if ( !isnum )
		Error( L, "function 'f' must return a number" );
	lua_pop( L, 1 );			// pop returned value
	return z;
}

//===================================
// main
//===================================
int main( int argc, char* argv[] ) {
	// check number of args
	if ( argc < 3 ) {
		printf( "Provide two arguments\n" );
		exit( EXIT_FAILURE );
	}
	// convert args to int
	double args[ argc - 1 ];
	char *p;
	errno = 0;
	for( int i = 1; i <= argc - 1; i++ ) {
		args[ i - 1 ] = strtod( argv[ i ], &p );
		if ( errno != 0 || *p != '\0' ) {
			printf( "wrong argument: %s\n", argv[ i ] );
			exit( EXIT_FAILURE );
		}
	}
	// printf( "%g %g\n", args[0], args[1] );

	lua_State *L = luaL_newstate();
	luaL_openlibs( L );

	// load and prime lua file
	if ( luaL_loadfile( L, "call_lua_func_f.lua" ) || lua_pcall( L, 0, 0, 0) )
		Error( L, "Cannot run script. File: %s\n", lua_tostring( L, -1 ) );

	// call lua function 'f'
	double ans;
	ans = CallF( L, args[0], args[1] );
	printf( "answer = %g\n", ans );

	lua_close( L );
	return 0;
}
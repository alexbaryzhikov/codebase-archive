#include <stdio.h>
#include <math.h>
#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

//===================================
// L_Sin
//===================================
static int L_Sin( lua_State *L ) {
	double d = luaL_checknumber( L, 1 );	// get argument
	lua_pushnumber( L, sin( d ) );			// push result
	return 1;								// number of results
}

//===================================
// main
//===================================
int main () {
	char buff[256];
	int error;

	lua_State *L = luaL_newstate();			// opens Lua
	luaL_openlibs( L );						// opens the standard libraries

	lua_pushcfunction( L, L_Sin );			// register function
	lua_setglobal( L, "mysin" );			// create global for function access

	printf( "> " );
	while ( fgets( buff, sizeof( buff ), stdin ) != NULL ) {
		error = luaL_loadstring( L, buff ) || lua_pcall( L, 0, 0, 0 );
		if ( error ) {
			fprintf( stderr, "%s\n", lua_tostring( L, -1 ) );
			lua_pop( L, 1 );  				// pop error message from the stack */
		}
		printf( "> " );
	}

	lua_close( L );
	return 0;
}

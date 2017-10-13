//==================================================================
// 
//	Calling subroutines, sending arguments and receiving returns
//
//==================================================================
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
#include <stdlib.h>
#include <stdio.h>

void bail( lua_State *L, char *msg ) {
	fprintf( stderr, "\nFATAL ERROR:\n  %s: %s\n\n",
		msg, lua_tostring( L, -1 ) );
	exit( 1 );
}

int main( void )
{
	lua_State *L = luaL_newstate();
	luaL_openlibs( L );
	// load callfuncscipt.lua
	if ( luaL_loadfile( L, "callfuncscript.lua" ) )
	   bail( L, "luaL_loadfile() failed" );
	if ( lua_pcall( L, 0, 0, 0 ) )				// PRIMING RUN. FORGET THIS AND YOU'RE TOAST
	   bail( L, "lua_pcall() failed" );			// Error out if Lua file has an error
	// call lua.tellme()
	printf( "C: Calling Lua->tellme()\n" );
	lua_getglobal( L, "tellme" );				// Tell what function to run
	if ( lua_pcall( L, 0, 0, 0 ) )
		bail( L, "lua_pcall() failed" );
	printf( "C: Back in C again\n" );
	// call lua.square()
	printf( "C: Calling Lua->square( 6 )\n" );
	lua_getglobal( L, "square" );				// Tell it to run callfuncscript.lua->square()
	lua_pushnumber( L, 6 );						// Submit 6 as the argument to suare()
	if ( lua_pcall( L, 1, 1, 0 ) )				// Run function, !!! NRETURN=1 !!!
		bail( L, "lua_pcall() failed" );
	printf( "C: Back in C again\n" );
	int mynumber = lua_tonumber( L, -1 );
	printf( "C: Returned number = %d\n", mynumber );
	lua_close( L );
	return 0;
}
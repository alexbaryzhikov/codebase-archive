//==================================================================
// 
//	Passing tables to Lua functions
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
	if ( luaL_loadfile( L, "passtablescript.lua" ) ) bail( L, "luaL_loadfile() failed" );
	// priming run
	if ( lua_pcall( L, 0, 0, 0 ) ) bail( L, "lua_pcall() failed" );
	// call lua.tweaktable()
	printf( "C: Calling Lua->tweaktable()\n" );
	lua_getglobal( L, "tweaktable" );	// tell it to run passtablescript.lua->tweaktable()
	lua_newtable( L );					// push empty table onto stack, table now at -1
	lua_pushliteral( L, "fname" );		// push a key onto the stack, table now at -2
	lua_pushliteral( L, "Walter" );		// push a value onto the stack, table now at -3
	lua_settable( L, -3 );				// take key and value, put ino table at -3,
										//   then pop key and value so table again at -1
	lua_pushliteral( L, "lname" );		// push a key onto the stack, table now at -2
	lua_pushliteral( L, "White" );		// push a value onto the stack, table now at -3
	lua_settable( L, -3 );				// take key and value, put ino table at -3,
										//   then pop key and value so table again at -1
	// run function, !!! NRETURN = 1 !!!
	if ( lua_pcall( L, 1, 1, 0 ) ) bail( L, "lua_pcall() failed" ); 
	printf( "C: =========== Iterating thru returned table ===========\n" );
	// table is in the stack at index 't'
	lua_pushnil( L );					// make sure lua_next starts at the beginning
	const char *k, *v;
	while( lua_next( L, -2 ) ) {		// TABLE LOCATED AT -2 IN STACK
		v = lua_tostring( L, -1 );		// value at stacktop
		lua_pop( L, 1 );				// remove value
		k = lua_tostring( L, -1 );		// read key at stacktop,
										//   leave in place to guide next lua_next()
		printf( "C: Fromc k = %s, v = %s\n", k, v );
	}

	lua_close( L );
	return 0;
}
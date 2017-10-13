#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <dirent.h>
#include <errno.h>
#include <string.h>

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
// L_Dir
//===================================
static int L_Dir( lua_State *L ) {
	DIR *dir;
	struct dirent *entry;
	const char *path = luaL_checkstring( L, 1 );

	// open directory
	dir = opendir( path );
	if ( dir == NULL ) {  						// error opening the directory?
		lua_pushnil( L ); 						// return nil...
		lua_pushstring( L, strerror( errno ) );	// and error message
		return 2;								// number of results
	}

	// create result table
	lua_newtable( L );
	int i = 1;
	while ( ( entry = readdir( dir ) ) != NULL ) {
		lua_pushnumber( L, i++ );				// push key
		lua_pushstring( L, entry->d_name );		// push value
		lua_settable( L, -3 );
	}

	closedir( dir );
	return 1;									// table is already on top
}

//===================================
// main
//===================================
int main( int argc, char *argv[] ) {
	// check path argument
	if ( argc < 2 ) {
		printf( "Provide a directory path\n" );
		exit( EXIT_FAILURE );
	}
	
	lua_State *L = luaL_newstate();
	luaL_openlibs( L );

	// register function L_Dir
	lua_pushcfunction( L, L_Dir );
	lua_setglobal( L, "mydir" );

	// push path
	lua_pushstring( L, argv[1] );
	lua_setglobal( L, "path" );

	// run Lua script
	if ( luaL_loadfile( L, "call_c_func_dir.lua" ) || lua_pcall( L, 0, 0, 0) )
		Error( L, "Cannot run script: %s\n", lua_tostring( L, -1 ) );
	
	lua_close( L );

	return 0;
}
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
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
// Call_va
//
//   Wrapper for a Lua function call. Usage example:
//   Call_va( L, "f", "dd>d", x, y, &z );
//===================================
void Call_va( lua_State *L, const char *func, const char *sig, ... ) {
	va_list vl;
	int narg, nres;					// number of arguments and results
	va_start( vl, sig );
	lua_getglobal( L, func );		// push function

	//  push arguments
	for ( narg = 0; *sig; narg++ ) {	// repeat for each argument
		// check stack space
		luaL_checkstack( L, 1, "too many arguments" );
		switch ( *sig++ ) {
			case 'd':				// double argument
				lua_pushnumber( L, va_arg( vl, double ) );
				break;
			case 'i':				// int argument
				lua_pushinteger( L, va_arg( vl, int ) );
				break;
			case 's':				// string argument
				lua_pushstring( L, va_arg( vl, char * ) );
				break;
			case 'b':				// bool argument
				lua_pushboolean( L, va_arg( vl, int ) );
				break;
			case '>':				// end of aruments
				goto endargs;
			default:
				Error( L, "invalid option (%c)", *( sig - 1 ) );
		}
	}
	endargs:

	nres = strlen( sig );			// number of expected results
	if ( lua_pcall( L, narg, nres, 0 ) != 0 )	// do the call
		Error( L, "error calling '%s': %s", func, lua_tostring( L, -1) );

	// retrieve results
	nres = - nres;					// stack index of first result
	while ( *sig ) {				// repeat for each result
		switch ( *sig++ ) {
			case 'd': {
				int isnum;
				double n = lua_tonumberx( L, nres, &isnum );
				if ( !isnum )
					Error( L, "wrong result type" );
				*va_arg( vl, double * ) = n;
				break;
			}
			case 'i': {
				int isnum;
				int n = lua_tointegerx( L, nres, &isnum );
				if ( !isnum )
					Error( L, "wrong result type" );
				*va_arg( vl, int * ) = n;
				break;
			}
			case 's': {
				const char *s = lua_tostring( L, nres );
				if (s == NULL )
					Error( L, "wrong result type" );
				*va_arg( vl, const char ** ) = s;
				break;
			}
			case 'b': {
				bool b = lua_toboolean( L, nres );
				if ( !lua_isboolean( L, nres ) )
					Error( L, "wrong result type" );
				*va_arg( vl, bool * ) = b;
				break;
			}
			default:
				Error( L, "invalid option (%c)", *( sig - 1 ) );
		}
		nres++;
	}

	va_end( vl );
}

//===================================
// StackDump
//===================================
static void StackDump( lua_State *L ) {
	int i;
	int top = lua_gettop( L );  // depth of the stack
	for( i = 1; i <= top; i++ ) {  // repeat for each level
		int t = lua_type( L, i );
		switch( t ) {
			case LUA_TSTRING: {
				printf( "'%s'", lua_tostring( L, i ) );
				break;
			}
			case LUA_TBOOLEAN: {
				printf( lua_toboolean( L, i ) ? "true" : "false" );
				break;
			}
			case LUA_TNUMBER: {
				printf( "%g", lua_tonumber( L, i ) );
				break;
			}
			default: {
				printf( "%s", lua_typename( L, t ) );
				break;
			}

		}
		printf( "  " );
	}
	printf( "\n" );  // end the listing
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
	// parse args
	bool args[2];
	for( int i = 1; i <= 2; i++ ) {
		if ( !strcmp( argv[i], "true" ) ) {
			args[i-1] = true;
			// printf( "true\n" );
		} else if ( !strcmp( argv[i], "false" ) ) {
			args[i-1] = false;
			// printf( "false\n" );
		} else {
			printf( "wrong argument: %s\n", argv[ i ] );
			exit( EXIT_FAILURE );
		}
	}

	lua_State *L = luaL_newstate();
	luaL_openlibs( L );

	// load and prime lua file
	if ( luaL_loadfile( L, "call_lua_func_generic_bool.lua" ) || lua_pcall( L, 0, 0, 0) )
		Error( L, "Cannot run script. File: %s\n", lua_tostring( L, -1 ) );

	// call lua function 'f'
	bool ans;
	Call_va( L, "f", "bb>b", args[0], args[1], &ans );
	printf( "answer = %d\n", ans );
	// lua_pop( L, 1 );			// pop the return from the stack
	// StackDump( L );				// check what's left on the stack
	lua_close( L );
	return 0;
}
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#define MAX_COLOR	255

struct colorTable_t {
	char *name;
	unsigned char red, green, blue;
} colorTable[] = {
	{ "WHITE", 	MAX_COLOR, MAX_COLOR, MAX_COLOR },
	{ "RED", 	MAX_COLOR,         0,         0 },
	{ "GREEN", 	        0, MAX_COLOR,         0 },
	{ "BLUE", 	        0,         0, MAX_COLOR },
	{ NULL, 	        0,         0,         0 }  // sentinel
};

struct color_t {
	int red, green, blue;
} bgColor;

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
// GetColorField
// 
//   Assume that table is on the stack top
//===================================

int GetColorField( lua_State *L, const char *key ) {
	// lua_pushstring( L, key );  // push key
	// lua_gettable( L, -2 );  // get background[key]
	lua_getfield( L, -1, key );
	if ( !lua_isnumber( L, -1 ) )
		Error( L, "invalid component in background color\n" );
	int result = ( int )( lua_tonumber( L, -1 ) * MAX_COLOR );
	lua_pop( L, 1 );  // remove number
	return result;
}

//===================================
// SetColorField
// 
//   Assume that table is at the top
//===================================
void SetColorField( lua_State *L, const char *index, int value ) {
	// lua_pushstring( L, index );  // key
	// lua_pushnumber( L, ( double )value / MAX_COLOR );  // value
	// lua_settable( L, -3 );
	lua_pushnumber( L, ( double )value / MAX_COLOR );  // value
	lua_setfield( L, -2, index );
}

//===================================
// SetColor
//===================================
void SetColor( lua_State *L, struct colorTable_t *ct ) {
	lua_newtable( L );						// creates a table
	SetColorField( L, "r", ct->red );		// tble.r = ct->r
	SetColorField( L, "g", ct->green );		// tble.r = ct->r
	SetColorField( L, "b", ct->blue );		// tble.r = ct->r
	lua_setglobal( L, ct->name );			// 'name' = table
}

//===================================
// Load
//===================================
void Load( lua_State *L, const char *fname, int *w, int *h, struct color_t *c ) {
	// register colors for the configuration script
	int i = 0;
	while ( colorTable[i].name != NULL )
		SetColor( L, &colorTable[i++] );
	// load configuration script
	if ( luaL_loadfile( L, fname ) || lua_pcall( L, 0, 0, 0 ) )
		Error( L, "cannot run config. file: %s\n", lua_tostring( L, -1 ) );
	// load dimensions
	lua_getglobal( L, "width" );
	lua_getglobal( L, "height" );
	if ( !lua_isnumber( L, -2) )
		Error( L, "'width' should be a number\n" );
	if ( !lua_isnumber( L, -1) )
		Error( L, "'height' should be a number\n" );
	*w = lua_tointeger( L, -2 );
	*h = lua_tointeger( L, -1 );
	// load colors
	lua_getglobal( L, "background" );
	if ( lua_isstring( L, -1 ) ) {  // value is a string?
		const char *colorName = lua_tostring( L, -1 );  // get string
		int i;  // search the color table
		for ( i = 0; colorTable[i].name != NULL; i++ ) {
			if ( strcmp ( colorName, colorTable[i].name ) == 0 )
				break;
		}
		if ( colorTable[i].name == NULL )  // is string not found?
			Error( L, "invalid color name (%s)\n", colorName );
		else {  // use colorTable[i]
			c->red = colorTable[i].red;
			c->green = colorTable[i].green;
			c->blue = colorTable[i].blue;
		}
	} else if ( lua_istable( L, -1 ) ) {
		c->red = GetColorField( L, "r" );
		c->green = GetColorField( L, "g" );
		c->blue = GetColorField( L, "b" );
	} else
		Error( L, "invalid value for 'background'\n" );
}

//===================================
// main
//===================================
int main( void ) {
	int *width = malloc( sizeof( int ) );
	int *height = malloc( sizeof( int ) );
	lua_State *L = luaL_newstate();
	luaL_openlibs( L );
	Load( L, "config.lua", width, height, &bgColor );
	printf( "width = %d, height = %d\n", *width, *height );
	printf( "background color: red = %d, green = %d, blue = %d\n",
		bgColor.red, bgColor.green, bgColor.blue );
	lua_close( L );
	return 0;
}
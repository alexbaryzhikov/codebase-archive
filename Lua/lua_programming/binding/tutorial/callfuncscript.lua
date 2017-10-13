function tellme()
	io.write( "Lua: This is coming from tellme()\n" )
end

function square( n )
	io.write( "Lua: Within callfuncscript.lua square(), arg = " )
	io.write( tostring( n ) )
	n = n * n
	io.write( ", square = " )
	io.write( tostring( n ).."\n" )
	return( n )
end

print( "Lua: Priming run" )
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

function tweaktable( tab_in )
	local tab_out = { numfields = 1 }
	for k, v in pairs( tab_in ) do
		tab_out.numfields = tab_out.numfields + 1
		tab_out[ tostring( k ) ] = string.upper( tostring( v ) )
	end
	tab_out.numfields = tostring( tab_out.numfields )
	io.write( "Lua: At the bottom of tweaktable(), numfields = " )
	io.write( tab_out.numfields.."\n" )
	return tab_out
end

print( "Lua: Priming run" )
co = coroutine.wrap(
	function( a )
		return pcall(
			function ( x )
				coroutine.yield( x[1] )
				return x[3]
			end,
			a )
	end )
print( co( { 10, 3, -8, 15 } ) )
print( co() )

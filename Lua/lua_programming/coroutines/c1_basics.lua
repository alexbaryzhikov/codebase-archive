-- ======================================================

-- Coroutine Basics

-- ======================================================

-- ===========================
-- Create, Resume

--   Coroutine status can be: suspended, running, dead and normal
-- ===========================
--[[
co = coroutine.create(
	function()
		print( "hi" )
	end
)
print( co )
print( coroutine.status( co ) )
coroutine.resume( co )
print( coroutine.status( co ) )
--]]

-- ===========================
-- Yield
-- ===========================
--[[
co = coroutine.create(
	function()
		for i = 1,10 do
			print( "co", i )
			coroutine.yield()
		end
	end )
coroutine.resume( co )
print( coroutine.status( co ) )
coroutine.resume( co )
coroutine.resume( co )
coroutine.resume( co )
coroutine.resume( co )
coroutine.resume( co )
coroutine.resume( co )
coroutine.resume( co )
coroutine.resume( co )
coroutine.resume( co )
coroutine.resume( co )
print( coroutine.status( co ) )
print( coroutine.resume( co ) )
--]]

-- ===========================
-- Resume <-> Yield  data exchange
-- ===========================

-- the first resume(), which has no corresponding yield, passes arguments to the co main
--[[
co = coroutine.create(
	function( a, b, c )
		print( "co", a, b, c + 2 )
	end )
coroutine.resume( co, 1, 2, 3 )  --> co  1  2  5
--]]

-- a call to resume() returns arguments passed to the corresponding yield
--[[
co = coroutine.create(
	function( a, b )
		coroutine.yield( a + b, a - b )  -- passing two new arguments
	end )
print( coroutine.resume( co, 20, 10 ) )  --> true  30  10
--]]

-- yield() returns arguments passed to the corresponidng resume
--[[
co = coroutine.create(
	function( x )
		print( "co1", x )
		print( "co2", coroutine.yield() )  	
	end )
coroutine.resume( co, "hi" )				--> co1  hi
coroutine.resume( co, 4, 5 )				--> co2  4  5
--]]

-- when a coroutine ends, any values returned by its main go to corresponding resume()
---[[
co = coroutine.create(
	function()
		return 6, 7
	end )
print( coroutine.resume( co ) )
--]]
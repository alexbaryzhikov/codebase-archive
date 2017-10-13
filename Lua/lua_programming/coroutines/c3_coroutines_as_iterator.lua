-- ======================================================

-- Coroutines as Iterators

-- ======================================================

-- ===========================
-- permgen - generator

--   A function to generate all permutations
--   of the first n elements of a
-- ===========================
--[[
function printResult( l )
	for i = 1, #l do
		io.write( l[i], " " )
	end
	io.write( "\n")
end

function permgen( a, n )
	n = n or #a 				-- default for n is size of a
	if n <= 1 then 				-- nothing to change?
		printResult( a )
	else
		for i = 1, n do
			-- put i-th element as the last one
			a[n], a[i] = a[i], a[n]
			-- generate all permutations of the other elements
			permgen( a, n - 1 )
			-- restore i-th element
			a[n], a[i] = a[i], a[n]
		end
	end
end

permgen { "a", "b", "c" }
--]]

-- ===========================
-- permgen - iterator

--   A function to iterate through all permutations of a
-- ===========================
-- We want to convert generator to iterator, so we can call it
-- to produce one value at a time and not worry about keeping
-- its state
---[[
function printResult( l )
	for i = 1, #l do
		io.write( l[i], " " )
	end
	io.write( "\n" )
end

function permgen( a, n )
	n = n or #a 				-- default for n is size of a
	if n <= 1 then 				-- nothing to change?
		-- change printResult to yield
		coroutine.yield( a )
	else
		for i = 1, n do
			-- put i-th element as the last one
			a[n], a[i] = a[i], a[n]
			-- generate all permutations of the other elements
			permgen( a, n - 1 )
			-- restore i-th element
			a[n], a[i] = a[i], a[n]
		end
	end
end

-- Variant 1
-- define a factory to run generator inside a coroutine
---[=[
function permutations( a )
	local co = coroutine.create( function() permgen( a ) end )
	return
		function()		-- iterator
			local code, res = coroutine.resume( co )
			return res
		end
end
--]=]

-- Variant 2
-- create iterator using coroutine.wrap() - easier, but less flexible
--[=[
function permutations( a )
	return coroutine.wrap(function() permgen( a ) end)
end
--]=]

-- iterate over all permutations
for p in permutations { "a", "b", "c" } do
	printResult( p )
end
--]]
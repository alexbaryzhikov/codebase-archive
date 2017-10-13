--[[
The eight-queen puzzle:
  the goal is to put eight queens in a chessboard in such a way 
  that no queen can attack another one.
--]]

local N = 8			-- board size

-- check whether position ( n, c ) is free from attacks
local function IsPlaceOK( a, n, c )
	for i = 1, n - 1 do 				-- for each queen already placed
		if ( a[i] == c ) or 			-- same column?
		   ( a[i] - i == c - n ) or 	-- same diagonal?
		   ( a[i] + i == c + n ) then 	-- same diagonal?
			return false 				-- place can be attacked
		end
	end
	return true 						-- no attacks; place is OK
end

-- print a board (coroutine)
local function PrintSolution_cr( a )
	local num = 0
	while true do
		num = num + 1
		print( "# " .. num )
		for i = 1, N do
			for j = 1, N do
				io.write( a[i] == j and "X" or "-", " " )
			end
			io.write( "\n" )
		end
		io.write( "\n" )
		a = coroutine.yield()
	end
end

-- PrintSolution = coroutine.wrap( PrintSolution_cr )
---[[
function wrapPrintSol()
	local co = coroutine.create( PrintSolution_cr )
	return function( a ) coroutine.resume( co, a ) end
end

PrintSolution = wrapPrintSol()
--]]

-- add to board 'a' all queens from 'n' to 'N'
local function AddQueen( a, n )
	if n > N then 						-- all queens have been placed?
		PrintSolution( a )
	else 								-- try to place n-th queen
		for c = 1, N do
			if IsPlaceOK( a, n, c ) then
				a[n] = c				-- place n-th queen at column 'c'
				AddQueen( a, n + 1 )
			end
		end
	end
end

-- run the program
AddQueen( {}, 1 )
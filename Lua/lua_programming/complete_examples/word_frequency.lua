--[[
arg[1]: minimum word length
arg[2]: number of results

returns: a program reads a text and prints the most frequent words
--]]

-- iterator that returns one word at a time
local function allwords()
	local auxwords = function()
		for line in io.lines() do
			for word in string.gmatch( line, "%w+" ) do
				if #word > ( tonumber( arg[1] ) or 3 ) then
					coroutine.yield( word )
				end
			end
		end
	end
	return coroutine.wrap( auxwords )
end

-- create word / number datastructure
local counter = {}
for w in allwords() do
	counter[w] = ( counter[w] or 0 ) + 1
end

-- create list of words
local words = {}
for w in pairs( counter ) do
	words[#words + 1] = w
end

-- sort list of words
table.sort( words,
	function( w1, w2 )
		return counter[w1] > counter[w2] or counter[w1] == counter[w2] and w1 < w2
	end )

-- output results
for i = 1, ( tonumber( arg[2] ) or 10 ) do
	print( words[i], counter[words[i]] )
end

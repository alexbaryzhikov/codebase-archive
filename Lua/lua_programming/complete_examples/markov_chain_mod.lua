-- Markov chain algorithm
-- N can be any number

local N = 3
local MAXGEN = 300
local NOWORD = "\n"

function allwords()
	local line = io.read()			-- current line
	local pos = 1					-- current position in the line
	return function()				-- iterator function
		while line do				-- repeat while there are lines
			local s, e = string.find( line, "%w+", pos )
			if s then				-- found a word?
				pos = e + 1			-- update next position
				return string.sub( line, s, e )	-- return the word
			else
				line = io.read()	-- word not found; try another line
				pos = 1				-- restart from the first position
			end
		end
		return nil					-- no more lines: end of traversal
	end
end

function prefix( t )
	local res = nil
	for i, w in ipairs( t ) do
		if not res then res = w
		else res = res .. " " .. w end
	end
	return res
end

local statetab = {}

function insert( index, value )
	local list = statetab[index]
	if list == nil then
		statetab[index] = { value }
	else
		list[#list + 1] = value
	end
end

function initPrefix()
	local res = { NOWORD, NOWORD }
	if N <= 2 then return res end
	for i = 3, N do table.insert( res, NOWORD ) end
	return res
end

local words = initPrefix()

function updatePrefix( t, w )
	table.remove( t, 1 )
	table.insert( t, w )
end

-- build table
for w in allwords() do
	insert( prefix( words ), w )
	updatePrefix( words, w )
end
insert( prefix( words ), NOWORD )

-- generate text
local words = initPrefix()			-- reinitialize
for i = 1, MAXGEN do
	local list = statetab[prefix( words )]
	-- choose a random item form list
	local r = math.random( #list )
	local nextword = list[r]
	if nextword == NOWORD then return end
	io.write( nextword, " " )
	updatePrefix( words, nextword )
end

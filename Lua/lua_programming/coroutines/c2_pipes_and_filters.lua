-- ======================================================

-- Pipes and Filters

-- ======================================================

--[[
Producer / consumer problem:

function producer()
	while true do
		local x = io.read() -- produce new value 
		send( x ) -- send it to consumer 
	end 
end 

function consumer () 
	while true do 
		local x = receive() -- receive value from producer 
		io.write( x, "\n" ) -- consume it 
	end 
end

How do we match send with recieve? Who has the main loop?
Both must be active and treat other as a slave, thus coroutines.
--]]


-- ===========================
-- Consumer-driven design

--   The program stargs by calling the consumer, so producer is a coroutine
-- ===========================

function receive( prod )
	local status, value = coroutine.resume( prod )
	return value
end

function send( x )
	coroutine.yield( x )
end

function producer()
	return coroutine.create(
		function()
			while true do
				local x = io.read()  -- produce new value
				send( x )
			end
		end )
end

-- filter inserts a line number at the beginning of each line
function filter( prod )
	return coroutine.create(
		function()
			local line = 1
			while true do
				local x = receive( prod )	-- get new value
				x = string.format( "%5d %s", line, x )
				send( x )					-- send it to consumer
				line = line + 1
			end
		end )
end

function consumer( prod )
	while true do
		local x = receive( prod )	-- get new value
		io.write( x, "\n" )			-- consume new value
	end
end

-- start the consumer
consumer( filter ( producer() ) )
-- ======================================================

-- Non-preemptive multithreading

-- ======================================================

socket = require "socket"

-- ===========================
-- Downloading one file
-- ===========================
--[[
host = "www.w3.org"
file = "/TR/REC-html32.html"
-- create a connection object
c = assert( socket.connect( host, 80 ) )
-- send the file request
c:send( "GET " .. file .. " HTTP/1.0\r\n\r\n" )
-- read the file in blocks of 1Kb, writing each to stdout
while true do
	local s, status, partial = c:receive( 2^10 )
	io.write( s or partial )
	if status == "closed" then break end
end
-- close the connection
c:close()
--]]

-- ===========================
-- Download multiple files
-- ===========================
---[[
function Download( host, file )
	local c = assert( socket.connect( host, 80 ) )
	local count = 0					-- counts number of bytes to read
	c:send( "GET " .. file .. " HTTP/1.0\r\n\r\n" )
	while true do
		local s, status = Receive( c )
		count = count + #s
		if status == "closed" then break end
	end
	c:close()
	print( file, count )
end

function Receive( connection )
	connection:settimeout( 0 )		-- do not block
	local s, status, partial = connection:receive( 2^10 )
	if status == "timeout" then
		coroutine.yield( connection )
	end
	return s or partial, status
end

threads = {}						-- list of all live threads

function Get( host, file )
	-- create coroutine
	local co = coroutine.create( function() Download( host, file ) end )
	-- insert in the list
	table.insert( threads, co )
end

-- "busy wait" dispatcher
--[=[
function Dispatch()
	local i = 1
	while true do
		if threads[i] == nil then	-- no more threads?
			if threads[1] == nil then break end  -- list is empty?
			i = 1
		end
		local status, res = coroutine.resume( threads[i] )
		if not res then				-- thread finished its task?
			table.remove( threads, i )
		else
			i = i + 1				-- go to next thread
		end
	end
end
--]=]

-- dispatcher using "select"
---[=[
function Dispatch()
	local i = 1
	local timedout = {}
	while true do
		if threads[i] == nil then	-- no more threads?
			if threads[1] == nil then break end
			i = 1					-- restart the loop
			timedout = {}
		end
		local status, res = coroutine.resume( threads[i] )
		if not res then				-- thread finished its task?
			table.remove( threads, i )
		else						-- time out
			i = i + 1
			timedout[#timedout + 1] = res
			if #timedout == #threads then  -- all threads blocked?
				socket.select( timedout )
			end
		end
	end
end
--]=]

host = "www.w3.org"
Get( host, "/TR/html401/html40.txt" )
Get( host, "/TR/2002/REC-xhtml1-20020801/xhtml1.pdf" )
Get( host, "/TR/REC-html32.html" )
Get( host, "/TR/2000/REC-DOM-Level-2-Core-20001113/DOM2-Core.txt" )

Dispatch()							-- main loop
--]]
--[[
Exercise 9.3: Implement a transfer function in Lua. If you think
about resume-yield as similar to call-return, a transfer would
be like a goto: it suspends the running coroutine and resumes
any other coroutine, given as argument. (Hint: use a kind of 
dispatch to control your coroutines. Then, a transfer would yield
to the dispatch signaling the next coroutine to run, and the
dispatch would resume that next coroutine.)
--]]

coroutines = {}

function Transfer( addr )
	coroutine.yield( addr )
end

function Register( func )
	local co = coroutine.create( func )
	table.insert( coroutines, co )
end

function Dispatch()
	if not coroutines[1] then 
		print( "No coroutines found." )
		os.exit()
	end
	local status, addrOld, addr = nil, nil, 1
	while true do
		if coroutines[addr] == nil then
			print( "Wrong address: ", addr)
			os.exit()
		end
		addrOld = addr
		status, addr = coroutine.resume( coroutines[addr] )
		if not addr then
			print( "Coroutine returned with no transfer.")
			break
		end
		if not status then
			print( "Transfer error, addr: ", addrOld )
			os.exit()
		end
	end
end

function Foo1()
	print( "Hello from Foo1" )
	Transfer( 2 )
	print( "Foo1 returns")
end
Register( Foo1 )

function Foo2()
	print( "Foo2 is here")
	Transfer( 3 )
end
Register( Foo2 )

function Foo3()
	print( "I'm Foo3" )
	Transfer( 1 )
end
Register( Foo3 )

Dispatch()
print( "The sequence is complete!")
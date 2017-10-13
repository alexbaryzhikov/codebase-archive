function love.load()
	screen_width, screen_height = love.graphics.getDimensions()
	foo = love.filesystem.newFile( "foo.sav" )
	exists = love.filesystem.exists( "foo.sav" )
	foo:open("w")
	foo:write( 10 .. "\n" .. 2 )
	foo:close()
	foo:open("r")
	data = {}
	i = foo:lines()
	data[1] = i()
	data[2] = i()
	foo:close()
end

function love.draw()
	love.graphics.print(tostring( exists ), 5, 5)
	love.graphics.print(data[1] .. "\n" .. data[2] or "N/A", 5, 25)
end

function love.keypressed(key, scancode, isrepeat)
	if key == "escape" then love.event.quit() end
end

--[[
===========================================================================

Template

===========================================================================
]]

function love.load()
	-- get camera dimensions
	cam = {}
	cam.width, cam.height = love.graphics.getDimensions()
	-- load modules
	world = require( "modules/world" )
	world.init()
end

function love.update( dt )
	if gameIsPaused then
		return
	end
	world.update( dt )
end

function love.draw()
	world.draw()
end

--[[
========================
Player Input
========================
]]

function love.keypressed( key, scancode, isrepeat )
	if key == "escape" then love.event.quit() end
	world.keypressed( key, scancode, isrepeat )
end

function love.mousepressed( x, y, button, istouch )
	world.mousepressed( x, y, button, istouch )
end

function love.mousereleased( x, y, button, istouch )
	world.mousereleased( x, y, button, istouch )
end

function love.wheelmoved( x, y )
	world.wheelmoved( x, y )
end

--[[
========================
Pause and Quit
========================
]]

function love.focus( f )
	gameIsPaused = not f
end

function love.quit()
end

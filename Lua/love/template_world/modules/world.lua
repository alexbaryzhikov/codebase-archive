--[[
===========================================================================

World

===========================================================================
]]

M = {}

function M.init()
end

function M.update( dt )
end

function M.draw()
	love.graphics.print( "screen x: "..cam.width , 10, 10 )
	love.graphics.print( "screen y: "..cam.height , 10, 25 )
end

--[[
========================
Player Input
========================
]]

function M.keypressed( key, scancode, isrepeat )
	if key == "escape" then love.event.quit() end
end

function M.mousepressed( x, y, button, istouch )
end

function M.mousereleased( x, y, button, istouch )
end

function M.wheelmoved( x, y )
end

return M


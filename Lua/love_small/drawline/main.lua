function love.load()
	screen_width, screen_height = love.graphics.getDimensions()
end

function genline( x0, y0, x1, y1 )
--[[
	The implicit equation of a line is y * dx - x * dy = 0.

	So any point off the line will produce an error:
		error = y * dx - x * dy
	It's negative if point is below the line and positive otherwise.

	The error of the diagonal, horizontal and vertical steps:
		error_xy = ( y + 1 ) * dx - ( x + 1 ) * dy = error + dx - dy
		error_x  =         y * dx - ( x + 1 ) * dy = error      - dy = error_xy - dx
		error_y  = ( y + 1 ) * dx -         x * dy = error + dx      = error_xy + dy

	To decide which step to take we can pick one with the least absolute error,
	or
	1) take the error_xy + error_x, if it's negative then increase y
	2) take the error_xy + error_y, if it's positive then increase x
--]]

	o = {}
	x0, y0 = math.floor( x0 + 0.5 ), math.floor( y0 + 0.5 )
	x1, y1 = math.floor( x1 + 0.5 ), math.floor( y1 + 0.5 )

	dx = math.abs( x1 - x0 )
	dy = - math.abs( y1 - y0 )		-- use neg in advance, it is never added anyway
	sx = x0 < x1 and 1 or -1		-- step x
	sy = y0 < y1 and 1 or -1		-- step y
	err = dx + dy					-- error_xy one step ahead

	while true do
		table.insert( o, x0 ); table.insert( o, y0 )
		err2 = err * 2
		if err2 >= dy then					-- error_y decides if we can increase x
			if x0 == x1 then break end
			err = err + dy; x0 = x0 + sx	-- x + 1 increase error by -dy
		end
		if err2 <= dx then					-- error_x decides if we can increase y
			if y0 == y1 then break end
			err = err + dx; y0 = y0 + sy	-- y + 1 increase error by dx
		end
	end

	return o
end

function love.draw()
	for i = 0, 2 * math.pi, math.pi / 12 do
		x0 = screen_width / 2
		y0 = screen_height / 2
		x1 = 500 * math.cos( i ) + screen_width / 2
		y1 = 500 * math.sin( i ) + screen_height / 2
		love.graphics.points( genline( x0, y0, x1, y1 ) )
	end
end

function love.keypressed( key, scancode, isrepeat )
	if key == "escape" then love.event.quit() end
end

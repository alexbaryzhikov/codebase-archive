--[[
===========================================================================

Mandelbrot

Graphs the Mandelbrot set.

===========================================================================
]]


--[[
========================
love.load
========================
]]

function love.load()
	screen_width, screen_height = love.graphics.getDimensions()
	ITER_MAX = 64
	WIDTH = 4
	OFFSET_X = -0.79291632608273
	OFFSET_Y = -0.162067956062
	-- canvas = love.graphics.newCanvas( 7680, 4320 )
	-- canvas = love.graphics.newCanvas( 320, 320 )
	canvas = love.graphics.newCanvas( 1920, 1080 )
	canvas_width, canvas_height = canvas:getDimensions()
	offset_x = OFFSET_X / WIDTH * canvas_width
	offset_y = - OFFSET_Y / WIDTH * canvas_width
	ComputingMessage()
end

--[[
========================
love.draw
========================
]]
function love.draw()
	love.graphics.setColor( 255, 255, 255 )
	love.graphics.draw( canvas, 0, 0, 0, screen_width / canvas_width, screen_height / canvas_height )
	-- DrawOrigin()
	if doUpdate then
		-- render frames
		-- for k = 1, 600 do
		-- 	UpdateScreen()
		-- 	local img = canvas:newImageData()
		-- 	img:encode( "png", string.format("mandelbrot%05d.png", k))
		-- 	WIDTH = WIDTH / 1.03766
		-- 	ITER_MAX = ITER_MAX + 0.103766
		-- end
		UpdateScreen()
		doUpdate = false
		isUpdateDone = true
	end
	if not isUpdateDone then doUpdate = true end
end

--[[
========================
love.update
========================
]]
function love.update( dt )
end

--[[
========================
love.keypressed
========================
]]
function love.keypressed( key, scancode, isrepeat )
	if key == "escape" then
		local img = canvas:newImageData()
		img:encode( "png", "mandelbrot.png")
		print( "Offset X: " .. OFFSET_X )
		print( "Offset Y: " .. OFFSET_Y )
		love.event.quit()
	end
	if key == "left" then
		ComputingMessage()
		OFFSET_X = OFFSET_X - WIDTH / 5
		isUpdateDone = false
	end
	if key == "right" then
		ComputingMessage()
		OFFSET_X = OFFSET_X + WIDTH / 5
		isUpdateDone = false
	end
	if key == "up" then
		ComputingMessage()
		OFFSET_Y = OFFSET_Y - WIDTH / 5
		isUpdateDone = false
	end
	if key == "down" then
		ComputingMessage()
		OFFSET_Y = OFFSET_Y + WIDTH / 5
		isUpdateDone = false
	end
	if key == "pageup" then
		ComputingMessage()
		WIDTH = WIDTH / 2
		isUpdateDone = false
	end
	if key == "pagedown" then
		ComputingMessage()
		if WIDTH * 2 <= 2 then
			WIDTH = WIDTH * 2
			isUpdateDone = false
		end
	end
	if key == "home" then
		ComputingMessage()
		WIDTH = 4
		OFFSET_X = 0
		OFFSET_Y = 0
		isUpdateDone = false
	end
end

--[[
========================
UpdateScreen
========================
]]
function UpdateScreen()
	delta = WIDTH / canvas_width
	offset_x = OFFSET_X / WIDTH * canvas_width
	offset_y = - OFFSET_Y / WIDTH * canvas_width
	local nextZ = function ( z, c )
		return AddComplexOrt( SqrComplexOrt( z ), c )
	end
	love.graphics.setCanvas( canvas )
	love.graphics.setColor( 255, 255, 255 )
	for i = - canvas_height / 2 + offset_y, canvas_height / 2 + offset_y do
		for j = - canvas_width / 2 + offset_x, canvas_width / 2 + offset_x do
			local c = GetComplexOrt( j * delta, i * delta )
			local z = nextZ( GetComplexOrt( 0, 0 ), c )
			local zOld = z
			local n = 0
			while n < ITER_MAX and OrtToRad( z ).r <= 2 do
				n = n + 1
				z = nextZ( z, c )
			end				
			-- set the color
			local c = {}
			if n >= ITER_MAX then
				c.r = 255
				c.g = 244
				c.b = 54
			else
				c.r = 255 * n / ITER_MAX
				c.g = 127 * ( 1 + n / ITER_MAX )
				c.b = 200
			end
			-- plot the point
			DrawPoint( { j - 0.5 - offset_x, i - 0.5 - offset_y, c.r, c.g, c.b } )
		end
	end
	love.graphics.setCanvas()
end

--[[
========================
DrawOrigin
========================
]]
function DrawOrigin()
	love.graphics.setColor( 200, 0, 0 )
	DrawLine( 0 - offset_x, 0 - offset_y, 15 - offset_x, 0 - offset_y )
	love.graphics.setColor( 0, 200, 0 )
	DrawLine( 0 - offset_x, 0 - offset_y, 0 - offset_x, 15 - offset_y )
end

--[[
========================
DrawLine
========================
]]
function DrawLine( x1, y1, x2, y2 )
	love.graphics.line( x1 + canvas_width / 2, canvas_height / 2 - y1, x2 + canvas_width / 2, canvas_height / 2 - y2 )
end

--[[
========================
DrawPoint
========================
]]
function DrawPoint( point )
	love.graphics.points( { { point[1] + canvas_width / 2, canvas_height / 2 - point[2], point[3], point[4], point[5] } } )
end

--[[
========================
ComputingMessage
========================
]]
function ComputingMessage()
	love.graphics.setCanvas( canvas )
	love.graphics.setColor( 0, 0, 0 )
	love.graphics.rectangle( "fill", 11, 11, 25, 25)
	love.graphics.setColor( 255, 255, 255 )
	love.graphics.rectangle( "fill", 10, 10, 25, 25)
	love.graphics.setCanvas()
end

--[[
================================================

Complex Math

================================================
]]

--[[
========================
GetComplexOrt
========================
]]
function GetComplexOrt( x, i )
	local o = { x = x, i = i, type = "ort" }
	return o
end

--[[
========================
GetComplexRad
========================
]]
function GetComplexRad( r, a )
	local o = { r = r, a = a, type = "rad" }
	return o
end

--[[
========================
OrtToRad
========================
]]
function OrtToRad( a )
	if a.type == "ort" then
		local o = { r = math.sqrt( a.x ^ 2 + a.i ^ 2 ), a = math.atan2( a.i, a.x ), type = "rad" }
		return o
	else
		return a
	end
end

--[[
========================
RadToOrt
========================
]]
function RadToOrt( a )
	if a.type == "rad" then
		local o = { x = a.r * math.cos( a.a ), i = a.r * math.sin( a.a ), type = "ort" }
		return o
	else
		return a
	end
end

--[[
========================
AddComplexOrt
========================
]]
function AddComplexOrt( a, b )
	return { x = a.x + b.x, i = a.i + b.i, type = "ort" }
end

--[[
========================
MultComplexRad
========================
]]
function MultComplexRad( a, b )
	return { r = a.r * b.r, a = a.a + b.a, type = "rad" }
end

--[[
========================
MultComplexOrt
========================
]]
function MultComplexOrt( a, b )
	return { x = a.x * b.x - a.i * b.i, i = a.x * b.i + a.i * b.x, type = "ort" }
end

--[[
========================
SqrComplexRad
========================
]]
function SqrComplexRad( a )
	return { r = a.r ^ 2, a = 2 * a.a, type = "rad" }
end

--[[
========================
SqrComplexOrt
========================
]]
function SqrComplexOrt( a )
	local o = { x = a.x ^ 2 - a.i ^ 2, i = 2 * a.x * a.i, type = "ort" }
	return o
end

--[[
================================================
]]


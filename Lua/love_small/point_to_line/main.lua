function love.load()
	screen_width, screen_height = love.graphics.getDimensions()
	math.randomseed(math.floor(os.clock() * 100000))
	A = {x = -100, y = -50}  -- rocket
	B = {x = 300, y = 200}  -- mouse
	beamPoint = nil
	beamObject = nil
	objects = {}
	getObjects()
end

function love.update(dt)
	if love.mouse.isDown(1) then A.x, A.y = toCamCoords(love.mouse.getPosition()) end
	if love.mouse.isDown(2) then B.x, B.y = toCamCoords(love.mouse.getPosition()) end
	beamPoint, beamObject = getBeamPoint()
end

function love.draw()
	love.graphics.origin()
	love.graphics.translate(screen_width / 2, screen_height / 2)
	-- objects
	for i = 1, #objects do
		love.graphics.setColor(60, 60, 60, 127)
		love.graphics.circle("fill", objects[i].x, objects[i].y, objects[i].r, 64)
	end
	for i = 1, #objects do
		love.graphics.setColor(180, 180, 180)
		love.graphics.circle("fill", objects[i].x, objects[i].y, 5, 16)
	end
	-- point
	if beamPoint then
		love.graphics.setColor(0, 180, 0)
		love.graphics.circle("fill", beamPoint.x, beamPoint.y, 5, 16)
		love.graphics.line(beamObject.x, beamObject.y, beamPoint.x, beamPoint.y)
	end
	-- rocket
	love.graphics.setColor(255, 0, 0)
	love.graphics.circle("fill", A.x, A.y, 5, 16)
	-- mouse
	love.graphics.setColor(255, 255, 255)
	love.graphics.circle("fill", B.x, B.y, 5, 16)
	-- beam
	love.graphics.line(A.x, A.y, B.x, B.y)
	-- 0, 0
	love.graphics.setColor(128, 128, 128)
	love.graphics.line(-10, 0, 10, 0)
	love.graphics.line(0, -10, 0, 10)
	-- stats
	local x, y = toCamCoords(love.mouse.getPosition())
	love.graphics.print(x..", "..y, toCamCoords(10, 10))
end

function love.keypressed(key, scancode, isrepeat)
	if key == "escape" then love.event.quit() end
	if key == "e" then getObjects() end
end

-- methods
function getBeamPoint()
	local x, y = 0, 0
	local beamAng = math.atan2(B.y - A.y, B.x - A.x)
	local rotA = rotatePoint(beamAng, A)
	local rotB = rotatePoint(beamAng, B)
	local bPnt, bObj = nil, nil
	for i = 1, #objects do
		local rotObj = rotatePoint(beamAng, objects[i])
		local h = rotObj.y - rotA.y
		if h <= objects[i].r then
			local dx = math.sqrt(objects[i].r^2 - h^2)
			x, y = rotObj.x - dx, rotA.y
			if (x >= rotA.x) and (x <= rotB.x) then
				if bPnt then
					if x < bPnt.x then
						bPnt = {x = x, y = y}
						bObj = objects[i]
					end
				else
					bPnt = {x = x, y = y}
					bObj = objects[i]
				end
			end
		end
	end
	if bPnt then return rotatePoint(-beamAng, bPnt), bObj end
end

function rotatePoint(ang, point)
	local r = math.sqrt(point.x^2 + point.y^2)
	local a = math.atan2(point.y, point.x) - ang
	return {x = r * math.cos(a), y = r * math.sin(a)}
end

function getObjects()
	local x, y, r = 0, 0, 0
	objects = {}
	for i = 1, 10 do
		x = (math.random() - 0.5) * screen_width
		y = (math.random() - 0.5) * screen_height
		r = math.random(100) + 50
		local o = {x = x, y = y, r = r}
		table.insert(objects, o)
	end
end

function toCamCoords(x, y)
	x_ = x - screen_width / 2
	y_ = y - screen_height / 2
	return x_, y_
end
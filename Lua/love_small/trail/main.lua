Trail = require("trail")
TrailRake = require("trailRake")

function love.load()
	screen_width, screen_height = love.graphics.getDimensions()
	math.randomseed(math.floor(os.clock()*100000))
	isTargetReached = true
	OBJ_SPEED = 300
	REACH_THRESHOLD = 50
	comet = { x = 200, y = 200, angle = 0, speedX = 0, speedY = 0, speed = 0 }
	targetXY = { 0, 0 }
	dx, dy = 0, 0
	-- create trails
	objTrail1 = Trail.new(
		{
			host = comet,
			radius = 12,
			stepDelta = 1,
			lifetime = 0.25,
			color = { r = 231, g = 255, b = 170 },
		}
	)
	objTrail1:init()
	objTrail1Rake = TrailRake.new(
		{
			host = comet,
			radius = 12,
			stepDelta = 1,
			lifetime = 0.35,
			color = { r = 231, g = 255, b = 170 },
			rake = 3,
		}
	)
	objTrail1Rake:init()
	objTrail2 = Trail.new(
		{
			host = comet,
			radius = 10,
			stepDelta = 1,
			lifetime = 0.12,
			color = { r = 92, g = 65, b = 101 },
		}
	)
	objTrail2:init()
	objTrail2Rake = TrailRake.new(
		{
			host = comet,
			radius = 10,
			stepDelta = 1,
			lifetime = 0.22,
			color = { r = 92, g = 65, b = 101 },
			rake = 2,
		}
	)
	objTrail2Rake:init()
	objTrail3 = Trail.new(
		{
			host = comet,
			radius = 10,
			stepDelta = 1,
			lifetime = 0.05,
			color = { r = 255, g = 120, b = 133 },
		}
	)
	objTrail3:init()
end

function love.update(dt)
	-- update comet
	if isTargetReached then
		isTargetReached = false
		targetXY[1] = math.random()*screen_width
		targetXY[2] = math.random()*screen_height
	end
	if math.abs(targetXY[1]-comet.x) < REACH_THRESHOLD and math.abs(targetXY[2]-comet.y) < REACH_THRESHOLD then
		isTargetReached = true
	else
		dx = (targetXY[1]-comet.x)/OBJ_SPEED
		dy = (targetXY[2]-comet.y)/OBJ_SPEED
		comet.speedX = OBJ_SPEED*dx
		comet.speedY = OBJ_SPEED*dy
		comet.angle = math.atan2(dy, dx)
		comet.speed = math.sqrt(comet.speedX^2 + comet.speedY^2)
		comet.x = comet.x + comet.speedX*dt
		comet.y = comet.y + comet.speedY*dt
	end
	-- update trails
	objTrail1:update(dt)
	objTrail1Rake:update(dt)
	objTrail2:update(dt)
	objTrail2Rake:update(dt)
	objTrail3:update(dt)
end

function love.draw()
	-- draw trails
	objTrail1:draw()
	objTrail1Rake:draw()
	objTrail2:draw()
	objTrail2Rake:draw()
	objTrail3:draw()
    love.graphics.setColor(255, 255, 255)
    --love.graphics.circle("fill", comet.x, comet.y, 10, 12)

	love.graphics.print("comet X Y = "..math.floor(comet.x+0.5).." "..math.floor(comet.y+0.5), 10, 10)
	love.graphics.print("comet speed = "..math.floor(comet.speed+0.5), 10, 25)
end

function love.keypressed(key, scancode, isrepeat)
	if key == "escape" then love.event.quit() end
end

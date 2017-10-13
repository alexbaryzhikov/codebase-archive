require("star")

function love.load()
	-- observer
	OBSERVER_SPEED = 5
	OBSERVER_SPIN_SPEED = 5
	OBSERVER_FOV = 60
	FAR_CLIP = 15
	distance = 0
	-- starfield
	CLUSTER_DEPTH = 100
	STARS_NUMBER = 500
	starfield = {}
	isClusterAhead = false
	timer = 0
	-- graphics
	windowWidth = love.graphics.getWidth()
	windowHeight = love.graphics.getHeight()
	math.randomseed(math.floor(os.clock()*100000))
	-- generate first claster
	if OBSERVER_SPEED >= 0 then
		generateStars(STARS_NUMBER, 0, CLUSTER_DEPTH)
	else 
		generateStars(STARS_NUMBER, FAR_CLIP, FAR_CLIP-CLUSTER_DEPTH)
	end
end

function love.update(dt)
	distance = distance+OBSERVER_SPEED*dt
	timer = timer+dt
	if timer >= 1 then
		timer = 0
		local n = STARS_NUMBER*math.abs(OBSERVER_SPEED)/CLUSTER_DEPTH
		if OBSERVER_SPEED >= 0 then
			generateStars(n, CLUSTER_DEPTH-OBSERVER_SPEED, CLUSTER_DEPTH)
		else
			generateStars(n, FAR_CLIP-CLUSTER_DEPTH-OBSERVER_SPEED, FAR_CLIP-CLUSTER_DEPTH)
		end
	end
	if #starfield > 0 then
		for i = 1, #starfield do
			if starfield[i] then starfield[i]:update(dt) end
		end
	end
end

function love.draw()
	love.graphics.setColor(255, 255, 255)
	love.graphics.print("Distance traveled: "..math.floor(distance+0.5) , 10, 10)
	if #starfield > 0 then
		for i = 1, #starfield do
			starfield[i]:draw()
		end
	end
end

function love.quit()
end

function love.keypressed(key, scancode, isrepeat)
	if key == "escape" then love.event.quit() end
end

function generateStars(number, near, far)
	for i = 1, number do
		local size = 5 + math.random(10)
		local distance = near + math.random()*(far-near)
		local radius = 1 + math.random()*10
		local angle = math.rad(math.random()*360)
		local star = Star.new(
			{
				size = size,
				distance = distance,
				radius = radius,
				angle = angle,
			}
		)
		table.insert(starfield, star)
	end
end

function onStarClipped(event)
	for i = 1, #starfield do
		if starfield[i] == event.star then
			table.remove(starfield, i)
			break
		end
	end
end
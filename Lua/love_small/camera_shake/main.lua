--[[
hold mouse 1  --  shake more
hold mouse 2  --  shake less
]]--

utils = require("utils")
timekeeper = require("timekeeper")
local cameraShake = require("cameraShake")

function love.load()
	screen_width, screen_height = love.graphics.getDimensions()
	love.graphics.setBackgroundColor(59, 50, 81)
	math.randomseed(math.floor(os.clock()*100000))
	cameraShake.init()
end

function love.update(dt)
	if love.mouse.isDown(1) then
		cameraShake.magnitude = math.min(50, cameraShake.magnitude + 10 * dt)
	end
	if love.mouse.isDown(2) then
		cameraShake.magnitude = math.max(0, cameraShake.magnitude - 10 * dt)
	end
	timekeeper.update(dt)
	cameraShake.update(dt)
end

function love.draw()
	cameraShake.draw()
	love.graphics.setColor(173, 73, 119)
	love.graphics.circle("fill", screen_width/2, screen_height/2, 100, 100)
end

function love.keypressed(key, scancode, isrepeat)
	if key == "escape" then love.event.quit() end
end

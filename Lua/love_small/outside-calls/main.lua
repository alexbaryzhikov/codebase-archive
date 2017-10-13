local b = require "b"

function love.load()
	screen_width, screen_height = love.graphics.getDimensions()
	b.init()
end

function love.draw()
	b:draw()
end

function love.keypressed(key, scancode, isrepeat)
	if key == "escape" then love.event.quit() end
end

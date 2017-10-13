function love.load()
	screen_width, screen_height = love.graphics.getDimensions()

end

function love.draw()
	love.graphics.print("screen x: "..screen_width , 10, 10)
	love.graphics.print("screen y: "..screen_height , 10, 25)
end

function love.keypressed(key, scancode, isrepeat)
	if key == "escape" then love.event.quit() end
end

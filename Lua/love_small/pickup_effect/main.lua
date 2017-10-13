
function love.load()
	screen_width, screen_height = love.graphics.getDimensions()
	love.graphics.setBackgroundColor(59, 50, 81)
	math.randomseed(math.floor(os.clock()*100000))
	pickup = require("pickup")
end

function love.update(dt)
	if pickupEffect then pickupEffect:update(dt) end
end

function love.draw()
	if pickupEffect then pickupEffect:draw() end
end

function love.mousepressed(x, y, button, istouch)
	if button == 1 and not pickupEffect then
		pickupEffect = pickup.new(
			{
				x = x,
				y = y,
				radius = 10 + math.random(50),
			}
		)
		pickupEffect:init()
	end
end


function love.keypressed(key, scancode, isrepeat)
	if key == "escape" then love.event.quit() end
end

function onEffectFinished(event)
	pickupEffect = nil
end
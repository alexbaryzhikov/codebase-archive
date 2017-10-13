require("explosion")

function love.load()
	screen_width, screen_height = love.graphics.getDimensions()
	love.graphics.setBackgroundColor(59, 50, 81)
	math.randomseed(math.floor(os.clock()*100000))
end

function love.update(dt)
	if explosion then explosion:update(dt) end
end

function love.draw()
	if explosion then explosion:draw() end
end

function love.mousepressed(x, y, button, istouch)
	if button == 1 and not explosion then
		explosion = Explosion.new(
			{
				x = x,
				y = y,
				scale = 0.5+math.random(),
			}
		)
		explosion:init()
	end
end


function love.keypressed(key, scancode, isrepeat)
	if key == "escape" then love.event.quit() end
end

function onExplosionFinished(event)
	explosion = nil
end
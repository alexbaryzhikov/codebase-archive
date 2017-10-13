require("implosion")

function love.load()
	screen_width, screen_height = love.graphics.getDimensions()
	love.graphics.setBackgroundColor(59, 50, 81)
	math.randomseed(math.floor(os.clock()*100000))
end

function love.update(dt)
	if implosion then implosion:update(dt) end
end

function love.draw()
	if implosion then implosion:draw() end
	--love.graphics.print("screen x: "..screen_width , 10, 10)
	--love.graphics.print("screen y: "..screen_height , 10, 25)
end

function love.mousepressed(x, y, button, istouch)
	if button == 1 and not implosion then
		implosion = Implosion.new(
			{
				x = x,
				y = y,
				scale = 0.3+math.random()*0.7,
			}
		)
		implosion:init()
	end
end


function love.keypressed(key, scancode, isrepeat)
	if key == "escape" then love.event.quit() end
end

function onImplosionFinished(event)
	implosion = nil
end
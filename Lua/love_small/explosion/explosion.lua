Explosion = {}

function Explosion.new(params)
	local o = {}
	o.x = params.x
	o.y = params.y
	o.scale = params.scale
	o.duration = 0.7
	o.time = 0
	o.isInProcess = false
	local sound = love.audio.newSource("gas_explosion.ogg", "static")
	sound:play()
	setmetatable(o, {__index = Explosion})
	return o
end

function Explosion:update(dt)
	if self.isInProcess then
		if self.time <= self.duration then
			self.time = self.time+dt
			-- parse circles
			self:updateCircles(dt)
		else
			self.isInProcess = false
			onExplosionFinished({ entity = self })
		end
	end
end

function Explosion:draw()
	-- draw circles
	for i = 1,#self.circles do
		if self.circles[i].radius > 0 then
			love.graphics.setColor(self.circles[i].color.r, self.circles[i].color.g, self.circles[i].color.b)
			love.graphics.circle("fill", self.x+self.circles[i].x, self.y+self.circles[i].y, self.circles[i].radius, 50)
		end
	end
end

function Explosion:init()
	-- define circles
	local colors = {
		{ r = 252, g = 213, b = 80 },
		{ r = 255, g = 255, b = 255 },
		{ r = 59, g = 50, b = 81 },
	}
	self.circles = {}
	for i = 1,5 do
		local startTime = 	math.random()*i*0.1*self.duration
		local endTime = 	(0.5+math.random()*i*0.1)*self.duration
		local endRadius = 	(100+math.random(i*10))*self.scale
		local x =			(math.random(80)-40)*self.scale
		local y =			(math.random(80)-40)*self.scale
		local c1 = {
			startTime = 	startTime,
			endTime = 		endTime,
			startRadius = 	40*self.scale,
			endRadius = 	endRadius,
			x =				x,
			y =				y,
			color = 		colors[1],
			radius = -1
		}
		local c2 = {
			startTime = 	math.min(startTime+0.1, 0.9*self.duration),
			endTime = 		endTime,
			startRadius = 	0,
			endRadius = 	endRadius-10,
			x =				x,
			y =				y,
			color = 		colors[2],
			radius = -1
		}
		local c3 = {
			startTime = 	math.min(startTime+0.3, 0.9*self.duration),
			endTime = 		endTime,
			startRadius = 	0,
			endRadius = 	endRadius,
			x =				x,
			y =				y,
			color = 		colors[3],
			radius = -1
		}
		table.insert(self.circles, c1)
		table.insert(self.circles, c2)
		table.insert(self.circles, c3)
	end
	self.isInProcess = true
end

function Explosion:kill()
end

function Explosion:updateCircles(dt)
	for i = 1,#self.circles do
		if self.time > self.circles[i].endTime then
			self.circles[i].radius = 0
		elseif self.time > self.circles[i].startTime then
			local c = self.circles[i]
			if c.radius < 0 then c.radius = c.startRadius end
			local dr = (c.endRadius - c.startRadius)/(c.endTime - c.startTime)
			c.radius = c.radius + dr*dt
		end
	end
end

return Explosion
Implosion = {}

function Implosion.new(params)
	local o = {}
	o.x = params.x
	o.y = params.y
	o.scale = params.scale
	o.duration = 0.8
	o.time = 0
	o.isInProcess = false
	o.sound = love.audio.newSource("implode.wav", "static")
	setmetatable(o, {__index = Implosion})
	return o
end

function Implosion:update(dt)
	if self.isInProcess then
		if self.time <= self.duration then
			self.time = self.time+dt
			-- parse circles
			self:updateCircles(dt)
		else
			self.isInProcess = false
			onImplosionFinished({ entity = self })
		end
	end
end

function Implosion:draw()
	-- draw circles
	for i = 1,#self.circles do
		if self.circles[i].radius > 0 then
			love.graphics.setColor(self.circles[i].color.r, self.circles[i].color.g, self.circles[i].color.b)
			love.graphics.circle("fill", self.x, self.y, self.circles[i].radius, 50)
		end
	end
end

function Implosion:init()
	-- define circles
	self.circles = {
		{
			startTime = 	0.5*self.duration,
			endTime = 		1*self.duration,
			startRadius = 	150*self.scale,
			endRadius = 	210*self.scale,
			color = 		{ r = 0, g = 0, b = 0 },
			radius = -1
		},
		{
			startTime = 	0.5*self.duration,
			endTime = 		1*self.duration,
			startRadius = 	150*self.scale,
			endRadius = 	135*self.scale,
			color = 		{ r = 59, g = 50, b = 81 },
			radius = -1
		},
		{
			startTime = 	0.4*self.duration,
			endTime = 		0.7*self.duration,
			startRadius = 	100*self.scale,
			endRadius = 	150*self.scale,
			color = 		{ r = 0, g = 0, b = 0 },
			radius = -1
		},
		{
			startTime = 	0.3*self.duration,
			endTime = 		0.7*self.duration,
			startRadius = 	100*self.scale,
			endRadius = 	80*self.scale,
			color = 		{ r = 59, g = 50, b = 81 },
			radius = -1
		},
		{
			startTime = 	0*self.duration,
			endTime = 		0.6*self.duration,
			startRadius = 	50*self.scale,
			endRadius = 	80*self.scale,
			color = 		{ r = 0, g = 0, b = 0 },
			radius = -1
		},
		{
			startTime = 	0*self.duration,
			endTime = 		0.6*self.duration,
			startRadius = 	50*self.scale,
			endRadius = 	0*self.scale,
			color = 		{ r = 59, g = 50, b = 81 },
			radius = -1
		},
		-- main eraser
		{
			startTime = 	0.1*self.duration,
			endTime = 		1*self.duration,
			startRadius = 	0*self.scale,
			endRadius = 	210*self.scale,
			color = 		{ r = 59, g = 50, b = 81 },
			radius = -1
		},
		-- inner rim
		{
			startTime = 	0.6*self.duration,
			endTime = 		0.9*self.duration,
			startRadius = 	100*self.scale,
			endRadius = 	120*self.scale,
			color = 		{ r = 0, g = 0, b = 0 },
			radius = -1
		},
		{
			startTime = 	0.6*self.duration,
			endTime = 		0.9*self.duration,
			startRadius = 	100*self.scale,
			endRadius = 	100*self.scale,
			color = 		{ r = 59, g = 50, b = 81 },
			radius = -1
		},
		{
			startTime = 	0.6*self.duration,
			endTime = 		0.9*self.duration,
			startRadius = 	90*self.scale,
			endRadius = 	120*self.scale,
			color = 		{ r = 59, g = 50, b = 81 },
			radius = -1
		},
		-- after core
		{
			startTime = 	0.8*self.duration,
			endTime = 		1*self.duration,
			startRadius = 	30*self.scale,
			endRadius = 	80*self.scale,
			color = 		{ r = 0, g = 0, b = 0 },
			radius = -1
		},
		{
			startTime = 	0.8*self.duration,
			endTime = 		1*self.duration,
			startRadius = 	10*self.scale,
			endRadius = 	80*self.scale,
			color = 		{ r = 59, g = 50, b = 81 },
			radius = -1
		},
		{
			startTime = 	0*self.duration,
			endTime = 		0.7*self.duration,
			startRadius = 	50*self.scale,
			endRadius = 	10*self.scale,
			color = 		{ r = 0, g = 0, b = 0 },
			radius = -1
		},
	}
	self.isInProcess = true
	self.sound:play()
end

function Implosion:kill()
end

function Implosion:updateCircles(dt)
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

return Implosion
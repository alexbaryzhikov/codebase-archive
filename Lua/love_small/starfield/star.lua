Star = {}

function Star.new(params)
	o = {}
	o.size = params.size
	o.distance = params.distance
	o.radius = params.radius*math.tan(math.rad(OBSERVER_FOV/2))*windowWidth
	o.angle = params.angle
	setmetatable(o, {__index = Star})
	return o
end

function Star:update(dt)
	self.distance = self.distance - OBSERVER_SPEED*dt
	self.angle = self.angle + math.rad(OBSERVER_SPIN_SPEED)*dt
	if OBSERVER_SPEED >= 0 and self.distance < 1 then self:onStarClipped()
	elseif OBSERVER_SPEED < 0 and self.distance > FAR_CLIP then self:onStarClipped() end
end

function Star:draw()
	if self.distance > 1 and self.distance < FAR_CLIP then 
	    local r = self.radius/self.distance
		local x, y =
			math.cos(self.angle)*r + windowWidth/2,
			math.sin(self.angle)*r + windowHeight/2
		local s = self.size/self.distance
		local c = 255-(self.distance*1.063)^2
		love.graphics.setColor(c, c, c)
	    love.graphics.circle("fill", x, y, s, 12)
	end
end

function Star:onStarClipped()
	if onStarClipped then
		onStarClipped({ star = self })
	end
end

return Star
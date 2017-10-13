local M = {}

function M.new(params)
	o = {}
	o.host = params.host
	o.radius = params.radius
	o.stepDelta = params.stepDelta
	o.lifetime = params.lifetime
	o.color = params.color
	o.particles = {}
	o.isActive = false
	o.isUpdating = false
	setmetatable(o, { __index = M })
	return o
end

function M:update(dt)
	if not self.isUpdating then
		self.isUpdating = true
		local p = self.particles
		if #p > 0 then
			-- update particles ages
			for i = 1,#p do
				p[i].age = p[i].age + dt
			end
			-- insert new particle
			if self.isActive then
				local d = math.sqrt((self.host.x-p[#p].x)^2+(self.host.y-p[#p].y)^2)
				if d >= self.stepDelta then
					local c = { x = self.host.x, y = self.host.y, age = 0 }
					table.insert(p, c)
				end
			end
			-- delete old particles
			local pNew = {}
			for i,v in pairs(p) do
				if v.age < self.lifetime then
					table.insert(pNew, v)
				end
			end
			p = pNew
		else
			if self.isActive then
				local c = { x = self.host.x, y = self.host.y, age = 0 }
				table.insert(p, c)
			end
		end
		self.particles = p
		self.isUpdating = false
	end
end

function M:draw()
	if #self.particles > 0 then
		love.graphics.setColor(self.color.r, self.color.g, self.color.b)
		local p = self.particles
		for i = 1,#p do
			if p[i] then love.graphics.circle("fill", p[i].x, p[i].y, self.radius, 12) end
		end
	end
end

function M:init()
	self.isActive = true
end

function M:activate()
	self.isActive = true
end

function M:deactivate()
	self.isActive = false
end

return M
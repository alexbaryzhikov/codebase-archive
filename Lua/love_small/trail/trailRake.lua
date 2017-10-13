local trail = require("trail")

local M = {}

function M.new(params)
	o = {}
	-- parameters
	o.params = params
	o.host = params.host
	o.rake = params.rake
	o.offsetY = params.offsetY or 0
	o.radius = params.radius
	-- variables
	o.tracks = params.rake*2-1
	o.hostlings = {}
	o.trailings = {}
	o.isUpdating = false
	setmetatable(o, { __index = M })
	return o
end

function M:update(dt)
	if not self.isUpdating then
		self.isUpdating = true
		if #self.hostlings > 0 then
			for i = 1,#self.hostlings do
				self.hostlings[i].x =
					self.hostlings[i].rad*math.cos(self.host.angle+self.hostlings[i].ang-math.pi/2) + self.host.x
				self.hostlings[i].y =
					self.hostlings[i].rad*math.sin(self.host.angle+self.hostlings[i].ang-math.pi/2) + self.host.y
				self.trailings[i]:update(dt)
			end
		end
		self.isUpdating = false
	end
end

function M:draw()
	-- -- draw hostlings
	-- if #self.hostlings > 0 then
	-- 	love.graphics.setColor(self.params.color.r, self.params.color.g, self.params.color.b)
	-- 	for i = 1,#self.hostlings do
	-- 		love.graphics.circle("fill", self.hostlings[i].x, self.hostlings[i].y, self.radius/self.tracks, 12)
	-- 	end
	-- end
	-- draw trailings
	local t = self.trailings
	if #t > 0 then
		for i = 1,#t do
			t[i]:draw()
		end
	end
end

function M:init()
	self:createHostlings()
end

function M:activate()
	for i = 1,#self.trailings do
		self.trailings[i]:activate()
	end
end

function M:deactivate()
	for i = 1,#self.trailings do
		self.trailings[i]:deactivate()
	end
end

function M:createHostlings()
	local hostlingRadius = self.radius/self.tracks
	for i = 1,self.tracks,2 do
		local hostling = self:getHostlingXY(hostlingRadius, i)
		table.insert(self.hostlings, hostling)
		local t = trail.new(
			{
				host = hostling,
				radius = hostlingRadius,
				stepDelta = self.params.stepDelta,
				lifetime = self.params.lifetime*(1+0.3*(math.random()-0.5)),
				color = self.params.color,
			}
		)
		t:activate()
		table.insert(self.trailings, t)
	end
end

function M:getHostlingXY(hostlingRadius, num)
	local offsetX = hostlingRadius*(2*num - 1) - self.radius
	local angle = math.atan2(self.offsetY, offsetX)
	local r = math.sqrt(offsetX^2+self.offsetY^2)
	local x = r*math.cos(self.host.angle+angle-math.pi/2) + self.host.x
	local y = r*math.sin(self.host.angle+angle-math.pi/2) + self.host.y
	return { x = x, y = y, rad = r, ang = angle }
end

return M
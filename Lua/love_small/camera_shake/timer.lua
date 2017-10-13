local M = {}

function M.new(params)
	local o = {}
	if not params.callbackModule then print("ERROR! No callback: "..params.name) end
	-- parameters
	o.name = params.name
	o.callbackModule = params.callbackModule
	o.basePeriod = params.basePeriod
	o.randomness = math.max(math.min(params.randomness, 1), 0)
	o.isRepeating = params.isRepeating
	-- variables
	o.time = 0
	o.currentPeriod = o.basePeriod*(1 + o.randomness*(2*math.random() - 1))
	o.isPaused = false
	setmetatable(o, {__index = M})
	return o
end

function M:update(dt)
	if self.time >= self.currentPeriod then
		if self.isRepeating then
			self:onTimerTick()
		else
			self:onTimerFinish()
		end
	end
	if not self.isPaused then
		self.time = self.time+dt
	end
end

function M:onTimerTick()
	self.time = 0
	self.currentPeriod = self.basePeriod*(1 + self.randomness*(2*math.random() - 1))
	timekeeper.onTimerTick({ timer = self })
end

function M:onTimerFinish()
	timekeeper.onTimerFinish({ timer = self })
end

function M:pause()
	self.isPaused = true
end

function M:unpause()
	self.isPaused = false
end

function M:reset()
	self.time = 0
	self.currentPeriod = self.basePeriod*(1 + self.randomness*(2*math.random() - 1))
end

return M
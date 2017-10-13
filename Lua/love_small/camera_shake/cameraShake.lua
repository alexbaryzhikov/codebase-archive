local timer = require("timer")

local M = {
	magnitude = 5,
}
local config = {
	MAGNITUDE_RANDOMNESS = 0.15,
	ANGLE_SPREAD = math.pi/3,  -- PI is the max angle spread
	WAVE_DURATION_RANDOMNESS = 0.05,
	WAVE_DURATION = 0.06,
}
local wave = {
	magnitude = 0,
	angle = 0,
	duration = 0,
	offsetX = 0,
	offsetY = 0,
	stepX = 0,
	stepY = 0,
}	

local function getNewWave()
	-- get wave properties
	wave.magnitude = math.floor(0.5 + M.magnitude *
		(1 + config.MAGNITUDE_RANDOMNESS * (math.random() * 2 - 1)))
	local a = math.random() * config.ANGLE_SPREAD +	(math.pi - config.ANGLE_SPREAD) / 2
	if wave.angle < math.pi / 2 then 
		wave.angle = a + math.pi / 2
	else
		wave.angle = a - math.pi / 2
	end
	wave.duration = config.WAVE_DURATION *
		(1 + config.WAVE_DURATION_RANDOMNESS * (math.random() * 2 - 1))
	-- get step size
	local targetOffsetX = math.cos(wave.angle) * wave.magnitude
	local targetOffsetY = math.sin(wave.angle) * wave.magnitude
	wave.stepX = (targetOffsetX - wave.offsetX) / wave.duration
	wave.stepY = (targetOffsetY - wave.offsetY) / wave.duration
end

function M.init()
	M.shakeTimer = timer.new{
		name = "shakeTimer",
		callbackModule = M,
		basePeriod = config.WAVE_DURATION,
		randomness = config.WAVE_DURATION_RANDOMNESS,
		isRepeating = true
	}
	timekeeper.addTimer(M.shakeTimer)
	getNewWave()
end

function M.update(dt)
	-- update offsets
	wave.offsetX = wave.offsetX + wave.stepX * dt
	wave.offsetY = wave.offsetY + wave.stepY * dt
end

function M.draw()
   love.graphics.translate(wave.offsetX, wave.offsetY)
end

function M.onTimerTick(event)
	getNewWave()
end

return M
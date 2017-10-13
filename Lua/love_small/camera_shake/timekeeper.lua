local M = {}
-- subscribe to engine callbacks
-- addCallbackForward(M, { "update" })

local timers = {}

function M.update(dt)
	local t = {}
	for i,v in pairs(timers) do
		table.insert(t, v)
	end
	if #t > 0 then
		for i = 1,#t do
			if t[i] then t[i]:update(dt) end
		end
	end
end

function M.onTimerTick(event)
	if event.timer.callbackModule.onTimerTick then
		event.timer.callbackModule.onTimerTick({ timer = event.timer })
	end
end

function M.onTimerFinish(event)
	utils.removeFromList(timers, event.timer)
	if event.timer.callbackModule.onTimerFinish then
		event.timer.callbackModule.onTimerFinish({ timer = event.timer })
	end
end

function M.addTimer(t)
	table.insert(timers, t)
end

function M.removeTimer(t)
	utils.removeFromList(timers, t)
end

return M
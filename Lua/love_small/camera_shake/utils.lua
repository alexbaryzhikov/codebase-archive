local M = {}

function M.getDistance(pointA, pointB)
	local dx = pointA[1]-pointB[1]
	local dy = pointA[2]-pointB[2]
	return math.sqrt(dx^2+dy^2)
end

function M.getAccelXY(pointA, pointB, mass)
    local distance = utils.getDistance(pointA, pointB)
    local force = mass/distance^config.GRAVITY_DECAY_POWER
    local angle = math.atan2(pointB[2]-pointA[2], pointB[1]-pointA[1])
    local accelX = force*math.cos(angle)
    local accelY = force*math.sin(angle)
    return accelX, accelY
end

function M.removeFromList(list, ent)
    if list and #list > 0 then
        for i = 1,#list do
            if list[i] == ent then
            	table.remove(list, i)
            	break
            end
        end
    end
end

return M
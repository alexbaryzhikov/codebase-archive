P = {}
complex = P

local function checkComplex(c)
	if not ((type(c) == "table") and tonumber(c.r) and tonumber(c.i)) then
		print("Error: bad complex number")
		return false
	end
	return true
end

function P.new(r, i)
	return { r=r, i=i }
end

P.i = P.new(0, 1)

function P.add(c1, c2)
	if checkComplex(c1) and checkComplex(c2) then
		return P.new(c1.r+c2.r, c1.i+c2.i)
	end
end

return P


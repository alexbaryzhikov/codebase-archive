local function checkComplex(c)
	if not ((type(c) == "table") and tonumber(c.r) and tonumber(c.i)) then
		print("Error: bad complex number")
		return false
	end
	return true
end

local function new(r, i)
	return { r=r, i=i }
end

local i = new(0, 1)

local function add(c1, c2)
	if checkComplex(c1) and checkComplex(c2) then
		return new(c1.r+c2.r, c1.i+c2.i)
	end
end

-- export table
complex = {
	new = new,
	i = i,
	add = add,
}

return complex


-- Point: make module contents independent of module name, easy module renaming

local P = {}
complex = P  -- module name

function P.new(r, i)
	return { r=r, i=i }
end

P.i = P.new(0, 1)

function P.add(c1, c2)
	return P.new(c1.r+c2.r, c1.i+c2.i)
end

return P


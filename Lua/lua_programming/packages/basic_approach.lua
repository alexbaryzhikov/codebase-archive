complex = {}

function complex.new(r, i)
	return { r=r, i=i }
end

complex.i = complex.new(0, 1)

function complex.add(c1, c2)
	return complex.new(c1.r+c2.r, c1.i+c2.i)
end

return complex


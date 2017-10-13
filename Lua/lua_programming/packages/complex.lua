-- Point: remove explicit module name declaration and returning statement

local modname = ...  -- "require" passes file name to module as argument
local P = {}
_G[modname] = P  -- module name from file name
package.loaded[modname] = P  -- instead of returning P at the end

function P.new(r, i)
	return { r=r, i=i }
end

P.i = P.new(0, 1)

function P.add(c1, c2)
	return P.new(c1.r+c2.r, c1.i+c2.i)
end

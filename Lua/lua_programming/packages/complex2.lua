-- Point: make an exclusive environment for the module

local modname = ...
local P = {}
_G[modname] = P
package.loaded[modname] = P
setfenv(1, P)

function new(r, i)
	return { r=r, i=i }
end

i = new(0, 1)

function add(c1, c2)
	return new(c1.r+c2.r, c1.i+c2.i)
end

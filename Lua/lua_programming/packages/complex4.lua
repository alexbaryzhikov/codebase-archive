-- Point: make access to global environment faster

local modname = ...
local P = {}
_G[modname] = P
package.loaded[modname] = P
local _G = _G
setfenv(1, P)

function new(r, i)
	return { r=r, i=i }
end

i = new(0, 1)

function add(c1, c2)
	return new(c1.r+c2.r, c1.i+c2.i)
end

function sine(c)
	return _G.math.sin(c)
end
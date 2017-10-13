local a = require "a"

local M = {
	foo = "Basterd's work is never done",
}

function M.init()
	a.init(M)
end

function M.draw()
	a.draw()
	if M.foo then
		love.graphics.print("b - b.foo: "..M.foo, 10, 25)
	else
		love.graphics.print("b - b.foo: n/a", 10, 25)
	end
end

function M.change()
	M.foo = "Boy oh boy"
end

return M
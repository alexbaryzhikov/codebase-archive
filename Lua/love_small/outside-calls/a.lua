local M = {}

function M.init(arg)
	M.foo = arg.foo
	M.change = arg.change
	M.change()
end

function M.draw()
	if M.foo then
		love.graphics.print("a - a.foo: "..M.foo, 10, 10)
	else
		love.graphics.print("a - a.foo: n/a", 10, 10)
	end
end

return M
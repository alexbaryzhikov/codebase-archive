-- ===========================
-- Using modules
-- ===========================

-- require "mod"
-- mod.foo()

-- local m = require "mod"
-- m.foo()

-- require "mod"
-- local f = mod.foo
-- f()

-- ===========================
-- Basic approach
-- ===========================

-- require "basic_approach"
-- local c = complex.add(complex.i, complex.new(10, 20))
-- print(unpack{ c.r, c.i })

-- require "basic_approach2"
-- local c = complex.add(complex.i, complex.new(10, 20))
-- print(unpack{ c.r, c.i })

require "complex"
local c = complex.add(complex.i, complex.new(10, 20))
print(unpack{ c.r, c.i })

-- ===========================
-- Using Environments
-- ===========================

-- require "complex2"
-- local c = complex2.add(complex2.i, complex2.new(10, 20))
-- print(unpack{ c.r, c.i })

-- require "complex3"
-- local c = complex3.sine(1)
-- print(c)

-- require "complex4"
-- local c = complex4.sine(1)
-- print(c)

-- require "complex5"
-- local c = complex5.sine(1)
-- print(c)

-- ===========================
-- Privacy
-- ===========================

-- require "privacy"
-- local c = complex.add(complex.i, complex.new("y", 20))
-- if c then
-- 	print(unpack{ c.r, c.i })
-- end

-- require "privacy2"
-- local c = complex.add(complex.i, complex.new(10, 20))
-- if c then
-- 	print(unpack{ c.r, c.i })
-- end

-- finish
love.event.quit()

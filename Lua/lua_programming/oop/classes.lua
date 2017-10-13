-- ======================
-- Classes
-- ======================

-- make b a prototype for a:
-- setmetatable(a, {__index = b})

Account = {
	balance = 0,
	withdraw =
		function(self, v)
			self.balance = self.balance - v
		end,
	deposit =
		function(self, v)
			self.balance = self.balance + v
		end}

function Account:new(o)
	o = o or {}
	setmetatable(o, self)
	self.__index = self
	return o
end

a = Account:new{balance = 0}
a:deposit(100.00)
print(a.balance)

b = Account:new()
b:deposit(50.00) -- equivalent to b.balance = b.balance+50
print(b.balance)

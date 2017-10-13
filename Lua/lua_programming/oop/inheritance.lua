-- ======================
-- Inheritance
-- ======================

-- base class Account

Account = {balance = 0}

function Account:new(o)
	o = o or {}
	setmetatable(o, self)
	self.__index = self
	return o
end

function Account:deposit(v)
	self.balance = self.balance+v
	print(self.balance)
end

function Account:withdraw(v)
	if v>self.balance then
		print("insufficient funds")
	else
		self.balance = self.balance-v
		print(self.balance)
	end
end

-- subclass SpecialAccount

SpecialAccount = Account:new()
-- s inherits from SpecialAccount, which inherits from Account
s = SpecialAccount:new{limit = 1000.00}
s:deposit(100.00)

-- overriding base methods

function SpecialAccount:withdraw(v)
	if v-self.balance >= self:getLimit() then
		print("insufficient funds")
	else
		self.balance = self.balance-v
		print(self.balance)
	end
end

function SpecialAccount:getLimit()
	return self.limit or 0
end

s:withdraw(200.00)

function s:getLimit()
	return self.balance*0.10
end

s:deposit(1100.00)
s:withdraw(1100)
s:withdraw(1090)
s:deposit(10)
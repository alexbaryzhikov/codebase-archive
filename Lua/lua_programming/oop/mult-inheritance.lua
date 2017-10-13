-- ======================
-- Multiple Inheritance
-- ======================

-- look up for 'k' in list of tables 'plist'
local function search(k, plist)
	for i,v in pairs(plist) do
		local r = plist[i][k]
		if r then return r end
	end
end

function createClass(...)
	local c = {}
	local args = {...}
	-- class will search for each method in the list of its
	-- parents ('arg' is the list of parents)
	setmetatable(c, {__index =	function(t, k)
									return search(k, args)
								end})

	-- prepare 'c' to be metatable of its instances
	c.__index = c

	-- define a new constructor for this new class
	function c:new(o)
		o = o or {}
		setmetatable(o, c)
		return o
	end

	-- return new class
	return c
end

-- the use of CreateClass

Account = {balance = 0}
function Account:new(o)
	o = o or {}
	setmetatable(o, self)
	self.__index = self
	return o
end

Named = {}
function Named:getname()
	return self.name
end
function Named:setname(n)
	self.name = n
end

-- NamedAccount is a subclass of Account and Named
NamedAccount = createClass(Account, Named)

-- create instancess of a NamedAccount class
account = NamedAccount:new{name = "Paul"}
print(account:getname())

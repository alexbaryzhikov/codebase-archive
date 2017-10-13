-- ======================
-- Privacy
-- ======================

-- table, that represents the state of the object, is kept in closure of the methods

function newAccount(initialBalance)
	local self = {
		balance = initialBalance,
		LIM = 10000.00,
	}
	self.extra = function()
		if self.balance > self.LIM then
			return self.balance*0.10
		else
			return 0
		end
	end
	local withdraw = function(v) self.balance = self.balance - v end
	local deposit = function(v) self.balance = self.balance + v end
	local getBalance = function()
		return self.balance + self.extra()
	end

	return {
		withdraw = withdraw,
		deposit = deposit,
		getBalance = getBalance
	}
end

-- there's no way to access "self" directly
acc1 = newAccount(100.00)
acc1.withdraw(40.00)
print(acc1.getBalance())

acc1.deposit(15000.00)
print(acc1.getBalance())


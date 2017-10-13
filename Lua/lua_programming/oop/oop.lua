-- ======================
-- OOP
-- ======================

-- using "self" allows individual states

Account = {balance = 0}
function Account.withdraw(self, v)
	self.balance = self.balance - v
end

a = Account
a:withdraw(100.00)
print(a.balance)

a2 = {balance=0, withdraw = Account.withdraw}
a2:withdraw(200)
print(a2.balance)




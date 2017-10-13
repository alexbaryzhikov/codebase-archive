-- ===========================
-- The Single-Method Approach
-- ===========================

-- if an object has a single method, then we can just return it as the object representation

function newObject(value)
	return function (action, v)
		if action == "get" then return value
		elseif action == "set" then value = v
		else print("invalid action")
		end
	end
end

-- the use

d = newObject(0)
print(d("get"))
d("set", 10)
print(d("get"))

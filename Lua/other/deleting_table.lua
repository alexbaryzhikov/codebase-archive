a = { "a", 1 }
b = { "b", 2 }
c = { "c", 3 }
d = { "d", 4 }
foo = {}
table.insert(foo, a)
table.insert(foo, b)
bar = {}
table.insert(bar, c)
table.insert(bar, d)
alpha = {foo=foo, bar=bar}

for k,v in pairs(alpha) do
    for k,v in pairs(v) do
        print(unpack(v))
    end
end
print("\r")

function removeFromList(listName, ent)
    for i = 1,#alpha[listName] do
        if alpha[listName][i] == ent then
        	alpha[listName][i] = nil
        	break
        end
    end
end


removeFromList("foo", b)

for k,v in pairs(alpha) do
    for k,v in pairs(v) do
        print(unpack(v))
    end
end

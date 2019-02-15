double x = x * 2
quadruple = double . double

factorial n = product [1..n]
average l = sum l `div` length l

n = a `div` length xs where
    a = 10
    xs = [1, 2, 3, 4, 5]

add x y = x + y
fs = [tail, init, reverse]
second xs = tail (head xs)
swap (x, y) = (y, x)
pair x y = (x, y)
palindrome xs = reverse xs == xs
twice f x = f (f x)

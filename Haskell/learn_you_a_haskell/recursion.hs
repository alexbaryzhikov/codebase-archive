fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

max' :: Ord a => [a] -> a
max' []     = error "No maximum of empty list."
max' [x]    = x
max' (x:xs) = max x (max' xs)

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate (n - 1) x

take' :: Int -> [a] -> [a]
take' n xs
    | n <= 0 || null xs = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' xs ys
    | null xs || null ys = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs)
    | y == x    = True
    | otherwise = elem' y xs

reverseInt :: Int -> Int
reverseInt x = r 0 x
    where r acc 0 = acc
          r acc x = r (acc * 10 + mod x 10) (div x 10)

-- variant 1

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x : xs) = (sort ys) ++ [x] ++ (sort zs)
    where ys = [y | y <- xs, y <= x]
          zs = [z | z <- xs, z > x]


-- variant 2 (single pass)

sort1 :: Ord a => [a] -> [a]
sort1 [] = []
sort1 (x : xs) = (sort as) ++ bs ++ (sort cs)
    where (as, bs, cs) = foldr (insert x) ([], [], []) (x : xs)

insert :: Ord a => a -> a -> ([a], [a], [a]) -> ([a], [a], [a])
insert x i (as, bs, cs)
    | i < x     = (i : as, bs, cs)
    | i == x    = (as , i : bs, cs)
    | otherwise = (as, bs, i : cs)


-- variant 3

sort2 :: Ord a => [a] -> [a]
sort2 [] = []
sort2 xs = (sort2 as) ++ bs ++ (sort2 cs)
    where as = [a | a <- xs, a < head xs]
          bs = [b | b <- xs, b == head xs]
          cs = [c | c <- xs, c > head xs]
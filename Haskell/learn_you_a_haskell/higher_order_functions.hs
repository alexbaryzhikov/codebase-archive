applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ xs ys
    | null xs || null ys = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

largestDivisible :: Int
largestDivisible = head $ filter p [100000,99999..]
    where p x = mod x 3829 == 0

sumOfSquaredOdds :: Int
sumOfSquaredOdds = sum $ takeWhile (< 10000) $ filter odd $ map (^2) [1..]

collatzChain :: Int -> [Int]
collatzChain x = x : next
    where next
            | x == 1 = [] 
            | even x = collatzChain (div x 2)
            | otherwise = collatzChain (x * 3 + 1)

numLongChains :: Int
numLongChains = length $ filter (\xs -> length xs > 15) $ map collatzChain [1..100]

main :: IO ()
main = do
    print $ applyTwice (+3) 1 -- 7
    print $ applyTwice (3:) [1] -- [3,3,1]
    print $ applyTwice (++ " haha") "hey" -- "hey haha haha"

    print $ zipWith' (+) [1,2,3] [4,5,6] -- [5,7,9]
    print $ zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]

    print $ flip' zip "hello" [1..] -- [(1,'h'),(2,'e'),(3,'l'),(4,'l'),(5,'o')]
    print $ zipWith' (flip' div) [2,2..] [1..5] -- [0,1,1,2,2]

    print $ map (+3) [1,2,3,4] -- [4,5,6,7]
    print $ filter (>3) [1,2,3,4] -- [4]
    print $ filter (not.null) [[1,2],[],[],[3]] -- [[1,2],[3]]

    print largestDivisible -- 99554
    print sumOfSquaredOdds -- 166650

    print $ collatzChain 13 -- [13,40,20,10,5,16,8,4,2,1]
    print numLongChains -- 66

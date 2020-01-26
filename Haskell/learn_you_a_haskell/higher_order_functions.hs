applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ xs ys
    | null xs || null ys = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

main :: IO ()
main = do
    print $ applyTwice (+3) 1 -- 7
    print $ applyTwice (3:) [1] -- [3,3,1]
    print $ applyTwice (++ " haha") "hey" -- "hey haha haha"

    print $ zipWith' (+) [1,2,3] [4,5,6] -- [5,7,9]
    print $ zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]

    print $ flip' zip "hello" [1..] -- [(1,'h'),(2,'e'),(3,'l'),(4,'l'),(5,'o')]
    print $ zipWith' (flip' div) [2,2..] [1..5] -- [0,1,1,2,2]

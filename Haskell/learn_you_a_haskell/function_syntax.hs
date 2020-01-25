-- Pattern Matching

sayMyName :: String -> String
sayMyName "Heisenberg" = "You god damn right."
sayMyName x            = "Wrong."

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

callMe :: Char -> String
callMe 'd' = "dude"
callMe 'p' = "pal"

addVectors :: (Fractional a, Fractional b) => (a, b) -> (a, b) -> (a, b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

firstLetter :: String -> String
firstLetter ""         = "Empty string"
firstLetter all@(x:xs) = "First letter of " ++ all ++ " is " ++ [x]

-- Guards

bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b = b
    | otherwise = a

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
    | a == b = EQ
    | a < b = LT
    | otherwise = GT

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
    where (f:_, l:_) = (firstName, lastName)

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- Let

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

calcBmis2 :: [(Double, Double)] -> [Double]
calcBmis2 xs = [bmi | (w, h) <- xs
                   , let bmi = w / h ^ 2
                   , bmi > 25.0]

-- Case

head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty list"
                      (x:_) -> x

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty."
                                               [x] -> "a singleton."
                                               xs -> "a long list."

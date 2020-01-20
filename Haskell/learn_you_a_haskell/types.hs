factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Double -> Double
circumference r = 2 * pi * r

main :: IO ()
main = do
    print $ circumference 4

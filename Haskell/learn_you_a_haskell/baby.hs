doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 
                        then x
                        else x * 2

a = [x * 2 | x <- [1..10]
           , x * 2 >= 12]


boomBangs xs = [if x < 10
                  then "BOOM!"
                  else "BANG!" | x <- xs
                               , odd x]

-- right triangle

triples = [(a, b, c)
          | c <- [1..10]
          , a <- [1..c]
          , b <- [1..a]
          , a^2 + b^2 == c^2
          , a + b + c == 24]

-- pythagorean triples

pyth n = [(x, y, z)
         | x <- [1..n]
         , y <- [x..n]
         , z <- [y..n]
         , x^2 + y^2 == z^2]

pyth' n = do
  x <- [1..n]
  y <- [x..n]
  z <- [y..n]
  if x^2 + y^2 == z^2
    then [(x, y, z)]
    else []

main :: IO ()
main = print $ pyth' 25

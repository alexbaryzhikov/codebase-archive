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

{-
Countdown problem

Given a sequence of source numbers and a single target number, attempt to construct
an arithmetic expression using each of the source numbers at most once, and such that
the result of evaluating the expression is the target number. The given numbers are
restricted to being non-zero naturals, as are the intermediate results during evaluation
of the expression, which can otherwise be freely constructed using addition, subtraction,
multiplication and division.
-}

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val x)     = show x
  show (App o x y) = "(" ++ show x ++ " " ++ show o ++ " " ++ show y ++ ")"

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l
                                , y <- eval r
                                , valid o x y]

splits :: [a] -> [([a],[a])]
splits [] = []
splits xs = [(ys, zs) | i <- [0..length xs - 1]
                      , let (ys, zs) = splitAt i xs]

choices :: [a] -> [[a]]
choices [] = [[]]
choices xs = [] : [x : cs | (ys, zs) <- splits xs
                          , let x = head zs
                          , cs <- choices (ys ++ tail zs)]

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- splits ns
              , l       <- exprs ls
              , r       <- exprs rs
              , e       <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine x y = [App o x y | o <- [Add, Sub, Mul, Div]]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns
                    , e   <- exprs ns'
                    , eval e == [n]]

-- Optimization

type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls,rs) <- splits ns
                  , x       <- results ls
                  , y       <- results rs
                  , res     <- combine' x y]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- [Add, Sub, Mul, Div]
                                                 , valid' o x y]

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x <= y && x /= 1 && y /= 1
valid' Div x y = x `mod` y == 0 && y /= 1

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns'   <- choices ns
                     , (e,m) <- results ns'
                     , m == n]

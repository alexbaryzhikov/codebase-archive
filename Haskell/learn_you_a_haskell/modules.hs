import qualified Data.List as Lists
import qualified Data.Char as Chars

numUniques :: (Eq a) => [a] -> Int
numUniques = length . Lists.nub

digits :: Int -> [Int]
digits 0 = []
digits x = (mod x 10):(digits $ div x 10)

digitSum :: Int -> Int
digitSum = sum . digits

firstTo :: Int -> Maybe Int
firstTo n = Lists.find (\x -> digitSum x == n) [1..]
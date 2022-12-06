import Data.List
import Data.Char
import Debug.Trace

main = do 
    readFile "input.txt" >>= print . solve1
    readFile "input.txt" >>= print . solve2

all4Different :: String -> Bool
all4Different (a:b:c:d:_) 
    | c == d = False
    | b == c || b == d = False
    | a == b || a == c || a == d = False
    | otherwise = True
all4Different _ = True

allDifferent :: String -> Bool
allDifferent (x:[]) = True
allDifferent (x:xs) 
    | x `elem` xs = False
    | otherwise = allDifferent xs

solve1:: String -> String
solve1 s = 
    case  findIndex all4Different $ tails s of
        Just i -> show $ i + 4

solve2 :: String -> String
solve2 s =
    case findIndex (allDifferent . take 14 ) $ tails s of 
        Just i -> show $ i + 14
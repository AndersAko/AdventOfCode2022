import Data.List
--import Data.List.Split
--- Issues encountered:
-- Splitting strings, using Data.List.Split module or without,  


main = do 
    readFile "input.txt" >>= print . solve1 . parse
    readFile "input.txt" >>= print . solve2 . parse

-- 95-96,11-96\n...  => [[95,96],[11,96]]
parse :: String -> [[[Int]]]
parse s = map (map readAssignment . readPair) (lines s)

--- 95-96,11-96  => ["95,96","11,96"]
readPair :: String -> [String]
readPair s = 
    let commaIndex = (elemIndices ',' s) !! 0
    in [take commaIndex s, drop (commaIndex+1) s ]

--- 95-96  => [95,96]
readAssignment :: String -> [Int]
readAssignment s = 
    let dashIndex = (elemIndices '-' s) !! 0
    in [ read x | x <- [take dashIndex s, drop (dashIndex+1) s ]]



------ part 1
solve1 :: [[[Int]]] -> String
solve1 i = show (length ( filter fullOverlap i))

fullOverlap :: [[Int]] -> Bool
fullOverlap ((a0:a1:_):(b0:b1:_):_) 
    | a0 <= b0 && a1 >= b1 = True
    | b0 <= a0 && b1 >= a1 = True
    | otherwise = False
    
------ part 2
solve2 :: [[[Int]]] -> String
solve2 i = show (length ( filter partialOverlap i))

partialOverlap :: [[Int]] -> Bool
partialOverlap ((a0:a1:_):(b0:b1:_):_) 
    | a1 < b0 = False
    | a0 > b1 = False
    | otherwise = True
    

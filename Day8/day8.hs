import Data.List
import Data.Char
import Debug.Trace

main = do 
    input <- readFile "input.txt"
    putStrLn . solve1 $ lines input
    putStrLn . solve2 $ lines input
    -- readFile "input.txt" >>= print . solve2 . lines

-- part 1
-- Generate list of bools, true if visible, as seen from the start of the list
visibleFromLeft :: String -> [Bool]
visibleFromLeft s = map (\x -> all (<last x) $ init x) $ tail $ inits s

visibleInLine :: String -> [Bool]
visibleInLine s = 
    let left = visibleFromLeft s 
        right = reverse . visibleFromLeft . reverse  
    in zipWith max left $ right s
--        map (\x -> fst x || snd x) $ zip left right

solve1 :: [String] -> String
solve1 s = 
    let fromSide = map visibleInLine s
        fromTop = transpose $ map visibleInLine $ transpose s
        total = zipWith (zipWith max) fromTop fromSide 
        countTrue = sum $ map (length . filter (==True)) total
    in --trace (show total) 
        show countTrue

-- part 2
viewDistanceToRight :: String -> [Int]
viewDistanceToRight s = 
    let treeLines = init $ tails s      -- ["33549","3549","549","49","9"] Trees to the right of each cell + itself
    in map (\x -> viewDistanceToRightForTree (head x) (tail x)) treeLines

viewDistanceToRightForTree:: Char -> String -> Int
viewDistanceToRightForTree _ [] = 0
viewDistanceToRightForTree tree (x:xs) 
    | x >= tree     = 1
    | otherwise     = 1 + viewDistanceToRightForTree tree xs 

solve2 :: [String] -> String
solve2 s = 
    let scoresRight     = map viewDistanceToRight s
        scoresLeft      = map (reverse . viewDistanceToRight . reverse) s
        scoresTop       = transpose $ map viewDistanceToRight $ transpose s
        scoresBottom    = transpose $ map (reverse . viewDistanceToRight . reverse) $ transpose s
        product4 a b c d = a * b * c * d
        scores          = zipWith4 (zipWith4 product4) scoresLeft scoresRight scoresBottom scoresTop 
    in trace ("Scores: " ++ show scores ++ "\n") 
        show (maximum $ map maximum scores)

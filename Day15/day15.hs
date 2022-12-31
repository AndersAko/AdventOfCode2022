import Data.Char 
import Data.List
import qualified Data.Set as Set 
import Data.Set (Set) 
import Data.Maybe
import Debug.Trace

main = do 
    input <- readFile "input.txt"
    putStrLn (solve1 $ parse $ lines input)
    putStrLn (solve2 $ parse $ lines input)

--- Parse input 
parse:: [String] -> [Sensor]
parse [] = []
parse (l:ls) = 
    let ws = words l
        readNum = read . filter (\x -> isDigit x || x=='-')
    in (Sensor { loc = (readNum (ws !! 2), readNum (ws !! 3)), closest = (readNum (ws !! 8), readNum (ws !! 9)) }):parse ls

data Sensor = Sensor { loc::(Int,Int), closest:: (Int, Int)} deriving (Show, Eq)
coveredDist Sensor{loc = (sensorX,sensorY), closest = (closestX,closestY)} = 
     abs (closestX - sensorX) + abs (closestY - sensorY)

coverRange rowY sensor@Sensor{loc = (sensorX,sensorY)} = 
    let distY = abs (rowY - sensorY)
        halfLengthCovered = (coveredDist sensor) - distY
    in  if halfLengthCovered < 0 then Nothing 
        else Just (sensorX - halfLengthCovered, sensorX + halfLengthCovered )

mergeRange:: [(Int,Int)] -> Maybe (Int,Int) -> [(Int,Int)]
mergeRange x Nothing = x
mergeRange [] (Just a)  = [a]
mergeRange (x1@(min1, max1):xs) (Just a@(min2, max2)) 
    | max2 < (min1-1) || max1 < (min2-1) = x1:(mergeRange xs $ (Just a))
    | otherwise  = mergeRange xs (Just (min min1 min2, max max1 max2)) 


solve1:: [Sensor] -> String
solve1 xs = 
    let isCoveredByAny (x,y) = any id $ map (isCoveredBy (x,y)) xs
        isCoveredBy :: (Int,Int) -> Sensor -> Bool 
        isCoveredBy (x,y) sensor@Sensor{loc = (sensorX,sensorY)} = 
            abs (x - sensorX) + abs (y - sensorY) <= coveredDist sensor
        minX = minimum $ map (\sensor@Sensor{loc =(x,_)} -> x - coveredDist sensor ) xs
        maxX = maximum $ map (\sensor@Sensor{loc =(x,_)} -> x + coveredDist sensor ) xs
        minY = minimum $ map (\sensor@Sensor{loc =(_,y)} -> y - coveredDist sensor ) xs
        maxY = maximum $ map (\sensor@Sensor{loc =(_,y)} -> y + coveredDist sensor ) xs
    in trace (show minX ++ " - " ++ show maxX ++ "; " ++ show minY ++ " - " ++ show maxY ++ "\n" )
        -- "Part 1: " ++ (show $ length $ 
        --     filter (\loc -> all (\Sensor{closest = beacon} -> loc /= beacon) xs) $ 
        --     [(x,y) | x<- [minX..maxX], y <- [2000000], let covered = isCoveredByAny (x,y), covered ])
        "Part 1: " ++ (
            let inspectRow =  2000000 -- 10 
                coveredRanges = foldl (mergeRange) [] $ map (coverRange inspectRow) xs 
                sumCovered = sum $ map (\(min,max) -> max - min + 1) coveredRanges
                beacons = nub $ map (\Sensor{closest = (x,y)} -> (x,y) ) $ filter (\Sensor{closest = (_,y)} -> y == inspectRow) xs
            in show coveredRanges ++ " => " ++ show (sumCovered - (length beacons ))
            )

solve2:: [Sensor] -> String
solve2 xs = 
    let coveredRanges = map (\row -> (row,foldl (mergeRange) [] $ map (coverRange row) xs))  [0..4000000] --[0..20] 
        possibleRows = filter (\(ix,r) -> length r > 1) coveredRanges
        candidateRange = snd $ head possibleRows
        emptyLocation = head [(x,y) | x <- [0..4000000] , (y,_) <- possibleRows, not $ any (\(minX,maxX) -> (x >= minX && x <= maxX)) candidateRange] 
    in " Possible rows: " ++ show possibleRows ++ " with empty location at " ++ show emptyLocation ++ 
        " => " ++ show ((fst emptyLocation ) * 4000000 + snd emptyLocation)
        
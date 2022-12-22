import Data.List
import Debug.Trace

main = do 
    input <- readFile "input.txt"
    putStrLn $ solve1 $ map (map read . splitOn ',') . lines $ input
    putStrLn $ solve2 $ map (map read . splitOn ',') . lines $ input


splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c (x:xs) | x == c = splitOn c xs
splitOn c s = (takeWhile (/= c) s):splitOn c (dropWhile (/= c) s)

neighbours (xc:yc:zc:[]) = [[x+xc,y+yc,z+zc] | x <- [-1..1], y <- [-1..1], z <- [-1..1], (x,y,z)/=(0,0,0) && ((x==0 && y==0) || (y==0 && z==0) || (x==0 && z==0))]

solve1:: [[Int]] -> String
solve1 input = 
    let sumSides :: ([[Int]], Int) -> [Int] -> ([[Int]], Int)
        sumSides (cubesSoFar, sum) newCube = 
            let matching = intersect cubesSoFar (neighbours newCube)
            in (newCube:cubesSoFar, sum + 6 - 2*(length matching) )
        (_,totalSum) = foldl sumSides ([], 0) input 
    in "Part1: " ++ show totalSum

-- Solve 2  attempt 1: Surface area = length of airCubes, except the ones which are 6    
--          attempt 2: Expand all bubbles, remove any bubbles from airCubes list
solve2_1:: [[Int]] -> String
solve2_1 input = 
    let airCubes = filter (not . flip elem input) $ foldl (\acc cube -> acc ++ (neighbours cube)) [] input
        -- go through the list of airCubes, expand to all neighbours, not in input. Returns all cubes in bubble
        expandBubble :: [[Int]] -> [[Int]] -> [[Int]]
        expandBubble [] _ = []
        expandBubble (cube:cubes) blockedCubes = 
            let expandTo = filter (not . flip elem blockedCubes) (neighbours cube)
            in --trace ("expand: " ++ show (cube:cubes) ++ "\n" ) $ 
                cube:(expandBubble (cubes ++ expandTo) (expandTo ++ blockedCubes))

        searchDepth = 1000
        findBubbles [] = []
        findBubbles (cube:cubes) = 
            let bubble = take searchDepth $ expandBubble [cube] (cube:input) 
                remaining = filter (not . flip elem bubble) cubes 
            in  if length bubble < searchDepth then 
                    trace ("Found a bubble: " ++ (show bubble) ++ "\n" )
                    bubble ++ (findBubbles remaining) 
                else findBubbles remaining
        airBubbles = findBubbles uniqueAirCubes
        uniqueAirCubes = map head $ group $ sort airCubes 
        externalSides =  sum $ map length $ group $ sort $ filter (not . flip elem airBubbles) airCubes 
    in "Unique air cubes: " ++ (show uniqueAirCubes) ++ "\n" ++ "Bubbles: " ++ (show airBubbles) ++ "\n" ++
        "External sides: " ++ show externalSides ++ "\n" ++ (show $ map length $ group $ sort $ filter (not . flip elem airBubbles) airCubes)

-- Solve 2 attempt 3 - expand outside lava droplet, count all cubes found
solve2 :: [[Int]] -> String
solve2 input = 
    let (minX, minY, minZ) = ((minimum $ map (!! 0) input)-1, (minimum $ map (!! 1) input)-1, (minimum $ map (!! 2) input)-1)
        (maxX, maxY, maxZ) = ((maximum $ map (!! 0) input)+1, (maximum $ map (!! 1) input)+1, (maximum $ map (!! 2) input)+1)
        externalSurface :: [[Int]] -> [[Int]] -> [[Int]]
        externalSurface [] _ = []
        externalSurface (cube:cubes) blockedCubes = 
            let filterCube c@(x:y:z:[]) 
                    | x > maxX = False
                    | y > maxY = False
                    | z > maxZ = False
                    | x < minX = False
                    | y < minY = False
                    | z < minZ = False
                    | otherwise = not (elem c blockedCubes)
                expandTo = filter filterCube (neighbours cube)
                lava = filter (flip elem input) expandTo
            in -- trace ("expand: " ++ show (cube:cubes) ++ " => " ++ show lava ++ "\n" ) $ 
            lava ++ (externalSurface (cubes ++ (expandTo \\ lava)) ((expandTo \\ lava) ++ blockedCubes))
    in  show (minX, minY, minZ) ++ " - " ++ show  (maxX, maxY, maxZ) ++ "\n" ++ show (length $ externalSurface [[minX, minY, minZ]] []) ++ "\n"
        -- ++ show (externalSurface [[minX, minY, minZ]] [])


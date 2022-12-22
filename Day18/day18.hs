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


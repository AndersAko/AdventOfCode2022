{-# LANGUAGE MultiWayIf #-}
import Data.Char 
import Data.List
import qualified Data.Set as Set 
import Data.Set (Set) 
import Data.Maybe
import Debug.Trace

main = do 
    input <- readFile "input.txt"
    putStrLn (solve $ parse $ lines input)
    -- putStrLn (solve2 $ parse $ lines input)

type Coord = (Int,Int)
parse:: [String] -> Set Coord
parse s = 
    let readCoord :: String -> (String, Coord)
        readCoord s =  
            let (first,rest) = span (isDigit) s
                (second, rest2) = span isDigit $ tail rest
            in (rest2,(read first, read second))

        parseLine :: Coord -> String -> [Coord]
        parseLine _ []       = []
        parseLine from@(x1,y1) (' ':'-':'>':' ':s)  = 
            let (t,toCoord@(x2,y2)) = readCoord s
                dirX = if x2<x1 then signum (x2-x1) else 1
                dirY = if y2<y1 then signum $ y2-y1 else 1
            in [(x,y) | x <- [x1, x1+dirX..x2], y<- [y1, y1+dirY..y2]] ++ parseLine toCoord t
        parseLine _ s  = 
            let (t,from) = readCoord s in parseLine from t
    in Set.fromList $ concat $  map (parseLine (0,0)) s

solve:: Set Coord -> String
solve coords = 
    let maxY = maximum $ map snd $ Set.toAscList coords
        nextSand :: (Coord, Set Coord) -> (Coord, Set Coord)
        nextSand (_, blocked) = let new = sand blocked (500,0) in (new, Set.insert new blocked)

        sand:: Set Coord -> Coord -> Coord
        sand blocked coord@(x,y) 
            | y >= maxY+1 = coord
            | not $ Set.member (x,y+1) blocked = sand blocked (x,y+1)
            | not $ Set.member (x-1,y+1) blocked = sand blocked (x-1,y+1)
            | not $ Set.member (x+1,y+1) blocked = sand blocked (x+1,y+1)
            | otherwise = coord
        part1 = map fst $ tail $ takeWhile (\((_,y),_) -> y<=maxY ) $ iterate nextSand ((0,0), coords)
        part2 = map fst $ takeWhile (\((_,y),_) -> y>0 ) $ tail $ iterate nextSand ((0,0), coords)
    in -- trace ("MaxY = "++ show maxY ++ "\n" ++ show part2)
    "Part 1: " ++ (show $ length part1 ) ++ "\n" ++
    "Part 2: " ++ (show $ (length part2) +1)


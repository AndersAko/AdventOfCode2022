import Data.List
import Data.Char
import qualified Data.Set as Set 
import Data.Set (Set) 

import Debug.Trace

main = do 
    input <- readFile "input.txt"
    putStrLn $ solve  (map words $ lines input) 2
    putStrLn $ solve (map words $ lines input) 10


solve1 :: [[String]] -> String
solve1 moves = 
    let positions :: [(Int,Int,Int,Int)]
        positions = foldl (\list (d:n:_) -> list <> (move d (read n) (last list)) )
            [(0,0,0,0)] moves 
        -- Direction number current_position => positions
        move :: String -> Int -> (Int,Int,Int,Int) -> [(Int,Int,Int,Int)]
        move _ 0 _        = []  
        move "R" n (xh,yh,xt,yt) = let newPos = moveTail (xh+1,yh,xt,yt) in newPos:move "R" (n-1) newPos
        move "L" n (xh,yh,xt,yt) = let newPos = moveTail (xh-1,yh,xt,yt) in newPos:move "L" (n-1) newPos
        move "U" n (xh,yh,xt,yt) = let newPos = moveTail (xh,yh-1,xt,yt) in newPos:move "U" (n-1) newPos
        move "D" n (xh,yh,xt,yt) = let newPos = moveTail (xh,yh+1,xt,yt) in newPos:move "D" (n-1) newPos
        move _ _ _ = trace ("Missing something") [] 
        moveTail (xh,yh,xt,yt) = -- trace $ show (xh,yh,xt,yt) ++ " " ++ show (xh-xt) ++ " " ++ show 
            (xh,
            yh,
            if xh-xt > 1 || ((abs (yh - yt) > 1) && xt < xh) then xt+1 else if xh-xt < -1 || ((abs (yh - yt) > 1) && xt > xh) then xt-1 else xt,
            if yh-yt > 1 || ((abs (xh - xt) > 1) && yt < yh) then yt+1 else if yh-yt < -1 || ((abs (xh - xt) > 1) && yt > yh) then yt-1 else yt)
        
        uniqueTailPositions = Set.fromList $ map (\(_,_,x,y) -> (x,y)) positions
    in show positions ++ "\nUnique: " ++ (show uniqueTailPositions) ++ " " ++ (show $ Set.size uniqueTailPositions)

solve :: [[String]] -> Int -> String
solve moves knots = 
    let positions :: [[(Int,Int)]]
        positions = foldl (\list (d:n:_) -> list ++ (move d (read n) (last list)) )
            [replicate knots (0,0)] moves 
        -- Direction number current_position => positions
        move :: String -> Int -> [(Int,Int)] -> [[(Int,Int)]]
        move _ 0 _        = []  
        move "R" n rope@((xh,yh):xs) = let newRope::[(Int,Int)] = moveTail ((xh+1,yh):xs) in newRope:move "R" (n-1) newRope
        move "L" n rope@((xh,yh):xs) = let newRope = moveTail ((xh-1,yh):xs) in newRope:move "L" (n-1) newRope
        move "U" n rope@((xh,yh):xs) = let newRope = moveTail ((xh,yh-1):xs) in newRope:move "U" (n-1) newRope
        move "D" n rope@((xh,yh):xs) = let newRope = moveTail ((xh,yh+1):xs) in newRope:move "D" (n-1) newRope

        moveTail :: [(Int,Int)] -> [(Int,Int)]
        moveTail ((xh,yh):(xt,yt):xs) = 
            (xh,yh):(moveTail 
                ((if xh-xt > 1 || ((abs (yh - yt) > 1) && xt < xh) then xt+1 else if xh-xt < -1 || ((abs (yh - yt) > 1) && xt > xh) then xt-1 else xt,
                if yh-yt > 1 || ((abs (xh - xt) > 1) && yt < yh) then yt+1 else if yh-yt < -1 || ((abs (xh - xt) > 1) && yt > yh) then yt-1 else yt)
                :xs))
        moveTail ((x,y):[]) = [(x,y)]
        
        uniqueTailPositions = Set.fromList $ map last positions
    in "Knots: " ++ show knots ++ " Unique positions for tail: " ++ (show $ Set.size uniqueTailPositions)
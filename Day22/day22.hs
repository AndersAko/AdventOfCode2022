{-# LANGUAGE MultiWayIf #-}
import Data.Char 
import Data.List
import qualified Data.Set as Set 
import Data.Set (Set) 
import qualified Data.Map as Map 
import Data.Map (Map) 
import Data.Maybe
import Debug.Trace

main = do 
    input <- readFile "input.txt"
    -- putStrLn (test $ parse $ lines $ input)
    putStrLn (solve1 $ parse $ lines input)
    putStrLn (solve2 $ parse $ lines input)

parse :: [String] -> ([String], String)
parse s = let p = break ((==) []) s in (fst p,(snd p)!!1)

type Coord = (Int, Int, Int) -- x, y, dir (0=R, 1=D, ...)

move1 :: [String] -> Coord -> String -> Coord
move1 board coord [] = coord
move1 board coord@(x,y,dir) instructions@(i:is)
    | isDigit i       = let sizeX = length $ board !! 0
                            sizeY = length $ board

                            executeMove :: Int -> Coord -> Coord 
                            executeMove 0 c = c
                            executeMove steps coord@(x,y,d) = 
                                let cmd = if 
                                            | d == 0 -> right
                                            | d == 1 -> down
                                            | d == 2 -> left
                                            | d == 3 -> up
                                in --trace ("exM: " ++ show coord ++ " = " ++ show ((board !! y) !! (x)))
                                executeMove (steps-1) (case step cmd (x,y) of 
                                    Nothing -> (x,y,d)
                                    Just (x1,y1) -> (x1,y1,d))
                            step cmd coord@(x,y) 
                                | x1 >= length (board !! y1) = step cmd (x1,y1)
                                | isSpace $ (board !! y1 ) !! x1 = step cmd (x1,y1)
                                | '#' == (board !! y1) !! x1 = Nothing
                                | otherwise = Just (x1,y1)
                                where (x1,y1) = cmd coord

                            right (x,y) = if x+1 >= sizeX then (0,y) else (x+1,y)
                            left (x,y)  = if x == 0 then (sizeX-1,y) else (x-1,y)
                            down (x,y)  = if y+1 >= sizeY then (x,0) else (x,y+1)
                            up (x,y)    = if y == 0 then (x,sizeY-1) else (x,y-1)
                            
                            (numMoves,rest)     = span isDigit instructions
                        in move1 board (executeMove (read numMoves) coord) rest
    | i == 'R'      = move1 board (x,y, ((dir+1) `mod` 4)) is
    | i == 'L'      = move1 board (x,y, ((dir+3) `mod` 4)) is  -- -1 +4
    | otherwise     = trace ("Lacking pattern for " ++ show i ++ " : " ++ show is ++ " at " ++ show coord) coord

solve1:: ([String], String) -> String
solve1 (board,instr) = 
    let startX = fromJust $ findIndex (not . isSpace) (board!!0)
        endPos@(x,y,d) = move1 board (startX,0,0) instr
    in show "Part 1" ++ show (endPos) ++ " => " ++ show (1000 * (y+1) + 4*(x+1) + d)

move2 :: [String] -> Coord -> String -> Coord
move2 board coord [] = coord
move2 board coord@(x,y,dir) instructions@(i:is)
    | isDigit i       = let 
                            executeMove :: Int -> Coord -> Coord 
                            executeMove 0 c = c
                            executeMove steps coord@(x,y,d) = 
                                let tryPos@(x1,y1,_) =  step coord
                                            -- | d == 0 -> right (x,y)
                                            -- | d == 1 -> down (x,y)
                                            -- | d == 2 -> left (x,y)
                                            -- | d == 3 -> up
                                in trace ("exM: " ++ show coord ++ " = " ++ show ((board !! y) !! (x)))
                                executeMove (steps-1) (if  '#' == (board !! y1) !! x1 then coord else
                                                         tryPos)

                            step (149,y,0)
                                | y < 50 = (99,149-y,2)                 -- EF
                            step (99,y,0)                
                                | y >= 50 && y < 100 = (y+50,49,3)     -- FG
                                | y >= 100 && y < 150 = (149,149-y,2)   -- EF
                            step (49,y,0)                
                                | y >= 150 && y < 200 = (y-100,149,3)     -- EH
                            step (x,y,0) = (x+1,y,0)
                            step (50,y,2) 
                                | y < 50 = (0,149-y,0)                    -- BC
                                | y >= 50 && y < 100 = (y-50,100,1)     -- AB
                            step (0,y,2) 
                                | y >= 100 && y < 150 = (50,149-y,0)    -- BC 
                                | y >= 150 && y < 200 = (y-100,0,1)     -- CD
                            step (x,y,2)  = (x-1,y,2)
                            step (x,49,1) 
                                | x >= 100 && x < 150 = (99,x-50,2)     -- FG
                            step (x,149,1) 
                                | x >= 50 && x < 100 = (49,x+100,2)     -- EH
                            step (x,199,1) 
                                | x >= 0 && x < 50 = (x+100,0,1)        -- DE
                            step (x,y,1)  = (x,y+1,1)
                            step (x,0,3) 
                                | x >= 50 && x < 100 = (0,x+100,0)      -- CD
                                | x >= 100 && x < 150 = (x-100,199,3)   -- DE
                            step (x,100,3) 
                                | x >= 0 && x < 50 = (50,x+50,0)        -- AB
                            step (x,y,3)    = (x,y-1,3)
                            
                            (numMoves,rest)     = span isDigit instructions
                        in trace (show coord)
                        move2 board (executeMove (read numMoves) coord) rest
    | i == 'R'      = move2 board (x,y, ((dir+1) `mod` 4)) is
    | i == 'L'      = move2 board (x,y, ((dir+3) `mod` 4)) is  -- -1 +4
    | otherwise     = trace ("Lacking patern for " ++ show i ++ " : " ++ show is ++ " at " ++ show coord) coord

solve2:: ([String], String) -> String
solve2 (board,instr) = 
    let startX = fromJust $ findIndex (not . isSpace) (board!!0)
        endPos@(x,y,d) = move2 board (startX,0,0) instr
    in show "Part 2" ++ show (endPos) ++ " => " ++ show (1000 * (y+1) + 4*(x+1) + d)

test (board,_) = 
    let testMove c = let result = move2 board c "1RR1RR" in (c == result, c, result) 
        coords = [(55,0,3), (105,0,3), (50,11,2), (50,61,2), (99,61,0), (0,111,2), (0,161,2), (31,199,1), (49,161,0), (61,149,1), (99,111,0)]
    in show (map testMove coords)
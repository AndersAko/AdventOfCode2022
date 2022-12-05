-- import Data.List
import Data.Char
import Debug.Trace

main = do 
    readFile "input.txt" >>= print . solve1 . parse
    readFile "input.txt" >>= print . solve2 . parse
{-
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
-}

type Move = String
type Moves = [Move]
type Stack = [Char]
type Stacks = [Stack]

parse :: String -> (Stacks, Moves) 
parse s = 
    let ls = lines s
        moves = tail $ dropWhile (not . null) ls
        stackLines = reverse $ takeWhile (not . null) ls 
    in (foldl addParsedLine (repeat []) stackLines , moves)  

addParsedLine:: [Stack] -> String -> [Stack]
addParsedLine stacks line =
    let col i = i*4 +1
        positions = filter (\i -> col i < length line) [0..8]
        possiblyAdd stack c = if isLetter c then c:stack else stack
    in map (\i -> possiblyAdd (stacks !! i) (line !! col i) ) positions

--- part 1
solve1 :: (Stacks, Moves) -> String
solve1 (stacks,moves) = 
    let applyOneMove stacks (from,to) = map (\i -> 
            if i == from-1 then tail $ stacks !! i
            else if i == to-1 then (head $ stacks !! (from-1)):(stacks !! i)
            else stacks !! i
            )    [0..length stacks-1 ]
        applyMove stacks move = 
            let args = map read $ words move
            in foldl (\acc _ -> 
                trace ("applyOneMove " ++ show (args !! 3, args !! 5) ++ " on " ++ show acc) 
                (applyOneMove acc (args !! 3, args !! 5) )) stacks [1..args !! 1]
        endResult = foldl applyMove stacks moves
        answer = map head endResult
    in trace ("\nFinishing with " ++ show endResult) answer

--- part 2
solve2 :: (Stacks, Moves) -> String
solve2 (stacks,moves) = 
    let applyOneMove stacks (from,to,num) = map (\i -> 
            if i == from-1 then drop num $ stacks !! i
            else if i == to-1 then (take num $ stacks !! (from-1))++(stacks !! i)
            else stacks !! i
            )    [0..length stacks-1 ]
        applyMove stacks move = 
            let args = map read $ words move
            in trace ("applyOneMove " ++ show (args !! 1) ++ " x " ++ show (args !! 3, args !! 5) ++ " on " ++ show stacks) 
                (applyOneMove stacks (args !! 3, args !! 5, args !! 1) )  
        endResult = foldl applyMove stacks moves
        answer = map head endResult
    in trace ("\nFinishing with " ++ show endResult) answer


{-
Issues with parsing input. Normal issues with syntax and operator priority.
-}
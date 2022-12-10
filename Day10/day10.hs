import Data.List
import Data.Char
import Debug.Trace

main = do 
    readFile "input.txt" >>= print . solve1 . lines
    readFile "input.txt" >>= print . solve2 . lines

type State = Int            -- register X
type Program = [Statement]  
type Statement = String     

execute :: Statement -> State -> [State] 
execute [] state            = [state]
execute ("noop") state      = [state]
execute statement state
    | instr == "addx" =  [state, state + read arg1]
    where (instr:arg1:_) = words statement 

solve1:: Program -> String
solve1 prog =
    let result = foldl (\acc line -> acc ++ execute line (last acc)) [1] prog
        signal = map (\x -> fst x * snd x) $ zip result [1..] 
        keyResults = [fst s | s <- zip signal [1..], snd s `elem` [20,60,100,140,180,220] ]
    in show signal ++ "  " ++ (show keyResults) ++ "  " ++ show (sum keyResults)

solve2 :: Program -> String
solve2 prog = 
    let result = foldl (\acc line -> acc ++ execute line (last acc)) [1] prog
        pixels = map (\x -> 
                        let sprite = [(fst x-1)..(fst x+1)]
                            crt = mod (snd x) 40
                        in if crt `elem` sprite then '#' else '.' ) $ zip result [0..] 
    in (show result) ++ "  " ++ pixels 

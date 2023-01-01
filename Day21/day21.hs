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
    putStrLn (solve1 $ parse $ lines input)
    -- putStrLn (solve2 $ parse $ lines input)

data Monkey = Yell String Int | Wait String Monkey Monkey Op deriving Show
type Op = String
type Monkeys = Map String Monkey

parse:: [String] -> Monkey
parse inp = 
    let monkeys = map (\l -> let (name,(':':' ':monkey)) = break (==':') l in (name,monkey)) inp
        getMonkey :: String -> (String, String)
        getMonkey name = fromJust $ find (\(n,_) -> n == name) monkeys

        createMonkey:: (String, String) -> Monkey 
        createMonkey (name, monkey) 
            | (length $ words monkey) == 1 = Yell name (read monkey)
            | otherwise = Wait name (createMonkey $ getMonkey (args!!0)) (createMonkey $ getMonkey (args!!2)) (args!!1)
            where args = words monkey
    in createMonkey $ getMonkey "root"

yell:: Monkey -> Int
yell (Yell _ i) = i
yell (Wait _ m1 m2 "+") = (yell m1) + (yell m2)
yell (Wait _ m1 m2 "-") = (yell m1) - (yell m2)
yell (Wait _ m1 m2 "*") = (yell m1) * (yell m2)
yell (Wait _ m1 m2 "/") = (yell m1) `div` (yell m2)

solve1:: Monkey -> String
solve1 ms = trace (show ms) $ 
            show $ yell ms
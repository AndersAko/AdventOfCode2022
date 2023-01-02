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
    putStrLn (solve1 $ parse1 $ lines input)
    putStrLn (solve2 $ parse2 $ lines input)

data Monkey = Yell String Int | Wait String Monkey Monkey Op deriving Show
type Op = String
type Monkeys = Map String Monkey

parse1:: [String] -> Monkey
parse1 inp = 
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
solve1 ms = "Part 1: " ++ (show $ yell ms)

parse2:: [String] -> Map String String
parse2 s = Map.fromList $ map (\l -> let (name,(':':' ':monkey)) = break (==':') l in (name,monkey)) s
    
solve2 ::  Map String String -> String
solve2 monkeys = 
    let getMonkey :: String -> String
        getMonkey name = fromJust $ Map.lookup name monkeys

        createAndYell humn root= 
            let 
                createMonkey:: String -> String -> Monkey 
                createMonkey name monkey
                    | name == "humn" = trace ("Humn: " ++ show humn) $ Yell name humn
                    | (length $ words monkey) == 1 = Yell name (read monkey)
                    | otherwise = Wait name (createMonkey (args!!0) $ getMonkey (args!!0)) (createMonkey (args!!2) $ getMonkey (args!!2)) (args!!1)
                    where args = words monkey
            in yell $ createMonkey root $ getMonkey root 

        rootChilds = [(words (getMonkey "root")) !! i | i<- [0,2]]

        humn1 = map (createAndYell 0) rootChilds
        humn2 = map (createAndYell 10) rootChilds
        targetNum = if (humn1 !! 0 == humn2 !! 0 ) then humn1 !! 0 else humn1 !! 1
        changingMonkey = if (humn1 !! 0 == humn2 !! 0 ) then rootChilds !! 1 else rootChilds !! 0

        findHumn :: Int -> Int -> Int  -> Int
        findHumn humn0 humn1 root0 
            | root0 == targetNum = humn1
            | humn0 == humn1 = trace ("Didn't find it, got " ++ show root0 ++ " at " ++ show humn0) humn0
            | otherwise = 
                let root1 = createAndYell humn1 changingMonkey
                    humn2 = humn1 + (humn1 - humn0) * (targetNum - root1) `div` (root1 - root0)
                in findHumn humn1 humn2 root1
        solution = findHumn 0 10 (if (humn1 !! 0 == humn2 !! 0 ) then humn1 !! 1 else humn1 !! 0)


    in trace (show humn1 ++ " " ++ show humn2 ++ " " ++ show rootChilds ++ "\nTarget number = " ++ show targetNum ++ " for monkey " ++ show changingMonkey ++ "\n" ++
            show (createAndYell solution (rootChilds!!0)) ++ " == " ++ show (createAndYell solution (rootChilds !! 1))) $
        "Part 2: " ++ show (solution)

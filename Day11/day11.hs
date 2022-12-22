{-# LANGUAGE MultiWayIf #-}

import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char
import Debug.Trace

main = do 
    putStrLn solve1
    putStrLn solve2
    -- input <- readFile "input0.txt"
    -- putStrLn . solve1 $ lines input
    -- putStrLn . solve2 $ lines input

data Operation = Add Int | Multiply Int | Square
data Monkey = Monkey { operation:: Operation, divTest:: Int, trueMonkey:: Int, falseMonkey:: Int }

-- TODO: Parse input file.
-- Test data:
-- monkeys =  [
--         Monkey { operation = Multiply 19, divTest = 23, trueMonkey = 2, falseMonkey = 3 },
--         Monkey { operation = Add 6, divTest = 19, trueMonkey = 2, falseMonkey = 0 },
--         Monkey { operation = Square, divTest = 13, trueMonkey = 1, falseMonkey = 3 },
--         Monkey { operation = Add 3, divTest = 17, trueMonkey = 0, falseMonkey = 1 }
--     ]
-- initial = [[79,98], [54,65,75,74], [79,60,97], [74]]

-- Actual input: 
monkeys = [
    -- 0
    Monkey { operation = Multiply 13, divTest = 7, trueMonkey = 1, falseMonkey = 5 },
    Monkey { operation = Square, divTest = 3, trueMonkey = 3, falseMonkey = 5 },
    Monkey { operation = Add 7, divTest = 2, trueMonkey = 0, falseMonkey = 4 },
    Monkey { operation = Add 4, divTest = 11, trueMonkey = 7, falseMonkey = 6 },
    -- 4
    Monkey { operation = Multiply 19, divTest = 17, trueMonkey = 1, falseMonkey = 0 },
    Monkey { operation = Add 3, divTest = 5, trueMonkey = 7, falseMonkey = 3 },
    Monkey { operation = Add 5, divTest = 13, trueMonkey = 4, falseMonkey = 2 },
    Monkey { operation = Add 1, divTest = 19, trueMonkey = 2, falseMonkey = 6 }
    ]
initial = [[91, 58, 52, 69, 95, 54], [80, 80, 97, 84], [86, 92, 71], [96, 90, 99, 76, 79, 85, 98, 61], 
            [60, 83, 68, 64, 73], [96, 52, 52, 94, 76, 51, 57], [75], [83, 75]]

solve1 :: String
solve1 =
    let round :: ([[Int]],[Int]) -> ([[Int]],[Int])
        round (state,_) = foldl ( \(acc,inspections) mix -> 
            (processMonkey mix acc, inspections ++ [length (acc !! mix)] ) ) (state,[]) [0..length monkeys-1]

        processMonkey :: Int -> [[Int]] -> [[Int]]
        processMonkey monkeyIx state = 
            foldl ( \acc worry -> 
                let newMonkey :: Int
                    (newMonkey, newWorry) = throw (monkeys !! monkeyIx) worry
                in map (\ix -> if 
                                | ix == monkeyIx -> (delete worry (acc !! monkeyIx))
                                | ix == newMonkey -> ((acc !! ix) ++ [newWorry])
                                | otherwise -> (acc !! ix)
                    ) [0..length monkeys-1]
                ) state (state !! monkeyIx)

        throw :: Monkey -> Int -> (Int, Int)
        throw (Monkey operation divTest trueMonkey falseMonkey) worry = 
            let newWorry = case operation of
                        Add n -> worry + n
                        Multiply n -> worry * n
                        Square -> worry * worry
                    `div` 3
                newMonkey = if (newWorry `mod` divTest) == 0 then trueMonkey else falseMonkey
            in (newMonkey, newWorry)
        results = take 21 $ iterate round (initial, []) 
        inspections = map sum $ transpose $ map snd results
    in intercalate "\n" (map show results) ++ "\n" ++ show inspections ++ " => " ++ show (product $ take 2 $ reverse $ sort inspections)

solve2 :: String
solve2 =
    let round :: ([[Int]],[Int]) -> ([[Int]],[Int])
        round (state,_) = foldl ( \(acc,inspections) mix -> 
            (processMonkey mix acc, inspections ++ [length (acc !! mix)] ) ) (state,[]) [0..length monkeys-1]

        processMonkey :: Int -> [[Int]] -> [[Int]]
        processMonkey monkeyIx state = 
            foldl ( \acc worry -> 
                let newMonkey :: Int
                    (newMonkey, newWorry) = throw (monkeys !! monkeyIx) worry
                in map (\ix -> if 
                                | ix == monkeyIx -> (delete worry (acc !! monkeyIx))
                                | ix == newMonkey -> ((acc !! ix) ++ [newWorry])
                                | otherwise -> (acc !! ix)
                    ) [0..length monkeys-1]
                ) state (state !! monkeyIx)

        throw :: Monkey -> Int -> (Int, Int)
        throw (Monkey operation divTest trueMonkey falseMonkey) worry = 
            let newWorry = case operation of
                        Add n -> worry + n
                        Multiply n -> worry * n
                        Square -> worry * worry
                    `mod` (2*3*5*7*11*13*17*19)
                newMonkey = if (newWorry `mod` divTest) == 0 then trueMonkey else falseMonkey
            in (newMonkey, newWorry)
        results = take 10001 $ iterate round (initial, []) 
        inspections = map sum $ transpose $ map snd results
    in intercalate "\n" (map show results) ++ "\n" ++ show inspections ++ " => " ++ show (product $ take 2 $ reverse $ sort inspections)


import Data.List

main = readFile "input.txt" >>= print . solve . parse

parse :: String -> [String]
parse = lines

solve :: [String] -> String
--solve xs = show $ countCalories1 0 [] xs

countCalories1 :: Int -> [String] -> [String] -> Int
countCalories1 maxSoFar _ [] = maxSoFar
countCalories1 maxSoFar collected (x:xs) =
  countCalories1 (max maxSoFar (sumCollected newCollected)) newCollected xs
  where newCollected = if x == [] then [] else x:collected

sumCollected xs = sum $ map read xs

solve xs = show $ countCalories2 [] [] xs

countCalories2 :: [Int] -> [String] -> [String] -> Int
--countCalories2 elfCalories _ [] = sum $ take 3 $ reverse $ sort elfCalories
countCalories2 elfCalories collected [] = sum $ take 3 $ reverse $ sort $ (sumCollected collected):elfCalories
countCalories2 elfCalories collected (x:xs)
  | x == [] = countCalories2 ((sumCollected collected):elfCalories) [] xs
  | otherwise = countCalories2 elfCalories (x:collected) xs

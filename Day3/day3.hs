import Data.List
import Data.Char

main = do 
    readFile "input.txt" >>= print . solve1 . parse
    readFile "input.txt" >>= print . solve2 . parse

parse :: String -> [String]
parse = lines

priority :: Char -> Int
priority x = if isUpper x then ord x - ord 'A' + 27 else ord x - ord 'a' + 1 

------ part 1
solve1 :: [String] -> String
solve1 xs = show $ sum $ map sharedItemPrio xs

split :: String -> [String]
split x = 
    let len = (length x) `div` 2
    in [take len x, drop len x]
     
sharedItemPrio :: String -> Int
sharedItemPrio ruck = 
    let compartments = splitAt (length ruck `div` 2 ) ruck
        common = find (`elem` (snd compartments)) $ fst compartments
    in 
    case common of 
        Just a -> priority a

------- part 2
solve2 :: [String] -> String
solve2 xs = show $ sum $ badges [] xs

badges :: [Int] -> [String] -> [Int]
badges bs [] = bs
badges bs rucks = 
    let badge = head $ intersect (head rucks) $ intersect (rucks !! 1) (rucks !! 2) 
    -- let badge = head [x | x <- [x | x <- head rucks, elem x (rucks !! 1)], elem x (rucks !! 2)]
    in badges ((priority badge) : bs) (drop 3 rucks) 
        

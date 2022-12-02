import Data.List

main = readFile "input.txt" >>= print . solve . parse

parse :: String -> [[String]]
parse = map (words) . lines

solve :: [[String]] -> String
solve xs = show $ sum $ map score2 xs


{- Score round: ["A" "X"] =>
A X 4
A Y 8
A Z 3
B X 1
B Y 5
B Z 9
C X 7
C Y 2
C Z 6
-}

score :: [String] -> Int
score (they:me:_) = myScore + winScore
  where myScore = case me of
            "X" -> 1
            "Y" -> 2
            "Z" -> 3
        winScore = (if they == "A" && me == "Y" || they == "B" && me == "Z" || they == "C" && me == "X" then 6 else
             if they == "A" && me == "Z" || they == "B" && me == "X" || they == "C" && me == "Y" then 0 else 3)


score2 :: [String] -> Int
score2 (they:result:_) = myScore + winScore
 where me = case result of
          "X" -> case they of "A" -> "C"; "B" -> "A"; "C" -> "B"
          "Y" -> they
          "Z" -> case they of "A" -> "B"; "B" -> "C"; "C" -> "A"
       myScore = (case me of "A" -> 1; "B" -> 2; "C" -> 3)
       winScore = (if they == "A" && me == "B" || they == "B" && me == "C" || they == "C" && me == "A" then 6 else
            if they == "A" && me == "C" || they == "B" && me == "A" || they == "C" && me == "B" then 0 else 3)

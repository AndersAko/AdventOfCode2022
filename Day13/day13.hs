{-# LANGUAGE MultiWayIf #-}
import Data.Char 
import Data.List
import Data.Maybe
import Debug.Trace

main = do 
    input <- readFile "input.txt"
    putStrLn (show $ solve1 1 $ parse $ lines input)
    putStrLn (solve2 $ parse $ lines input)

data Element = List [Element] | Single Int | None deriving (Eq, Show) 

instance Ord Element where
    compare (Single a) (Single b)   = compare a b
    compare a@(Single _) b@(List _) = compare (List [a]) b
    compare a@(List _) b@(Single _) = compare a (List [b]) 
    compare (List []) (List [])     = EQ
    compare (List []) (List b)      = LT
    compare (List a) (List [])      = GT
    compare (List (a:as)) (List (b:bs)) = let cmp = compare a b in if cmp == EQ then compare (List as) (List bs) else cmp

parseList :: String -> (String,[Element])
parseList [] = ([], [])
parseList (']':s) = (s, [])
parseList (',':s)   = parseList s
parseList s = 
    let (t1,e1) = parseElement s
        (t2,e2) = parseList t1
    in (t2,e1:e2)

parseElement :: String -> (String,Element)
parseElement [] = ([], None)
parseElement ('[':s) =
    let (t, es) = parseList s
    in (t,List es)
parseElement (c:s) 
    | isDigit c     = let (ds,t) = span isDigit s in (t,Single $ read (c:ds))
    | otherwise     = trace ("Something else " ++ show (c:s)) (s,Single (-1))

parse :: [String] -> [Element]
parse i = map (snd . parseElement) i

solve1:: Int -> [Element] -> Int
solve1 _ [] = 0
solve1 ix (None:es)    = solve1 ix es
solve1 ix (e1:e2:es) = -- trace (show ix ++ " - Comparing " ++ show e1 ++ " with " ++ show e2 ++ " => " ++ show (compare e1 e2)) $
                      (if e1 < e2 then ix else 0) + solve1 (ix+1) es

solve2 :: [Element] -> String
solve2 es = 
    let div1 = List [List [Single 2]]
        div2 = List [List [Single 6]]
        sorted = sort $ filter (/= None) $ div1:div2:es
        ix1 = fromJust $ findIndex (== div1) sorted
        ix2 = fromJust $ findIndex (== div2) sorted
    in trace (show sorted ++ "\nDivider indices: " ++ show ix1 ++ " " ++ show ix2) 
        show $ (ix1+1) * (ix2+1)
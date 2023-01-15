{-# LANGUAGE MultiWayIf #-}
import Data.Char 
import Data.List
import qualified Data.Set as Set 
import Data.Set (Set) 
import qualified Data.Map as Map 
import Data.Map (Map) 
import Data.Maybe
import Debug.Trace
-- import qualified Data.Text as T

main = do 
    input <- readFile "input.txt"
    putStrLn (solve1 input)
    putStrLn (solve2 input)

type Block = [(Int, Int)]
type Chamber = Set (Int, Int)

blocks :: [Block]
blocks = map (\block -> 
                let sizeX = length (block !! 0)
                    sizeY = length block
                in [(y-sizeY+1,x)| x<-[0..sizeX - 1], y<-[0..sizeY - 1], (block!!y)!!x=='#' ]
            )
     [["####"], [" # ", "###", " # "], ["  #", "  #", "###"], ["#","#","#","#"], ["##", "##"]]


-- next block
-- all blocks

applyJet:: Chamber -> Block -> Char -> Block
applyJet chamber block jet =
    let dx = if 
            | jet == '<' -> -1
            | jet == '>' ->  1
        tryBlock = positionedBlock block dx 0
        hitBorder = any (\(_,x) -> (x<0 || x >= 7)) tryBlock
        hitPreviousBlock = not $ Set.disjoint (Set.fromList tryBlock) chamber
    in --trace ("applyJet: " ++ [jet] ++ " " ++ show hitBorder ++ " " ++ show hitPreviousBlock ) $ 
    if hitBorder || hitPreviousBlock then block else tryBlock

positionedBlock:: Block -> Int -> Int -> Block
positionedBlock block dx dy = map (\(y,x) -> (y+dy,x+dx)) block

 
-- Let one block fall until it stops, return resulting chmber and remaining jets
fallBlock :: Block -> String -> Chamber -> (Chamber, String)
fallBlock block (j:jets) chamber = 
    let blownBlock = applyJet chamber block j
        droppedBlock = positionedBlock blownBlock 0 1
        hitBottom =  not $ Set.disjoint (Set.fromList droppedBlock) chamber
    in  if hitBottom then 
            -- trace ("-hit bottom-" ++ show blownBlock)
            (Set.union chamber (Set.fromList blownBlock), jets) 
        else -- trace ("-keep falling-" ++ show droppedBlock)
            fallBlock droppedBlock jets chamber
    -- drop block 1 step until it hits something

-- Drop a new block 
dropBlock :: (Chamber,String) -> Block -> (Chamber,String)
dropBlock (chamber,jets) block =
    let (startY,_) = (Set.findMin chamber)
        newBlock = positionedBlock block 2 (startY-4)
  in --trace ("Dropping new block " ++ show block ++ " => " ++ show newBlock ++ " jets: " ++ (take 10 jets) ++ "...") 
    fallBlock newBlock jets chamber

showChamber:: Chamber -> String
showChamber chamber = 
    let minY = minimum $ map fst $ Set.toAscList chamber
        maxY = maximum $ map fst $ Set.toAscList chamber
        row y = [if Set.member (y,x) chamber then '#' else '.'| x <- [0..6]]
    in concat [ show y ++ "|" ++ row y ++ "|\n" | y <- [(minY-1)..(maxY+1)] ]

rockBottom = Set.fromList [(0,x) | x<-[0..7]] 

solve1:: String -> String
solve1 jets = 
    let falling = scanl dropBlock (rockBottom, cycle jets) (take 2022 $ cycle blocks)
        (height,_) = Set.findMin $ fst $ last falling
    in -- trace (intercalate "\n\n" $ take 200 $ map (\(c,j) -> (showChamber c) ++ "\nJets: " ++ take 100 j) falling)
    "Part 1: height = " ++ show height

solve2:: String -> String
solve2 jets = 
    let falling = scanl dropBlock (rockBottom, cycle jets) (cycle blocks)
        getHeight (chamber, nxtJets) = abs $ fst $ Set.findMin chamber
        reference = falling !! 1000
        repeats = filter (\((chamber, js), index) -> (take 100 $ snd reference) `isPrefixOf` js && index `mod` (length blocks) == 0 ) $ zip falling [0..]
        difference :: [(Int,Int)] -> [(Int,Int)]
        difference ((h1,i1):y@(h2,i2):xs) = (h2-h1,i2-i1) : difference (y:xs)
        heights = map (\(c,ix) -> (getHeight c, ix) ) repeats
        repeatBlocks = snd $ head $ difference heights
        heightRepeat = fst $ head $ difference heights
        trillion = 1000000000000 - (snd (head heights)) -- 1000000000000
        heightRest = (getHeight $ falling !! ((snd $ head heights) + trillion `rem` repeatBlocks)) - fst (head heights)
        height1000000000000 = (fst $ head heights) + (trillion `div` repeatBlocks) * heightRepeat + heightRest
    in  trace (show (take 40 heights)  ++ "\n" ++ show (take 40 $ difference heights) ++ "\n" ++
                "repeatBlocks: " ++ show repeatBlocks ++ " heightRepeat: " ++ show heightRepeat ++ " heightRest: " ++ show heightRest)
        "Part 2: height = " ++ show height1000000000000

    
    
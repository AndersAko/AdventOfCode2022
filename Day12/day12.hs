import Data.List
import Data.Char
import qualified Data.Set as Set 
import Data.Set (Set) 
import Debug.Trace

main = do 
    input <- readFile "input.txt"
    trace (show $ lines input)  putStrLn $ show (solve (lines input) (0,20) )
    putStrLn $ solve2 $ lines input

-- startPos = (0,0)
-- endPos = (5,2)
-- startPos = (0,20)
endPos = (88,20)

type Position = (Int,Int) -- (x,y)
data State = State Int [Position] deriving (Eq, Show)        -- distance, path, with current loc first
instance Ord State where
    compare (State a _) (State b _ ) = compare a b

type Queue = [State]

data Dir = MoveUp | MoveLeft | MoveDown | MoveRight deriving (Eq, Show, Enum)

insertInQueue :: Queue -> State -> Queue
insertInQueue [] s = [s]
insertInQueue (q:qs) s = if s < q then s:q:qs else q:(insertInQueue qs s    )

solve :: [String] -> Position ->  Int
solve hill startPos  =
    let sizeX                       = length $ hill !! 0
        sizeY                       = length hill
        loc (x,y)                   = hill !! y !! x
        passable x y c              = ord (loc (x,y)) <= (ord c) + 1 || c == 'S'
        tryAdd moves x y c          = if passable x y c then (x,y):moves else moves 
        possibleMoves position@(x,y)   = foldl (possibleMove (x,y)) [] [MoveUp .. MoveRight]
        possibleMove (x,y) moves MoveLeft  = if x > 0 then tryAdd moves (x-1) y (loc (x,y)) else moves
        possibleMove (x,y) moves MoveRight = if x < sizeX - 1 then tryAdd moves (x+1) y (loc (x,y)) else moves
        possibleMove (x,y) moves MoveUp    = if y > 0 then tryAdd moves x (y-1) (loc (x,y)) else moves
        possibleMove (x,y) moves MoveDown  = if y < sizeY - 1 then tryAdd moves x (y+1) (loc (x,y)) else moves
        toState :: [Position] -> Position -> State
        toState path here = 
            -- length of path to here + manhattan distance to target
            let distanceEst = length path + abs (fst endPos - fst here) + abs (snd endPos - snd here)
            in State distanceEst (here:path)
        processQueue :: [State] -> Set Position -> [Position]
        processQueue ((State score this):qs) visited = 
--            trace ("processQ: " ++ (show this) ++ " " ++ (show qs) ++ " visited: " ++ (show visited)) $
            let (here:path) = this in
--            trace (show here ++ " " ++ show score ++ " " ++ (loc here):" " ++ show (length this) ++ " " ++ show (length qs) ) $
            if here == endPos then this else
            if Set.member here visited then processQueue qs visited else 
                let newQueue = foldl insertInQueue qs newMoves
                    newMoves = map (toState this) (filter (not . (`Set.member` visited)) $ possibleMoves here)
                    newVisited = Set.insert here visited
                in  --trace ("newMoves: " ++ show newMoves)
                processQueue newQueue newVisited 
        processQueue [] visited = trace ("No path possible, queue empty " ++ show visited) []
        -- processQueue _ visited = trace ("Visited: " ++ show visited ) []
        -- processQueue _ _ = trace ("Really empty") []
        pathToClimb = processQueue [ State 1000000 [startPos] ] Set.empty
    in  trace ("Start: " ++ show startPos ++ " " ++ show (loc startPos) ++ "  End: " ++ show endPos ++ " " ++ show (loc endPos) ++ "\n"  
        ++ "Solution: " ++ show (reverse pathToClimb) ++ " in " ++ show (length pathToClimb - 1 ) ++ " steps") $
        if pathToClimb == [] then maxBound else length pathToClimb - 1

solve2:: [String] -> String
solve2 hill = 
    let sizeX                       = length $ hill !! 0
        sizeY                       = length hill
        loc (x,y)                   = hill !! y !! x
        startPos                    = [(x,y) | x <- [0..sizeX-1], y <- [0..sizeY-1], loc(x,y) == 'a']
    in trace (show startPos) $
        show $ minimum ( map (\p -> solve hill p ) startPos )


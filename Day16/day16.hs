{-# LANGUAGE MultiWayIf #-}
import Data.Char 
import qualified Data.Set as Set 
import Data.Set (Set) 
import qualified Data.Map as Map
import Data.Map.Strict (Map, (!?))
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Sequence (Seq(..), (<|), (|>), (><))
import Data.Sequence (Seq (Empty, (:<|), (:|>)))
import qualified Data.Sequence as Seq
import Data.List
import Data.Maybe
import Debug.Trace

main = do 
    input <- readFile "input.txt"
    putStrLn (solve1 $ parse $ lines input)
    putStrLn (solve2 $ parse $ lines input)

--- Parse input 
parse:: [String] -> [Room]
parse [] = []
parse (l:ls) = 
    let ws = words l
    in (Room { name = ws !! 1, flow = readInt (ws !! 4 ), tunnelsTo = map (filter isLetter) $ drop 9 ws }):parse ls

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c (x:xs) | x == c = splitOn c xs
splitOn c s = (takeWhile (/= c) s):splitOn c (dropWhile (/= c) s)

readInt :: String -> Int
readInt = read . filter isNumber 

----- Domain common
type Domain = [Room]
data Room  = Room { name:: String, flow :: Int, tunnelsTo :: [String]  } deriving (Show)
data State = State { room:: [String], minute:: Int, openValves :: [String], accFlow :: Int, maxFlow :: Int } deriving (Show, Eq, Ord)

flowFromOpenValves :: Domain -> [String] -> Int
flowFromOpenValves rooms openValves = sum $ map (flow) $ filter (flip elem openValves . name) rooms

-- Part 1 Domain
initialState1 = State { room = ["AA"], minute = 0, openValves = [], accFlow = 0, maxFlow = 0 } -- maxFlow doesn't matter, since it will be the only entry in queue
isEndState1 :: State -> Bool
isEndState1 State { minute = minute } = minute >= 30

calcMaximumFlow1 :: Domain -> State -> Int
calcMaximumFlow1 rooms state@(State room minute openValves accFlow _) =
    let flowFromOpenValves' = flowFromOpenValves rooms openValves
        calcPossibleFlow:: Int -> [Int]  -> Int
        calcPossibleFlow _ [] = 0
        calcPossibleFlow 0 _ = 0
        calcPossibleFlow 1 _ = 0
        calcPossibleFlow minutes (x:xs) = x * (minutes - 1) + calcPossibleFlow (minutes-2) xs 
        maxFlowFromUnopenedValves = calcPossibleFlow (30-minute) $ reverse $ sort $ map (flow) $ filter (not . flip elem openValves . name) rooms
    in accFlow + flowFromOpenValves'*(30 - minute) + maxFlowFromUnopenedValves

possibleMoves1 :: Domain -> State -> [State]
possibleMoves1 rooms state@(State (room':_) minute' openValves' accFlow' _) = 
    let currentRoom :: Room
        currentRoom = case find ((== room') . name) rooms of
            Just r -> r
        nextState = state { minute = minute'+1, accFlow = accFlow' + flowFromOpenValves rooms openValves', maxFlow = calcMaximumFlow1 rooms nextState }
    -- Go to adjoining rooms
    in (map (\r -> nextState {room = [r] }) $ tunnelsTo currentRoom) ++
    -- Open valve in current room
    if (flow currentRoom > 0 && not (elem room' openValves'))
    then (
        let st::State = nextState { openValves=(room':openValves'), maxFlow = calcMaximumFlow1 rooms st} in [st] )
    else []

-- Part 2 domain
initialState2 = State { room = ["AA","AA"], minute = 0, openValves = [], accFlow = 0, maxFlow = 0 } -- maxFlow doesn't matter, since it will be the only entry in queue
isEndState2 :: Domain -> State -> Bool
isEndState2 rooms State { minute = minute, openValves = openValves } = 
    let allOpen = sort $ map name $ filter ((>0) . flow) rooms
    in minute >= 26 || allOpen == sort openValves 

calcMaximumFlow2 :: Domain -> State -> Int
calcMaximumFlow2 rooms state@(State room minute openValves accFlow _) =
    let flowFromOpenValves' = flowFromOpenValves rooms openValves
        calcPossibleFlow:: Int -> [Int]  -> Int
        calcPossibleFlow _ [] = 0
        calcPossibleFlow 0 _ = 0
        calcPossibleFlow 1 _ = 0
        calcPossibleFlow minutes (x1:x2:xs) = (x1+x2) * (minutes - 1) + calcPossibleFlow (minutes-2) xs 
        calcPossibleFlow minutes (x:xs) = x * (minutes - 1) + calcPossibleFlow (minutes-2) xs 
        maxFlowFromUnopenedValves = calcPossibleFlow (26-minute) $ reverse $ sort $ map (flow) $ filter (not . flip elem openValves . name) rooms
    in accFlow + flowFromOpenValves'*(26 - minute) + maxFlowFromUnopenedValves

data Move = Move String | Open String | Noop deriving (Show, Eq)
possibleMoves2 :: Domain -> State -> [State]
possibleMoves2 rooms state@(State rooms' minute' openValves' accFlow' _) = 
    let currentRooms :: [Room]
        currentRooms = map (\mr -> fromJust $ find ((== mr) . name) rooms) rooms'
        nextState = state { minute = minute'+1, accFlow = accFlow' + flowFromOpenValves rooms openValves', maxFlow = calcMaximumFlow2 rooms nextState }
        moves = map (\r -> (if flow r > 0 && not (elem (name r) openValves') then Open (name r) else Noop) : (map Move $ tunnelsTo r)) currentRooms
        
        applyMove :: (Move,Move) -> Maybe State
        applyMove (Move x,Move y) = Just nextState { room = [x,y] }
        applyMove (Move x,Open y) = 
            let next = nextState { room = [x,y], openValves =y:openValves', maxFlow = calcMaximumFlow2 rooms next }
            in Just next
        applyMove (Open x,Move y) = 
            let next = nextState { room = [x,y], openValves =x:openValves', maxFlow = calcMaximumFlow2 rooms next }
            in Just next
        applyMove (Open x,Open y) 
            | x == y = let next = nextState { openValves =x:openValves', maxFlow = calcMaximumFlow2 rooms next  }
                        in Just next
            | otherwise = let next = nextState { openValves =x:y:openValves', maxFlow = calcMaximumFlow2 rooms next  }
                        in Just next
        applyMove (Noop,_) = Nothing
        applyMove (_,Noop) = Nothing

    in -- trace (show moves ++ "\n" ++ show [applyMove (x,y) | x <-moves!!0, y <- moves !! 1])
        mapMaybe id [applyMove (x,y) | x <-moves!!0, y <- moves !! 1]


--- Solver using A* search
type Queue = IntMap [State]
type Visited = Map ([String],[String]) State
solve:: (State -> [State]) -> State -> (State -> Bool) -> State
solve possibleMoves initialState isEndState = 
    let 
        insertInQueue :: Visited -> Queue -> State -> Queue
        insertInQueue visited queue s@State{ maxFlow = newStateMaxFlow }  
            | isWorse s visited         = -- trace ("Rejecting insert of item " ++ show s)
                                            queue
            | otherwise                 = 
                let sameScoreStates = case IntMap.lookup newStateMaxFlow queue of
                                        Just sameScore  -> (s:sameScore)
                                        Nothing         -> [s]
                in IntMap.insert newStateMaxFlow sameScoreStates queue 

        -- Is the new state equal or worse than previously visited ?
        key state = (sort $ room state, sort $ openValves state)
        isWorse :: State -> Visited -> Bool
        isWorse state visited =
            case visited !? (key state) of
                Nothing -> False
                Just best -> maxFlow state <= maxFlow best 

        processQueue :: Queue -> Visited -> Int -> State
        processQueue queue visited counter =
            let (score, highestScoreStates) = fromJust (IntMap.lookupMax queue)
                state = head highestScoreStates
                qs = if length highestScoreStates == 1 then IntMap.delete score queue else IntMap.insert score (tail highestScoreStates) queue
                newMoves = possibleMoves state
                newQueue = foldl (insertInQueue visited) qs newMoves
                processNext = if 
                    | isEndState state              -> state
                    | isWorse state visited         -> -- trace ("Discarding " ++ show state) $ 
                                                        processQueue qs visited (counter+1)
                    | otherwise                     -> processQueue newQueue (Map.insert (key state) state visited) (counter+1)
            in  if counter `mod` 5000 == 0 then 
                trace (show (length qs) ++ " " ++ show (maxFlow state) ++ " newMoves from state: " ++ show state ++ " => " ++ show newMoves) processNext
                else processNext
        --processQueue Map.Empty _ counter= trace ("No path possible, queue empty. " ++ show counter ++ " iterations") State ["Error"] 0 [] 0 0 
    in processQueue (IntMap.singleton 0 [initialState]) Map.empty 0


-- Solve part 1
solve1:: [Room] -> String
solve1 rooms = 
    let solution = solve (possibleMoves1 rooms) initialState1 isEndState1 
    in "Solution part 1: " ++ show solution

-- Solve part 2
solve2:: [Room] -> String
solve2 rooms = 
    let solution = solve (possibleMoves2 rooms) initialState2 (isEndState2 rooms)
    in "Solution part 2: " ++ show solution

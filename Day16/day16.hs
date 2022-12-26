import Data.Char 
import qualified Data.Set as Set 
import Data.Set (Set) 
import Data.List
import Debug.Trace

main = do 
    input <- readFile "input.txt"
    putStrLn (solve $ parse $ lines input)

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

-- map input to dict: "XX": flow, [tunnels]
-- possibleMoves = open, move
-- state = position, [open valves], flow = + Sum(open flow)

----- Domain
type Domain = [Room]
data Room = Room { name:: String, flow :: Int, tunnelsTo :: [String]  } deriving (Show)
data State = State { room:: String, minute:: Int, openValves :: [String], accFlow :: Int, maxFlow :: Int } deriving (Show, Eq, Ord)
-- instance Eq State where 
--     (==) a@State{openValves = oa} b@State{openValves = ob} = (==) (a { openValves = sort oa }) (b { openValves = sort ob })
-- instance Ord State where 
--     compare a b = compare a { openValves = sort (openValves a)}
--                           b { openValves = sort (openValves b)}

maxMinutes = 30

flowFromOpenValves :: Domain -> [String] -> Int
flowFromOpenValves rooms openValves = sum $ map (flow) $ filter (flip elem openValves . name) rooms

calcPossibleFlow:: Int -> [Int]  -> Int
calcPossibleFlow _ [] = 0
calcPossibleFlow 0 _ = 0
calcPossibleFlow 1 _ = 0
calcPossibleFlow minutes (x:xs) = x * (minutes - 1) + calcPossibleFlow (minutes-2) xs 

calcMaximumFlow :: Domain -> State -> Int
calcMaximumFlow rooms state@(State room minute openValves accFlow _) =
    let flowFromOpenValves' = flowFromOpenValves rooms openValves
        maxFlowFromUnopenedValves = calcPossibleFlow (maxMinutes-minute) $ reverse $ sort $ map (flow) $ filter (not . flip elem openValves . name) rooms
    in accFlow + flowFromOpenValves'*(maxMinutes - minute) + maxFlowFromUnopenedValves

isEndState :: State -> Bool
isEndState State { minute = minute } = minute >= maxMinutes

initialState = State { room = "AA", minute = 0, openValves = [], accFlow = 0, maxFlow = 0 } -- maxFlow doesn't matter, since it will be the only entry in queue

-- TODO maxmimumFlow = current flow + flow rate * remaining time + "triangular" flow of opening all unopened valves every 2 minutes ( +1 comp for valve in current room) 
possibleMoves :: Domain -> State -> [State]
possibleMoves rooms state@(State room' minute' openValves' accFlow' _) = 
    let currentRoom :: Room
        currentRoom = case find ((== room') . name) rooms of
            Just r -> r
        nextState = state { minute = minute'+1, accFlow = accFlow' + flowFromOpenValves rooms openValves', maxFlow = calcMaximumFlow rooms nextState }
    -- Go to adjoining rooms
    in (map (\r -> nextState {room = r }) $ tunnelsTo currentRoom) ++
 -- Open valve in current room
    if (flow currentRoom > 0 && not (elem room' openValves'))
    then (
        let st::State = nextState { openValves=(room':openValves'), maxFlow = calcMaximumFlow rooms st} in [st] )
    else []
    
--- A* search
type Queue = [State]
insertInQueue :: Set State -> Queue -> State -> Queue
insertInQueue _ [] s = [s]
insertInQueue visited queue@(q:qs) s  
    | isVisited s visited       = queue
    | otherwise                 = 
        let existing = Set.filter (\v -> room s == room v && sort (openValves s) == sort (openValves v) && (accFlow s <= accFlow v && minute s >= minute v)) visited
        in if not $ null existing then trace ("Insert in queue rejected: " ++ show s ++ " is worse than " ++ show existing) queue
        else insertInQueue' queue s

insertInQueue' :: Queue -> State -> Queue
insertInQueue' [] s = [s]
insertInQueue' queue@(q:qs) s  
    | (maxFlow s) > (maxFlow q) = s:queue
    | otherwise                 = q:(insertInQueue' qs s)

isVisited :: State -> Set State -> Bool
isVisited state visited
    | Set.member state visited      = True
    | otherwise                     = False

processQueue :: Domain -> Queue -> Set State -> State
processQueue rooms (state:qs) visited 
    | isEndState state              = state
    | isVisited  state visited      = --trace ("Discarding " ++ show state) $ 
                                        processQueue rooms qs visited
    | otherwise                     =
        let newMoves = possibleMoves rooms state
            newQueue = foldl (insertInQueue visited) qs newMoves
        in  trace (show (length qs) ++ " newMoves from state: " ++ show state ++ " => " ++ show newMoves)
        processQueue rooms newQueue (Set.insert state visited)
processQueue _ [] _ = trace ("No path possible, queue empty ") State "Error" 0 [] 0 0 

--- Solve
solve:: [Room] -> String
solve rooms = 
    let solution = processQueue rooms [initialState] Set.empty
    in trace (show rooms ++ " ") $ 
    "Solution part 1: " ++ show solution



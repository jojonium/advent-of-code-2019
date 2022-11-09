import System.Environment
import FsHelpers
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (catMaybes, fromJust, listToMaybe)
import Data.List (foldl', intercalate, minimumBy, maximumBy)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day15.txt"
fileNameFromArgs (x:_) = x

type Coord = (Integer, Integer)
data State = State {
  stIp         :: Integer,   -- instruction pointer
  stVisited    :: Map.Map Coord Integer,
  stXY         :: Coord,     -- current x,y coordinates
  stDir        :: Integer,   -- current direction
  stRb         :: Integer,   -- relative base
  stFull       :: [Integer],
  stO2Coords   :: Maybe Coord
} deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let nums       = map (read :: String -> Integer) (split (==',') (head ls)) ++ [0, 0..]
      finalState = solve nums
      finalMap   = stVisited finalState
      goal       = fromJust (stO2Coords finalState)
  --putStrLn $ prettyPrint finalMap goal
  putStrLn $ "Part 1: " ++ show (finalMap Map.! goal)
  putStrLn $ "Part 2: " ++ show (part2 finalMap goal)

replace :: Integer -> a -> [a] -> [a]
replace index newVal xs = as ++ newVal : tail bs
  where (as,bs) = splitAt (fromInteger index) xs

param :: Integer -> Integer -> Integer -> [Integer] -> Integer
param 0 p _  full = full !! fromInteger p         -- position mode
param 1 p _ _     = p                             -- immediate mode
param 2 p rb full = full !! fromInteger (rb + p)  -- relative mode
param pm _ _ _    = error $ "Invalid param mode " ++ show pm

move :: Coord -> Integer -> Coord
move (x, y) 1 = (x, y - 1) -- north
move (x, y) 2 = (x, y + 1) -- south
move (x, y) 3 = (x - 1, y) -- west
move (x, y) 4 = (x + 1, y) -- east
move _ d = error $ "Illegal direction: " ++ show d

foldFunc :: Integer -> State -> [State] -> Integer -> [State]
foldFunc index s@(State {stIp=ip, stFull = full, stVisited = visited}) states d = 
  let full'    = replace index d full
      visited' = foldl' (Map.unionWith min) visited (map stVisited states)
  in run (s {stIp = ip + 2, stDir = d, stFull = full', stVisited = visited'}) : states

anyJust :: [Maybe a] -> Maybe a
anyJust ms = listToMaybe (catMaybes ms)

run :: State -> State
run s@(State ip visited (x, y) dir rb full o2)
  | p0 `mod` 100 == 1 = replaceAt a3 (a1 + a2)  -- add
  | p0 `mod` 100 == 2 = replaceAt a3 (a1 * a2)  -- multiply
  | p0 `mod` 100 == 3 =                         -- input
    let nextDirs = filter (\d -> Map.notMember (move (x, y) d) visited) [1..4]
        index    = if p0 `div` 100 `mod` 10 == 2 then rb + p1 else p1
        states   = foldl' (foldFunc index s) [] nextDirs
        merged   = foldl' (Map.unionWith min) visited (map stVisited states)
        newO2    = anyJust (o2 : map stO2Coords states)
    in s {stVisited = merged, stO2Coords = newO2}
  | p0 `mod` 100 == 4 =                         -- output
    if a1 == 0 
         then run (s {stIp = ip + 2, stVisited = Map.insert next 9999 visited})
    else if a1 == 1 
         then let steps = visited Map.! (x, y) + 1
                  newV  = Map.insert next steps visited
              in run (s {stIp = ip + 2, stXY = next, stVisited = newV})
    else if a1 == 2   -- found the oxygen system
         then let steps = visited Map.! (x, y) + 1
                  newV  = Map.insert next steps visited
              in run (s {stIp = ip + 2, stXY = next, stVisited = newV, stO2Coords = Just next})
    else error $ "Illegal status code " ++ show a1
  | p0 `mod` 100 == 5 = continue (if a1 /= 0 then a2 else ip + 3) -- jump-if-true
  | p0 `mod` 100 == 6 = continue (if a1 == 0 then a2 else ip + 3) -- jump-if-false
  | p0 `mod` 100 == 7 = replaceAt a3 (if a1 < a2 then 1 else 0)   -- less-than
  | p0 `mod` 100 == 8 = replaceAt a3 (if a1 == a2 then 1 else 0)  -- equals
  | p0 `mod` 100 == 9 = run (s {stIp = ip + 2, stRb = rb + a1})   -- adjust rb
  where (p0,p1,p2,p3)  = extract (drop (fromInteger ip) full)
        extract (a:b:c:d:_) = (a, b, c, d)
        extract _           = error "This is impossible" -- full memory is infinite
        a1                  = param (p0 `div` 100 `mod` 10) p1 rb full
        a2                  = param (p0 `div` 1000 `mod` 10) p2 rb full
        a3                  = if (p0 `div` 10000 `mod` 10) == 2 then rb + p3 else p3
        continue newIp      = run (s {stIp = newIp})
        replaceAt i val     = run (s {stIp = ip + 4, stFull = replace i val full})
        next                = move (x, y) dir
run s = s -- failure to match halts

solve :: [Integer] -> State
solve memory = run
  (State { stIp = 0
         , stVisited = Map.singleton (0, 0) 0
         , stXY = (0, 0)
         , stDir = 3
         , stRb = 0
         , stFull = memory
         , stO2Coords = Nothing
  })

prettyPrint :: Map.Map Coord Integer -> Coord -> String
prettyPrint visited goal = intercalate "\n" ls
  where list = Map.keysSet visited
        minX = fst $ minimumBy (\(a, _) (b, _) -> a `compare` b) list
        minY = snd $ minimumBy (\(_, a) (_, b) -> a `compare` b) list
        maxX = fst $ maximumBy (\(a, _) (b, _) -> a `compare` b) list
        maxY = snd $ maximumBy (\(_, a) (_, b) -> a `compare` b) list
        toChar coord
          | coord == (0, 0)          = 'O'
          | coord == goal            = 'X'
          | Map.member coord visited = if visited Map.! coord == 9999 then '#' else '.'
          | otherwise                = ' '
        ls = [[toChar (x, y) | x <- [minX..maxX]] | y <- [minY..maxY]]

part2 :: Map.Map Coord Integer -> Coord -> Integer
part2 finalMap start = dfs finalMap (Set.singleton start) start 0
  where dfs fm seen (x, y) steps
          | null open = steps
          | otherwise = maximum next
          where adjacent = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                openFunc c = Set.notMember c seen && Map.findWithDefault 9999 c fm /= 9999
                open = filter openFunc adjacent
                next = map (\c -> dfs fm (Set.insert c seen) c (steps + 1)) open

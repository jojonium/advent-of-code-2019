import System.Environment
import FsHelpers
import qualified Data.Map as Map
import Data.List (intercalate)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day13.txt"
fileNameFromArgs (x:_) = x

type Screen = Map.Map (Integer, Integer) Integer
data Direction = Up | Right | Down | Left
  deriving (Show)

-- arg1, arg2, arg3, ip, full -> (newRemaining, newFull)
type Instruction = (Integer, Integer, Integer, Int, [Integer]) -> ([Integer], [Integer])

data State = State {
  stRem        :: [Integer],
  stIp         :: Integer,  -- instruction pointer
  stScreen     :: Screen,
  stOutput     :: [Integer],
  stRb         :: Integer,  -- relative base
  stFull       :: [Integer]
} deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let nums = map (read :: String -> Integer) (split (==',') (head ls)) ++ [0, 0..]
  putStrLn $ "Part 1: " ++ show (part1 nums)
  putStrLn "Screen:"
  putStrLn $ part2 nums

replace :: Integer -> a -> [a] -> [a]
replace index newVal xs = as ++ newVal : tail bs
  where (as,bs) = splitAt (fromInteger index) xs

param :: Integer -> Integer -> Integer -> [Integer] -> Integer
param 0 p _  full = full !! fromInteger p         -- position mode
param 1 p _ _     = p                             -- immediate mode
param 2 p rb full = full !! fromInteger (rb + p)  -- relative mode
param pm _ _ _    = error $ "Invalid param mode " ++ show pm

run :: State -> State
run s@(State (99:_) _ _ _ _ _) = s -- halt
run s@(State (p0:p1:remaining) ip screen output rb full)
  -- | p0 `mod` 100 == 3 =  -- input
    -- let index        = if p0 `div` 100 `mod` 10 == 2 then rb + p1 else p1
        -- newFull      = replace index (Map.findWithDefault 0 (x, y) hull) full
        -- newRemaining = drop (fromInteger newIp) newFull
    -- in run (s {stRem = newRemaining, stIp = newIp, stFull = newFull})
  | p0 `mod` 100 == 4 = if length output == 2
                        then let painted = Map.insert (head output, output !! 1) a1 screen
                             in run (s {stRem = remaining , stIp = newIp , stOutput = [] , stScreen = painted})
                        else run (s {stRem = remaining, stIp = newIp, stOutput = output ++ [a1]})
  | p0 `mod` 100 == 9 = run (s {stRem = remaining, stIp = newIp, stRb = rb + a1}) -- adjust rb
  where a1    = param (p0 `div` 100 `mod` 10) p1 rb full
        newIp = ip + 2
run s@(State (p0:p1:p2:_) ip _ _ rb full) 
  | p0 `mod` 100 == 5 = continue (if a1 /= 0 then a2 else ip + 3) -- jump-if-true
  | p0 `mod` 100 == 6 = continue (if a1 == 0 then a2 else ip + 3) -- jump-if-false
  where a1 = param (p0 `div` 100 `mod` 10) p1 rb full
        a2 = param (p0 `div` 1000 `mod` 10) p2 rb full
        continue newIp = run (s {stRem = drop (fromInteger newIp) full, stIp = newIp})
run s@(State (p0:p1:p2:p3:_) ip _ _ rb full)
  | p0 `mod` 100 == 1 = continue addInstruction    -- add
  | p0 `mod` 100 == 2 = continue multInstruction   -- multiply
  | p0 `mod` 100 == 7 = continue ltInstruction     -- less-than
  | p0 `mod` 100 == 8 = continue equalsInstruction -- equals
  where a1    = param (p0 `div` 100 `mod` 10) p1 rb full
        a2    = param (p0 `div` 1000 `mod` 10) p2 rb full
        a3    = if (p0 `div` 10000 `mod` 10) == 2 then rb + p3 else p3
        newIp = ip + 4
        continue instruction = run (s {stRem = nr, stIp = newIp, stFull = nf})
          where (nr, nf) = instruction (a1, a2, a3, fromInteger newIp, full)
-- failure to match halts
run s@(State {}) = s

addInstruction :: Instruction
addInstruction (a1, a2, a3, delta, full) = 
  let newFull = replace a3 (a1 + a2) full
  in (drop delta newFull, newFull) 

multInstruction :: Instruction
multInstruction (a1, a2, a3, delta, full) = 
  let newFull = replace a3 (a1 * a2) full
  in (drop delta newFull, newFull) 

ltInstruction :: Instruction
ltInstruction (a1, a2, a3, delta, full) = 
  let newFull = replace a3 (if a1 < a2 then 1 else 0) full
  in (drop delta newFull, newFull) 

equalsInstruction :: Instruction
equalsInstruction (a1, a2, a3, delta, full) = 
  let newFull = replace a3 (if a1 == a2 then 1 else 0) full
  in (drop delta newFull, newFull) 

solve :: [Integer] -> Screen -> Screen
solve memory initScreen = stScreen $ run
  (State { stRem = memory
        , stIp = 0
        , stScreen = initScreen
        , stOutput = []
        , stRb = 0
        , stFull = memory
  })

printScreen :: Screen -> String
printScreen screen = intercalate "\n" strings
  where lst        = Map.toList screen
        xs         = map (fst . fst) lst
        ys         = map (snd . fst) lst
        strings    = map step [(minimum ys)..(maximum ys)]
        step y     = map (`toChar` y) [(minimum xs) .. (maximum xs)]
        toChar x y = case Map.findWithDefault 0 (x, y) screen of
                     1 -> '#'
                     2 -> 'B'
                     3 -> '_'
                     4 -> 'o'
                     _ -> ' '

part1 :: [Integer] -> Int
part1 instructions = length $ filter (\(_, x) -> x == 2) (Map.toList $ solve instructions Map.empty)

part2 :: [Integer] -> String
part2 instructions = printScreen $ solve (2 : tail instructions) Map.empty

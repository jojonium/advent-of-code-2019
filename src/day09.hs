import System.Environment
import FsHelpers

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day09.txt"
fileNameFromArgs (x:_) = x

-- arg1, arg2, arg3, ip, full -> (newRemaining, newFull)
type Instruction = (Integer, Integer, Integer, Int, [Integer]) -> ([Integer], [Integer])
data State = State {
  stRem        :: [Integer],
  stIp         :: Integer,  -- instruction pointer
  stInput      :: [Integer],
  stOutput     :: [Integer],
  stRb         :: Integer,  -- relative base
  stFull       :: [Integer]
} deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let nums = map (read :: String -> Integer) (split (==',') (head ls)) ++ [0, 0..]
  putStrLn $ "Part 1: " ++ show (head $ part1 nums)
  putStrLn $ "Part 2: " ++ show (head $ part2 nums)

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
run (State (p0:p1:remaining) ip input output rb full)
  | p0 `mod` 100 == 3 =  -- input
    let index        = if p0 `div` 100 `mod` 10 == 2 then rb + p1 else p1
        newFull      = replace index (head input) full
        newRemaining = drop (fromInteger newIp) newFull
    in run (State newRemaining newIp (tail input) output rb newFull)
  | p0 `mod` 100 == 4 = run (State remaining newIp input (output ++ [a1]) rb full) -- output
  | p0 `mod` 100 == 9 = run (State remaining newIp input output (rb + a1) full) -- adjust rb
  where a1    = param (p0 `div` 100 `mod` 10) p1 rb full
        newIp = ip + 2
run s@(State (p0:p1:p2:_) ip _ _ rb full) 
  | p0 `mod` 100 == 5 = continue (if a1 /= 0 then a2 else ip + 3) -- jump-if-true
  | p0 `mod` 100 == 6 = continue (if a1 == 0 then a2 else ip + 3) -- jump-if-false
  where a1 = param (p0 `div` 100 `mod` 10) p1 rb full
        a2 = param (p0 `div` 1000 `mod` 10) p2 rb full
        continue newIp = run (s {stRem = drop (fromInteger newIp) full, stIp = newIp})
run (State (p0:p1:p2:p3:_) ip input output rb full)
  | p0 `mod` 100 == 1 = continue addInstruction    -- add
  | p0 `mod` 100 == 2 = continue multInstruction   -- multiply
  | p0 `mod` 100 == 7 = continue ltInstruction     -- less-than
  | p0 `mod` 100 == 8 = continue equalsInstruction -- equals
  where a1    = param (p0 `div` 100 `mod` 10) p1 rb full
        a2    = param (p0 `div` 1000 `mod` 10) p2 rb full
        a3    = if (p0 `div` 10000 `mod` 10) == 2 then rb + p3 else p3
        newIp = ip + 4
        continue instruction = run (State nr newIp input output rb nf)
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

solve :: [Integer] -> [Integer] -> [Integer]
solve memory input = stOutput $ run (State memory 0 input [] 0 memory)

part1 :: [Integer] -> [Integer]
part1 instructions = solve instructions [1]

part2 :: [Integer] -> [Integer]
part2 instructions = solve instructions [2]

import System.Environment
import FsHelpers
import Debug.Trace (trace)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day17.txt"
fileNameFromArgs (x:_) = x

data State = State {
  stIp     :: Integer,   -- instruction pointer
  stOutput :: [Integer],
  stRb     :: Integer,   -- relative base
  stMem    :: [Integer]
} deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let nums = map (read :: String -> Integer) (split (==',') (head ls)) ++ [0, 0..]
  --putStrLn $ toAscii $ stOutput (solve nums)
  putStrLn $ "Part 1: " ++ show (part1 nums)

replace :: Integer -> a -> [a] -> [a]
replace index newVal xs = as ++ newVal : tail bs
  where (as,bs) = splitAt (fromInteger index) xs

param :: Integer -> Integer -> Integer -> [Integer] -> Integer
param 0 p _  full = full !! fromInteger p         -- position mode
param 1 p _ _     = p                             -- immediate mode
param 2 p rb full = full !! fromInteger (rb + p)  -- relative mode
param pm _ _ _    = error $ "Invalid param mode " ++ show pm

run :: State -> State
run s@(State ip output rb memory)
  | p0 `mod` 100 == 1 = replaceAt a3 (a1 + a2)  -- add
  | p0 `mod` 100 == 2 = replaceAt a3 (a1 * a2)  -- multiply
  | p0 `mod` 100 == 3 =                         -- input
    let index    = if p0 `div` 100 `mod` 10 == 2 then rb + p1 else p1
        inputVal = 0
    in run (s {stIp = ip + 2, stMem = replace index inputVal memory})
  | p0 `mod` 100 == 4 =                         -- output
     run (s {stIp = ip + 2, stOutput = output ++ [a1]})
  | p0 `mod` 100 == 5 = continue (if a1 /= 0 then a2 else ip + 3) -- jump-if-true
  | p0 `mod` 100 == 6 = continue (if a1 == 0 then a2 else ip + 3) -- jump-if-false
  | p0 `mod` 100 == 7 = replaceAt a3 (if a1 < a2 then 1 else 0)   -- less-than
  | p0 `mod` 100 == 8 = replaceAt a3 (if a1 == a2 then 1 else 0)  -- equals
  | p0 `mod` 100 == 9 = run (s {stIp = ip + 2, stRb = rb + a1})   -- adjust rb
  where (p0,p1,p2,p3)  = extract (drop (fromInteger ip) memory)
        extract (a:b:c:d:_) = (a, b, c, d)
        extract _           = error "This is impossible" -- full memory is infinite
        a1                  = param (p0 `div` 100 `mod` 10) p1 rb memory
        a2                  = param (p0 `div` 1000 `mod` 10) p2 rb memory
        a3                  = if (p0 `div` 10000 `mod` 10) == 2 then rb + p3 else p3
        continue newIp      = run (s {stIp = newIp})
        replaceAt i val     = run (s {stIp = ip + 4, stMem = replace i val memory})
run s = s -- failure to match halts

solve :: [Integer] -> State
solve memory = run s
  where s = State {stIp = 0 , stOutput = [] , stRb = 0 , stMem = memory}

part1 :: [Integer] -> Int
part1 instructions = sum intersects
  where ls = (filter (\l -> length l > 1) . lines . toAscii . stOutput . solve) instructions
        ys = [1..(length ls - 2)]
        xs = [1..(length (head ls) - 2)]
        isIntersect x y = take 3 (drop (x - 1) (ls !! y)) == "###" &&
          ls !! (y + 1) !! x == '#' && ls !! (y - 1) !! x == '#'
        intersects = [x * y | x <- xs, y <- ys, isIntersect x y]

toAscii :: [Integer] -> String
toAscii = map (toEnum . fromInteger)

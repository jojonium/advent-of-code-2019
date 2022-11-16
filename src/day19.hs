import System.Environment
import FsHelpers

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day19.txt"
fileNameFromArgs (x:_) = x

data BeamState = Before | Inside
data State = State {
  stIp     :: Integer,   -- instruction pointer
  stInput  :: [Integer],
  stOutput :: [Integer],
  stRb     :: Integer,   -- relative base
  stMem    :: [Integer]
} deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let nums = map (read :: String -> Integer) (split (==',') (head ls)) ++ [0, 0..]
  putStrLn $ "Part 1: " ++ show (part1 nums)
  putStrLn $ "Part 2: " ++ show (part2 nums)

replace :: Integer -> a -> [a] -> [a]
replace index newVal xs = as ++ newVal : tail bs
  where (as,bs) = splitAt (fromInteger index) xs

param :: Integer -> Integer -> Integer -> [Integer] -> Integer
param 0 p _  full = full !! fromInteger p         -- position mode
param 1 p _ _     = p                             -- immediate mode
param 2 p rb full = full !! fromInteger (rb + p)  -- relative mode
param pm _ _ _    = error $ "Invalid param mode " ++ show pm

run :: State -> State
run s@(State ip input output rb memory)
  | p0 `mod` 100 == 1 = replaceAt a3 (a1 + a2)  -- add
  | p0 `mod` 100 == 2 = replaceAt a3 (a1 * a2)  -- multiply
  | p0 `mod` 100 == 3 =                         -- input
    let index    = if p0 `div` 100 `mod` 10 == 2 then rb + p1 else p1
        inputVal = head input
    in run (s {stIp = ip + 2, stMem = replace index inputVal memory, stInput = tail input})
  | p0 `mod` 100 == 4 = run (s {stIp = ip + 2, stOutput = output ++ [a1]}) -- output
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

query :: [Integer] -> Integer -> Integer -> Integer
query memory x y = (head . stOutput) (run State
  { stIp = 0
  , stOutput = []
  , stInput = [x, y]
  , stRb = 0
  , stMem = memory
  })

part1 :: [Integer] -> Integer
part1 memory = (toInteger . length . filter (==1)) [ query memory x y | x <- [0..49], y <- [0..49] ]

-- beam starts at (5, 6) for some reason
part2 :: [Integer] -> Integer
part2 memory = step memory Inside 1100 1100 1200

step :: [Integer] -> BeamState -> Integer -> Integer -> Integer -> Integer
step memory Before _ x y  -- before beam
  | cur == 1  = step memory Inside x x y
  | otherwise = step memory Before (x + 1) (x + 1) y
  where cur = query memory x y
step memory Inside startX x y  -- within beam
  | not tl = step memory Before startX startX (y + 1) -- exited beam
  | not tr = step memory Before startX startX (y + 1) -- not wide enough at top
  | not bl = step memory Inside startX (x + 1) y -- bottom isn't in beam
  | otherwise = x * 10000 + y
  where tl = query memory x y == 1
        tr = query memory (x + 99) y == 1
        bl = query memory x (y + 99) == 1


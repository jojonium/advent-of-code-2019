import System.Environment
import FsHelpers
import qualified Data.Map as Map

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day23.txt"
fileNameFromArgs (x:_) = x

data Computer = Computer 
  { _ip     :: Integer   -- instruction pointer
  , _input  :: [Integer]
  , _output :: [Integer]
  , _rb     :: Integer   -- relative base
  , _mem    :: [Integer]
  , _idle   :: Bool
  } deriving (Show)

data Packet = Packet
  { _dest :: Integer
  , _x    :: Integer
  , _y    :: Integer
  } deriving (Show)

data NetworkState = NetworkState
  { _computers  :: Map.Map Integer Computer
  , _natYs      :: [Integer]
  , _natPacket  :: Packet
  }

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let nums = map (read :: String -> Integer) (split (==',') (head ls)) ++ [0, 0..]
  putStrLn $ "Part 1: " ++ show (part1 nums)
  putStrLn $ "Part 2 (this may take a while): " ++ show (part2 nums)

replace :: Integer -> a -> [a] -> [a]
replace index newVal xs = as ++ newVal : tail bs
  where (as,bs) = splitAt (fromInteger index) xs

param :: Integer -> Integer -> Integer -> [Integer] -> Integer
param 0 p _  full = full !! fromInteger p         -- position mode
param 1 p _ _     = p                             -- immediate mode
param 2 p rb full = full !! fromInteger (rb + p)  -- relative mode
param pm _ _ _    = error $ "Invalid param mode " ++ show pm

runOne :: Computer -> Computer
runOne s@(Computer ip input output rb memory _)
  | p0 `mod` 100 == 1 = replaceAt a3 (a1 + a2)  -- add
  | p0 `mod` 100 == 2 = replaceAt a3 (a1 * a2)  -- multiply
  | p0 `mod` 100 == 3 =                         -- input
    let index    = if p0 `div` 100 `mod` 10 == 2 then rb + p1 else p1
        (inputValue, newInput, isIdle) = 
          if null input then (-1, [], True) else (head input, tail input, False)
    in s {_ip = ip + 2, _mem = replace index inputValue memory, _input = newInput, _idle = isIdle}
  | p0 `mod` 100 == 4 = s {_ip = ip + 2, _output = output ++ [a1], _idle = False} -- output
  | p0 `mod` 100 == 5 = continue (if a1 /= 0 then a2 else ip + 3) -- jump-if-true
  | p0 `mod` 100 == 6 = continue (if a1 == 0 then a2 else ip + 3) -- jump-if-false
  | p0 `mod` 100 == 7 = replaceAt a3 (if a1 < a2 then 1 else 0)   -- less-than
  | p0 `mod` 100 == 8 = replaceAt a3 (if a1 == a2 then 1 else 0)  -- equals
  | p0 `mod` 100 == 9 = s {_ip = ip + 2, _rb = rb + a1}   -- adjust rb
  where (p0,p1,p2,p3)  = extract (drop (fromInteger ip) memory)
        extract (a:b:c:d:_) = (a, b, c, d)
        extract _           = error "This is impossible" -- full memory is infinite
        a1                  = param (p0 `div` 100 `mod` 10) p1 rb memory
        a2                  = param (p0 `div` 1000 `mod` 10) p2 rb memory
        a3                  = if (p0 `div` 10000 `mod` 10) == 2 then rb + p3 else p3
        continue newIp      = s {_ip = newIp}
        replaceAt i val     = s {_ip = ip + 4, _mem = replace i val memory}
runOne s = s -- failure to match halts

networkStep :: Map.Map Integer Computer -> Integer
networkStep computers
  | not (null winners) = _y (head winners)
  | otherwise          = networkStep (Map.map runOne delivered)
  where packets   = Map.foldr extractP [] computers
        winners   = filter (\p -> _dest p == 255) packets
        cleanedO  = Map.map cleanOutput computers
        delivered = foldr deliver cleanedO packets
        extractP (Computer {_output = [dest, x, y]}) ps = Packet dest x y : ps
        extractP _ ps = ps
        cleanOutput c@(Computer {_output = [_, _, _]}) = c {_output = []}
        cleanOutput c = c
        deliver (Packet dest x y) m = Map.insert dest (c { _input = inp ++ [x, y]}) m
          where c   = m Map.! dest
                inp = _input c

networkStep2 :: NetworkState -> Integer
networkStep2 (NetworkState computers natYs np)
  | length nYs >= 2 && head nYs == nYs !! 1 = head nYs
  | otherwise = networkStep2 newState
  where packets      = Map.foldr extractP [] computers
        cleanedO     = Map.map cleanOutput computers
        delivered    = foldr deliver cleanedO packets
        toNat        = filter (\p -> _dest p == 255) packets
        np'          = if not (null toNat) then last toNat else np
        (final, nYs) = if all _idle computers && null packets && all (null . _input) computers
                       then (deliver (np' { _dest = 0 }) delivered, _y np' : natYs)
                       else (delivered, natYs)
        newState  = NetworkState (Map.map runOne final) nYs np'
        extractP (Computer {_output = [dest, x, y]}) ps = Packet dest x y : ps
        extractP _ ps = ps
        cleanOutput c@(Computer {_output = [_, _, _]}) = c {_output = []}
        cleanOutput c = c
        deliver (Packet 255 _ _) m  = m
        deliver (Packet dest x y) m = Map.insert dest (c { _input = inp ++ [x, y]}) m
          where c   = m Map.! dest
                inp = _input c

part1 :: [Integer] -> Integer
part1 memory = networkStep initNetwork
  where initNetwork = foldr computerBuilder Map.empty [0..49]
        computerBuilder i = Map.insert i (Computer 0 [i] [] 0 memory False)

part2 :: [Integer] -> Integer
part2 memory = networkStep2 (NetworkState initNetwork [] (Packet 0 0 0))
  where initNetwork = foldr computerBuilder Map.empty [0..49]
        computerBuilder i = Map.insert i (Computer 0 [i] [] 0 memory False)

import System.Environment
import FsHelpers
import Data.Char (chr, ord)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (intercalate, isInfixOf)
import Data.Maybe (catMaybes)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day25.txt"
fileNameFromArgs (x:_) = x

data Computer = Computer 
  { _ip     :: Integer   -- instruction pointer
  , _input  :: [Integer]
  , _output :: [Integer]
  , _rb     :: Integer   -- relative base
  , _mem    :: Map.Map Integer Integer
  , _ai     :: Bool      -- awaiting input
  } deriving (Show, Ord, Eq)

-- whirled peas
-- ornament (too heavy)
-- dark matter
-- candy cane
-- tambourine
-- astrolabe
-- hologram
-- klein bottle
--
-- correct set: klein bottle, hologram, astrolabe, tambourine

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  myInputLines <- fileToLines "inputs/day25-instructions.txt"
  let myInput      = fromAscii $ intercalate "\n" myInputLines ++ "\n"
      nums         = Map.fromList $ zip [0..] (map read (split (==',') (head ls)))
      allItems     = runInput (Computer 0 myInput [] 0 nums False)
  repl allItems -- just go north to win

dfs :: (Computer, Set.Set String) -> Maybe Computer
dfs (c, is)
  | tooHeavy `isInfixOf` take 1000 output = Nothing
  | tooLight `isInfixOf` take 1000 output = if null successes then Nothing else Just (head successes)
  | otherwise                   = Just cur
  where cur       = runInput (c { _input = fromAscii "north\n", _output = [], _ai = False})
        output    = toAscii $ _output cur
        tooHeavy  = "Droids on this ship are lighter than the detected value"
        tooLight  = "Droids on this ship are heavier than the detected value"
        toNext i  = runInput (cur { _input = fromAscii ("take "++i++"\n"), _output = [], _ai = False})
        nextSteps = Set.map (\i -> (toNext i, Set.delete i is)) is
        successes = catMaybes $ Set.toList $ Set.map dfs nextSteps

runInput :: Computer -> Computer
runInput c
  | _ai c                    = c
  | otherwise = runInput (step c)

getNextInput :: Bool -> [Integer] -> IO [Integer]
getNextInput False is = do return is
getNextInput True  is = do
  nextLine <- getLine
  return $ is ++ fromAscii (nextLine ++ "\n")

repl :: Computer -> IO ()
repl c@(Computer _ input o _ _ ai) = do
  let (toPrint, o') = if not (null o) && last o == 10 then (toAscii o, []) else ("", o)
  putStr toPrint
  nextInput <- getNextInput ai input
  repl $ step (c { _input = nextInput, _output = o' })

param :: Integer -> Integer -> Integer -> Map.Map Integer Integer -> Integer
param 0 p _  full = Map.findWithDefault 0 p full        -- position mode
param 1 p _ _     = p                                   -- immediate mode
param 2 p rb full = Map.findWithDefault 0 (rb + p) full -- relative mode
param pm _ _ _    = error $ "Invalid param mode " ++ show pm

step :: Computer -> Computer
step s@(Computer ip input output rb memory _)
  | p0 `mod` 100 == 1 = replaceAt a3 (a1 + a2)  -- add
  | p0 `mod` 100 == 2 = replaceAt a3 (a1 * a2)  -- multiply
  | p0 `mod` 100 == 3 = if null input then (s { _ai = True })
    else let index      = if p0 `div` 100 `mod` 10 == 2 then rb + p1 else p1
             inputValue = head input
             mem'       = Map.insert index inputValue memory
         in s {_ip = ip + 2, _mem = mem', _input = tail input, _ai = False}
  | p0 `mod` 100 == 4 = s {_ip = ip + 2, _output = output ++ [a1], _ai = False} -- output
  | p0 `mod` 100 == 5 = continue (if a1 /= 0 then a2 else ip + 3) -- jump-if-true
  | p0 `mod` 100 == 6 = continue (if a1 == 0 then a2 else ip + 3) -- jump-if-false
  | p0 `mod` 100 == 7 = replaceAt a3 (if a1 < a2 then 1 else 0)   -- less-than
  | p0 `mod` 100 == 8 = replaceAt a3 (if a1 == a2 then 1 else 0)  -- equals
  | p0 `mod` 100 == 9 = s {_ip = ip + 2, _rb = rb + a1}   -- adjust rb
  where (p0,p1,p2,p3)  = extract (map (\i -> Map.findWithDefault 0 (ip + i) memory) [0..3])
        extract (a:b:c:d:_) = (a, b, c, d)
        extract _           = error "This is impossible" -- full memory is infinite
        a1                  = param (p0 `div` 100 `mod` 10) p1 rb memory
        a2                  = param (p0 `div` 1000 `mod` 10) p2 rb memory
        a3                  = if (p0 `div` 10000 `mod` 10) == 2 then rb + p3 else p3
        continue newIp      = s {_ip = newIp}
        replaceAt i val     = s {_ip = ip + 4, _mem = Map.insert i val memory}
step s = s -- failure to match halts

toAscii :: [Integer] -> String
toAscii = map (chr . fromInteger)

fromAscii :: String -> [Integer]
fromAscii = map (toInteger . ord)

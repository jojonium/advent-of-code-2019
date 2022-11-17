import System.Environment
import FsHelpers
import qualified Data.Map as Map
import Data.List (foldl', elemIndex)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day22.txt"
fileNameFromArgs (x:_) = x

data Deck = Deck 
  { _cards :: [Integer]
  , _length :: Int
  }

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  putStrLn $ "Part 1: " ++ show (elemIndex 2019 (run factoryOrder ls))

factoryOrder :: Deck
factoryOrder = Deck { _cards = [0..10006], _length = 10007 }

dealIntoNewStack :: Deck -> Deck
dealIntoNewStack d = d { _cards = (reverse . _cards) d }

cut :: Int -> Deck -> Deck
cut n d@(Deck c l)
  | n > 0     = d { _cards = drop n c ++ take n c }
  | n < 0     = d { _cards = b ++ a }
  | otherwise = d
  where (a, b) = splitAt (l + n) c

dealWithIncrement :: Int -> Deck -> Deck
dealWithIncrement n d@(Deck cards l) = output
  where dealt      = foldl' f Map.empty (zip cards [0..l])
        f m (c, i) = Map.insert ((i * n) `mod` l) c m
        output     = d { _cards = Map.elems dealt }

run :: Deck -> [String] -> [Integer]
run d []     = _cards d
run d (x:xs) = run (runOne d x) xs
  where runOne deck s
          | func == ["deal", "with"] = dealWithIncrement numArg deck
          | func == ["deal", "into"] = dealIntoNewStack deck
          | head func == "cut"       = cut numArg deck
          | otherwise                = error $ "Failed to parse line: " ++ s
          where ws = words s
                func = take 2 ws
                numArg = read (last ws)

import System.Environment
import FsHelpers
import Data.List (intercalate)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day08.txt"
fileNameFromArgs (x:_) = x

width, height :: Int
width = 25
height = 6

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let pixels = head ls
  putStrLn $ "Part 1: " ++ show (part1 pixels)
  putStrLn "Part 2:"
  prettyPrint (part2 pixels)

prettyPrint :: String -> IO ()
prettyPrint s = do
  let s' = map (\x -> if x == '0' then ' ' else x) s
  putStrLn $ intercalate ['\n'] (chunk width s')
  
-- Split a list into chunk of length x
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk x s  = a : chunk x b
  where (a, b) = splitAt x s

part1 :: String -> Int
part1 pixels = numDigits '1' targetLayer * numDigits '2' targetLayer
  where layers = chunk (width * height) pixels
        numDigits d = length . filter (== d)
        indexedList = zip (map (numDigits '0') layers) [0..(length layers)]
        minIndex = snd $ foldr step (maxBound, -1) indexedList
        step (x, i) elt = if fst elt < x then elt else (x, i)
        targetLayer = layers !! minIndex

part2 :: String -> String
part2 = foldl1 combine . chunk (width * height)
  where combine image layer = zipWith (curry blend) layer image
        blend (a, b) = if b == '2' then a else b

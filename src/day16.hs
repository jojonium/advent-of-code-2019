import System.Environment
import FsHelpers

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day16.txt"
fileNameFromArgs (x:_) = x

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let signal = map (\c -> read [c]) (head ls) :: [Integer]
  putStrLn $ "Part 1: " ++ part1 signal
  putStrLn $ "Part 2 (this will take a long time): " ++ part2 signal

part1 :: [Integer] -> String
part1 input = take 8 $ concatMap show output
  where output = iterate fft input !! 100

fft :: [Integer] -> [Integer]
fft input = map step [1..(length input)]
  where step index = abs total `mod` 10
          where total = sum $ zipWith (*) input (pattern index)
        pattern i = drop 1 $ cycle base'
          where base = [0, 1, 0, -1]
                base' = concatMap (replicate i) base

part2 :: [Integer] -> String
part2 input = concatMap show (take 8 $ iterate bigFft bigInput !! 100)
  where offset    = read $ concatMap show (take 7 input)
        bigInput  = drop offset (concat (replicate 10000 input))

bigFft :: [Integer] -> [Integer]
bigFft = scanr (\d s -> (d + s) `mod` 10) 0

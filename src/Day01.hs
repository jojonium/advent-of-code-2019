import System.Environment
import FsHelpers

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "../inputs/day01.txt"
fileNameFromArgs (x:xs) = x

main :: IO ()
main = do
  args <- getArgs
  nums <- fileToIntegers $ fileNameFromArgs args
  putStrLn $ "Part 1: " ++ show (part1 nums)
  putStrLn $ "Part 2: " ++ show (part2 nums)

part1 :: [Integer] -> Integer
part1 nums = sum (map fuelCost nums)
  where fuelCost x = div x 3 - 2

part2 :: [Integer] -> Integer
part2 nums = sum (map fuelCost nums)
  where fuelCost x
          | x <= 0    = 0
          | otherwise = fuelMass + fuelCost fuelMass
              where fuelMass = max 0 (div x 3 - 2)


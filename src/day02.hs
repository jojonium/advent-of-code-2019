import System.Environment
import FsHelpers

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "../inputs/day02.txt"
fileNameFromArgs (x:_) = x

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let nums = map (read :: String -> Int) (split (==',') (head ls))
  putStrLn $ "Part 1: " ++ show (part1 nums)
  putStrLn $ "Part 2: " ++ show (part2 nums)

replace :: Int -> a -> [a] -> [a]
replace index newVal xs = as ++ newVal : bs
  where (as,_:bs) = splitAt index xs

run :: [Int] -> [Int] -> [Int]
run remaining full
  | opCode == 1  = run (drop 4 remaining) (replace p3 (full!!p1 + full!!p2) full)
  | opCode == 2  = run (drop 4 remaining) (replace p3 (full!!p1 * full!!p2) full)
  | opCode == 99 = full
  where [opCode, p1, p2, p3] = take 4 remaining

solve :: Int -> Int -> [Int] -> Int
solve noun verb memory = head $ run restored restored
    where restored = replace 2 verb (replace 1 noun memory)

part1 :: [Int] -> Int
part1 = solve 12 2

part2 :: [Int] -> Int
part2 nums = head [100 * noun + verb | noun <- [0..99], verb <- [0..99], solve noun verb nums == 19690720]


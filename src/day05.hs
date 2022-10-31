import System.Environment
import FsHelpers
import Debug.Trace (trace)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day05.txt"
fileNameFromArgs (x:_) = x

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let nums = map (read :: String -> Int) (split (==',') (head ls))
  putStrLn $ "Part 1: " ++ show (part1 nums)
  -- putStrLn $ "Part 2: " ++ show (part2 nums)

replace :: Int -> a -> [a] -> [a]
replace index newVal xs = as ++ newVal : tail bs
  where (as,bs) = splitAt index xs

run :: ([Int], [Int], [Int], [Int]) -> ([Int], [Int], [Int], [Int])
run ([], input, output, full) = ([], input, output, full)
run ([_], input, output, full) = ([], input, output, full)
run (99:_, input, output, full) = run ([], input, output, full) -- halt
run (3:p1:remaining, input, output, full)  -- input
  = let delta = length full - length remaining
        newFull = replace p1 (head input) full
    in run (drop delta newFull, drop 1 input, output, newFull)
run (4:p1:remaining, input, output, full)  -- output position mode
  = run (remaining, input, output ++ [full!!p1], full)
run (104:p1:remaining, input, output, full) -- output immediate mode
  = run (remaining, input, output ++ [p1], full)
run ([_, _], _, _, full) = ([], [], [], full)
run ([_, _, _], _, _, full) = ([], [], [], full)
run (p0:p1:p2:p3:remaining, input, output, full)
  | p0 `mod` 100 == 1  = let a1 = if p1m p0 == 0 then full!!p1 else p1
                             a2 = if p2m p0 == 0 then full!!p2 else p2
                             (nr, nf) = addInstruction (a1, a2, p3, remaining, full)
                         in run (nr, input, output, nf)
  | p0 `mod` 100 == 2  = let a1 = if p1m p0 == 0 then full!!p1 else p1
                             a2 = if p2m p0 == 0 then full!!p2 else p2
                             (nr, nf) = multInstruction (a1, a2, p3, remaining, full)
                         in run (nr, input, output, nf)
  | otherwise = error $ "Illegal first instruction: " ++ show p0
  where p1m n = n `div` 100 `mod` 10
        p2m n = n `div` 1000 `mod` 10

addInstruction :: (Int, Int, Int, [Int], [Int]) -> ([Int], [Int])
addInstruction (a1, a2, a3, remaining, full) = 
  let delta = length full - length remaining
      newFull = replace a3 (a1 + a2) full
  in (drop delta newFull, newFull) 

multInstruction :: (Int, Int, Int, [Int], [Int]) -> ([Int], [Int])
multInstruction (a1, a2, a3, remaining, full) = 
  let delta = length full - length remaining
      newFull = replace a3 (a1 * a2) full
  in (drop delta newFull, newFull) 

solve :: [Int] -> [Int] -> ([Int], [Int])
solve input memory = (output, afterMemory)
  where (_, _, output, afterMemory) = run (memory, input, [], memory)

part1 :: [Int] -> Int
part1 nums = last $ fst $ solve [1] nums

import System.Environment
import FsHelpers
import Data.List (permutations)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day07.txt"
fileNameFromArgs (x:_) = x

type Instruction = (Int, Int, Int, [Int], [Int]) -> ([Int], [Int])

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let nums = map (read :: String -> Int) (split (==',') (head ls))
  putStrLn $ "Part 1: " ++ show (part1 nums)
  putStrLn $ "Part 2: " ++ show (part2 nums)

replace :: Int -> a -> [a] -> [a]
replace index newVal xs = as ++ newVal : tail bs
  where (as,bs) = splitAt index xs

-- (remaining, input, output, full) -> (remaining, input, output, full)
run :: ([Int], [Int], [Int], [Int]) -> ([Int], [Int], [Int], [Int])
run (99:_, input, output, full) = run ([], input, output, full) -- halt
run (3:p1:remaining, input, output, full)  -- input
  = let delta = length full - length remaining
        newFull = replace p1 (head input) full
    in run (drop delta newFull, drop 1 input, output, newFull)
run (4:p1:remaining, input, output, full)  -- output position mode
  = run (remaining, input, output ++ [full!!p1], full)
run (104:p1:remaining, input, output, full) -- output immediate mode
  = run (remaining, input, output ++ [p1], full)
run (p0:p1:p2:p3:remaining, input, output, full) = run (nr, input, output, nf)
  where a1 = if p0 `div` 100  `mod` 10 == 0 then full!!p1 else p1
        a2 = if p0 `div` 1000 `mod` 10 == 0 then full!!p2 else p2
        instruction
          | p0 `mod` 100 == 1 = addInstruction
          | p0 `mod` 100 == 2 = multInstruction
          | p0 `mod` 100 == 5 = jitInstruction
          | p0 `mod` 100 == 6 = jifInstruction
          | p0 `mod` 100 == 7 = ltInstruction
          | p0 `mod` 100 == 8 = equalsInstruction
          | otherwise = error $ "Illegal first instruction: " ++ show p0
        (nr, nf) = instruction (a1, a2, p3, remaining, full)
run (_, input, output, full) = ([], input, output, full) -- failure to match halts

addInstruction :: Instruction
addInstruction (a1, a2, a3, remaining, full) = 
  let delta = length full - length remaining
      newFull = replace a3 (a1 + a2) full
  in (drop delta newFull, newFull) 

multInstruction :: Instruction
multInstruction (a1, a2, a3, remaining, full) = 
  let delta = length full - length remaining
      newFull = replace a3 (a1 * a2) full
  in (drop delta newFull, newFull) 

ltInstruction :: Instruction
ltInstruction (a1, a2, a3, remaining, full) = 
  let delta = length full - length remaining
      newFull = replace a3 (if a1 < a2 then 1 else 0) full
  in (drop delta newFull, newFull) 

equalsInstruction :: Instruction
equalsInstruction (a1, a2, a3, remaining, full) = 
  let delta = length full - length remaining
      newFull = replace a3 (if a1 == a2 then 1 else 0) full
  in (drop delta newFull, newFull) 

jitInstruction :: Instruction
jitInstruction (a1, a2, _, remaining, full) = 
  (if a1 /= 0 then drop a2 full else remaining, full)

jifInstruction :: Instruction
jifInstruction (a1, a2, _, remaining, full) = 
  (if a1 == 0 then drop a2 full else remaining, full)

solve :: [Int] -> [Int] -> [Int]
solve memory input = output
  where (_, _, output, _) = run (memory, input, [], memory)

part1 :: [Int] -> Int
part1 instructions = snd $ foldr f ("", 0) combinations
  where combinations = [(a,b,c,d,e) | [a, b, c, d, e] <- permutations [0..4]]
        f combo pair = if snd newPair > snd pair then newPair else pair
          where newPair = thrusterOutput instructions combo

part2 :: [Int] -> Int
part2 instructions = snd $ foldr f ("", 0) combinations
  where combinations = [(a,b,c,d,e) | [a, b, c, d, e] <- permutations [5..9]]
        f combo pair = if snd newPair > snd pair then newPair else pair
          where newPair = thrusterOutput instructions combo

thrusterOutput :: [Int] -> (Int, Int, Int, Int, Int) -> (String, Int)
thrusterOutput instructions (a, b, c, d, e) = (str, last eOutput)
  where eOutput = solve instructions $ e : dOutput
        dOutput = solve instructions $ d : cOutput
        cOutput = solve instructions $ c : bOutput
        bOutput = solve instructions $ b : aOutput
        aOutput = solve instructions $ a : 0 : eOutput
        str = show a ++ show b ++ show c ++ show d ++ show e

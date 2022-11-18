import System.Environment
import FsHelpers
import Data.List (foldl', elemIndex)
import Data.Maybe (fromJust)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day22.txt"
fileNameFromArgs (x:_) = x

data Instruction = DealInto | DealWith Integer | Cut Integer | Identity
  deriving Show

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let deck      = [0..10006]
      len       = 10007
      condensed = condense len (parse ls)
      solvedPart1 = map (\i -> runBackwards len i condensed) deck
  putStrLn $ "Part 1: " ++ show (fromJust (elemIndex 2019 solvedPart1))
  let bigLen       = 119315717514047
      bigCondensed = condense bigLen (parse ls)
      bigInsts     = embiggen bigLen bigCondensed
  putStrLn $ "Part 2: " ++ show (runBackwards bigLen 2020 bigInsts)

runBackwards :: Integer -> Integer -> [Instruction] -> Integer
runBackwards size = foldr (flip (backwardsApply size))

parse :: [String] -> [Instruction]
parse = map readIt
  where readIt s
          | func == ["deal", "with"] = DealWith numArg
          | func == ["deal", "into"] = DealInto
          | head func == "cut"       = Cut numArg
          | otherwise                = error $ "Failed to parse line: " ++ s
          where ws = words s
                func = take 2 ws
                numArg = read (last ws)

-- returns the index of the current inhabitant of the given index before the
-- instruction was applied
backwardsApply :: Integer -> Integer -> Instruction -> Integer
backwardsApply _    i Identity     = i
backwardsApply size i DealInto     = size - 1 - i
backwardsApply size i (Cut n)      = (i + (n `mod` size)) `mod` size
backwardsApply size i (DealWith n) = (i * modinv n size) `mod` size
  where modinv a m = x `mod` m
          where (_, x, _) = egcd a m
        egcd 0 b = (b, 0, 1)
        egcd a b = (g, x - (b `div` a) * y, y)
          where (g, y, x) = egcd (b `mod` a) a

-- repeatedly fold and sort until there is at most one of each instruction type
condense :: Integer -> [Instruction] -> [Instruction]
condense size is
  | length is <= 3 = is
  | otherwise      = condense size (foldAndSort size is)

foldAndSort :: Integer -> [Instruction] -> [Instruction]
foldAndSort size = reverse . foldl' folder []
  where
    folder :: [Instruction] -> Instruction -> [Instruction]
    folder [] x = [x]
    folder (Identity    :is) x            = x:is
    folder (DealInto    :is) DealInto     = is
    folder ((Cut a)     :is) (Cut b)      = Cut ((a + b) `mod` size) : is
    folder ((DealWith a):is) (DealWith b) = DealWith ((a * b) `mod` size) : is
    folder (DealInto    :is) (Cut b)      = DealInto : Cut (size - b) : is
    folder ((Cut a)     :is) (DealWith b) = Cut ((a * b) `mod` size) : DealWith b :  is
    folder (DealInto    :is) (DealWith b) = DealInto : Cut (size + 1 - b)  : DealWith b : is
    folder ((DealWith a):is) (Cut b)      = Cut b : DealWith a : is
    folder ((DealWith a):is) DealInto     = DealInto : DealWith a : is
    folder ((Cut a)     :is) DealInto     = DealInto : Cut a : is
    folder is x = error $ "oops forgot one" ++ show is ++ ", " ++ show x

-- repeats instructions a lot of times and returns the condensed result
embiggen :: Integer -> [Instruction] -> [Instruction]
embiggen size instructions = condense size final
  where repetitions = 101741582076661 :: Integer
        bits        = helper repetitions
        helper 0    = [0]
        helper n    = n `rem` 2 : helper (n `quot` 2)
        instBits    = foldl' (\acc _ -> acc ++ [condense size (last acc ++ last acc)]) [instructions] [2 .. (length bits)]
        final       = foldl' (\acc (is, b) -> if b == 1 then acc ++ is else acc) [] (zip instBits bits)


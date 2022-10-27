import System.Environment
import FsHelpers
import qualified Data.Set as Set

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "../inputs/day03.txt"
fileNameFromArgs (x:_) = x

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let instructions = map (split (==',')) ls
  putStrLn $ "Part 1: " ++ show (part1 instructions)

part1 :: [[String]] -> Int
part1 []    = error "part1: invalid input"
part1 [[]]  = error "part1: invalid input"
part1 [_:_] = error "part1: invalid input"
part1 (wire1:wire2:_) = minimum (Set.map (\ (x, y) -> abs x + abs y) sect) 
  where sect = occupiedPoints wire1 `Set.intersection` occupiedPoints wire2

occupiedPoints :: [String] -> Set.Set (Int, Int)
occupiedPoints inst = points
  where (_, _, points) = foldl move (0, 0, Set.empty) inst

move :: (Int, Int, Set.Set (Int, Int)) -> String -> (Int, Int, Set.Set (Int, Int))
move (x, y, _) [] = (x, y, Set.empty)
move (x, y, points) (i:is)
  | i == 'U' = (x, y + amount, points `Set.union` Set.fromList [(x, y + y1) | y1 <- [1 .. amount]])
  | i == 'D' = (x, y - amount, points `Set.union` Set.fromList [(x, y - y1) | y1 <- [1 .. amount]])
  | i == 'R' = (x + amount, y, points `Set.union` Set.fromList [(x + x1, y) | x1 <- [1 .. amount]])
  | i == 'L' = (x - amount, y, points `Set.union` Set.fromList [(x - x1, y) | x1 <- [1 .. amount]])
  | otherwise = error "move: invalid input"
  where amount = read is :: Int

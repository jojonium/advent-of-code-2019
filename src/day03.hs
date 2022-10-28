import System.Environment
import FsHelpers
import qualified Data.Map as Map

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "../inputs/day03.txt"
fileNameFromArgs (x:_) = x

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let instructions = map (split (==',')) ls
  let (p1, p2) = solve instructions
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

solve :: [[String]] -> (Int, Int)
solve []    = error "solve: invalid input"
solve [[]]  = error "solve: invalid input"
solve [_:_] = error "solve: invalid input"
solve (wire1:wire2:_) = (Map.foldrWithKey mm maxBound sect, Map.foldr min maxBound sect)
  where sect = Map.intersectionWith (+) (occupiedPoints wire1) (occupiedPoints wire2)
        mm (x, y) _ = min (abs x + abs y) -- manhattanMin
        occupiedPoints inst = points
          where (_, _, _, points) = foldl move (0, 0, 0, Map.empty) inst

move :: (Int, Int, Int, Map.Map (Int, Int) Int) -> String -> (Int, Int, Int, Map.Map (Int, Int) Int)
move (x, y, steps, _) [] = (x, y, steps, Map.empty)
move (x, y, steps, points) (i:is) = (nx, ny, steps + amount, insertAll points lst)
  where amount        = read is :: Int
        insertAll     = foldr (uncurry (Map.insertWith min))
        (nx, ny, lst) = case i of 
          'U' -> (x, y + amount, [((x, y + y1), steps + y1) | y1 <- [1..amount]])
          'D' -> (x, y - amount, [((x, y - y1), steps + y1) | y1 <- [1..amount]])
          'R' -> (x + amount, y, [((x + x1, y), steps + x1) | x1 <- [1..amount]])
          'L' -> (x - amount, y, [((x - x1, y), steps + x1) | x1 <- [1..amount]])
          _   -> error $ "Invalid move " ++ (i:is)

import System.Environment
import FsHelpers
import qualified Data.Set as Set
import Data.Bifunctor (second)
import Data.Foldable (maximumBy)
import Data.Ratio

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day10.txt"
fileNameFromArgs (x:_) = x

main :: IO ()
main = do
  args <- getArgs
  asteroids <- fileToLines $ fileNameFromArgs args
  let set = toSet asteroids
  let (x, y, z) = solve set
  putStrLn $ "Part 1: " ++ show z ++ " at " ++ show (x, y)

solve :: Set.Set (Int, Int) -> (Int, Int, Int)
solve set = maximumBy (\(_, _, a) (_, _, b) -> a `compare` b) $ solveAll set

solveAll :: Set.Set (Int, Int) -> Set.Set (Int, Int, Int)
solveAll set = Set.map (\p@(x, y) -> (x, y, countVisible p set)) set

toSet :: [[Char]] -> Set.Set (Int, Int)
toSet asteroids = foldr step Set.empty indexes
  where width  = length asteroids
        height  = length $ head asteroids
        indexes = [(a, b) | a <- [0..width-1], b <- [0..height-1]]
        step (x, y) set = if asteroids !! y !! x == '#'
                          then Set.insert (x, y) set
                          else set

trim2 :: [a] -> [a]
trim2 [] = []
trim2 [_] = []
trim2 ls = init (tail ls)

-- List of points between two given points, exclusive of both ends
pointsOnLine :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
pointsOnLine (fromX, fromY) (toX, toY)
  | toX - fromX == 0 = [(fromX, y) | y <- trim2 [(min fromY toY) .. (max fromY toY)]]
  | toY - fromY == 0 = [(x, fromY) | x <- trim2 [(min fromX toX) .. (max fromX toX)]]
  | otherwise = trys
      where startX   = min fromX toX
            endX     = max fromX toX
            slope    = fromIntegral (toY - fromY) % fromIntegral (toX - fromX) :: Ratio Integer
            b        = fromIntegral fromY - (slope * fromIntegral fromX)
            lineEq x = slope * x + b
            doubleP  = [(x, lineEq (fromIntegral x)) | x <- trim2 [startX..endX]]
            filtered = filter (\(_, y) -> denominator y == 1) doubleP
            trys    = map (Data.Bifunctor.second floor) filtered

visibleFrom :: (Int, Int) -> (Int, Int) -> Set.Set (Int, Int) -> Bool
visibleFrom src dest set
  | not $ Set.member dest set = False
  | src == dest               = False
  | otherwise = not $ any (`Set.member` set) $ pointsOnLine src dest

countVisible :: (Int, Int) -> Set.Set (Int, Int) -> Int
countVisible (x, y) set
  | not $ Set.member (x, y) set = 0
  | otherwise = foldr step 0 set
    where step other acc = if visibleFrom (x, y) other set then acc + 1 else acc
            
getVisible :: (Int, Int) -> Set.Set (Int, Int) -> [(Int, Int)]
getVisible (x, y) set
  | not $ Set.member (x, y) set = []
  | otherwise = foldr step [] set
    where step other acc = if visibleFrom (x, y) other set then other:acc else acc

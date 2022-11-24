import System.Environment
import FsHelpers
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl', minimumBy, maximumBy, intercalate)
import Debug.Trace (trace)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day24.txt"
fileNameFromArgs (x:_) = x

type Coord = (Int, Int)
type Chart = Map.Map Coord Space
data Space = Bug | Empty deriving (Eq)

type RecCoord = (Int, Coord) -- layer, (x, y)
type RecChart = Map.Map RecCoord Space

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let chart = parse ls
  putStrLn $ "Part 1: " ++ show (part1 chart)
  putStrLn $ "Part 2: " ++ show (part2 chart)

parse :: [String] -> Chart
parse ls = foldl' folder Map.empty coords
  where ys = [0..(length ls - 1)]
        xs = [0..(length (head ls) - 1)]
        coords = [(x, y) | x <- xs, y <- ys]
        folder m (x, y) = Map.insert (x, y) s m
          where c = ls !! y !! x
                s = if c == '#' then Bug else Empty

prettyPrint :: Chart -> String
prettyPrint chart = intercalate "\n" ls ++ "\n"
  where toChar Bug   = '#'
        toChar Empty = '.'
        ls = [[toChar (chart Map.! (x, y)) | x <- [0..4]] | y <- [0..4]]

transform :: Space -> Int -> Space
transform Bug   1 = Bug
transform Bug   _ = Empty
transform Empty 1 = Bug
transform Empty 2 = Bug
transform Empty _ = Empty

step :: Chart -> Chart
step chart = Map.mapWithKey stepOne chart
  where stepOne (x, y) space = transform space num
          where n   = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                num = length (filter (\c -> Map.findWithDefault Empty c chart == Bug) n)

biodiversityRating :: Chart -> Integer
biodiversityRating chart = output
  where ls        = concat [[chart Map.! (x, y) | x <- [0..4]] | y <- [0..4]]
        output    = sum $ zipWith f ls [0..]
        f Bug n   = 2 ^ n
        f Empty _ = 0

part1 :: Chart -> Integer
part1 c = biodiversityRating $ helper Set.empty c
  where helper record chart
          | Set.member hash record = chart
          | otherwise = helper (Set.insert hash record) (step chart)
          where hash = prettyPrint chart

recStep :: RecChart -> RecChart
recStep chart = Map.mapWithKey stepOne expanded'
  where keys      = Map.keys chart
        maxL      = fst (maximumBy (\(a, _) (b, _) -> a `compare` b) keys)
        minL      = fst (minimumBy (\(a, _) (b, _) -> a `compare` b) keys)
        aLayer    = [(x, y) | x <- [0..4], y <- [0..4]]
        expanded  = foldr (`Map.insert` Empty) chart [(maxL + 1, c) | c <- aLayer]
        expanded' = foldr (`Map.insert` Empty) expanded [(minL - 1, c) | c <- aLayer]
        stepOne (_, (2, 2)) _     = Empty
        stepOne (l, (x, y)) space = transform space num
          where n   = neighbors (l, (x, y))
                num = length $ filter (\rc -> Map.findWithDefault Empty rc chart == Bug) n

neighbors :: RecCoord -> [RecCoord]
neighbors (l, (x, y)) = concat [above, below, toLeft, toRight]
  where above
          | y == 0           = [(l - 1, (2, 1))]
          | x == 2 && y == 3 = [(l + 1, (x', 4)) | x' <- [0..4]]
          | otherwise        = [(l, (x, y - 1))]
        below
          | y == 4           = [(l - 1, (2, 3))]
          | x == 2 && y == 1 = [(l + 1, (x', 0)) | x' <- [0..4]]
          | otherwise        = [(l, (x, y + 1))]
        toLeft
          | x == 0           = [(l - 1, (1, 2))]
          | x == 3 && y == 2 = [(l + 1, (4, y')) | y' <- [0..4]]
          | otherwise        = [(l, (x - 1, y))]
        toRight
          | x == 4           = [(l - 1, (3, 2))]
          | x == 1 && y == 2 = [(l + 1, (0, y')) | y' <- [0..4]]
          | otherwise        = [(l, (x + 1, y))]

part2 :: Chart -> Int
part2 c = count $ iterate recStep recChart !! 200
  where recChart = Map.mapKeys (\n -> (0, n)) c
        count = Map.size . Map.filter (==Bug)

import System.Environment
import FsHelpers
import qualified Data.Map as Map
import qualified Data.HashSet as HS
import Data.Char (isLower, toUpper, isUpper)
import Data.List (foldl', minimumBy, maximumBy, intercalate, permutations)
import Data.Graph.AStar (aStar)
import Data.Maybe (isJust, catMaybes)
import Debug.Trace (trace)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day18.txt"
fileNameFromArgs (x:_) = x

type Coord = (Int, Int)
type Chart = Map.Map Coord Char
type Locs = Map.Map Char Coord

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let (chart, doorLocs, keyLocs, startPos) = parse ls
  putStrLn (prettyPrint chart startPos)
  putStrLn $ "Numkeys = " ++ show (Map.size keyLocs)
  putStrLn $ "Part 1: " ++ show (try2 chart doorLocs keyLocs startPos maxBound 0)

-- returns (full chart, door locations, key locations, start position)
parse :: [String] -> (Chart, Locs, Locs, Coord)
parse ls = foldl' folder (Map.empty, Map.empty, Map.empty, (0, 0)) coords
  where ys = [0..(length ls - 1)]
        xs = [0..(length (head ls) - 1)]
        coords = [(x, y) | x <- xs, y <- ys]
        folder (chart, doorLocs, keyLocs, startPos) (x, y) = (chart', doorLocs', keyLocs', startPos')
          where t         = ls !! y !! x
                t'        = if t == '@' then '.' else t
                startPos' = if t == '@' then (x, y) else startPos
                doorLocs' = if isUpper t then Map.insert t (x, y) doorLocs else doorLocs
                chart'    = Map.insert (x, y) t' chart
                keyLocs'  = if isLower t then Map.insert t (x, y) keyLocs else keyLocs

pathTo :: Chart -> Coord -> Coord -> Maybe [Coord]
pathTo chart from (gx, gy) = aStar graphFunc (\_ _ -> 1) heuristic (==(gx, gy)) from
  where heuristic (x, y) = (abs gx - x) + (abs gy - y)
        graphFunc (x, y) = foldl' isOpen HS.empty adjacent
          where adjacent = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                isOpen hs c
                  | c == (gx, gy)                          = HS.insert c hs
                  | Map.findWithDefault '#' c chart == '.' = HS.insert c hs
                  | otherwise = hs

try2 :: Chart -> Locs -> Locs -> Coord -> Int -> Int -> Int
try2 chart doorLocs keyLocs cur best steps
  | Map.null keyLocs  = steps
  | steps >= best     = maxBound
  | otherwise         = Map.foldlWithKey' folder maxBound options'
  where options               = Map.map (pathTo chart cur) keyLocs
        options'              = Map.filter isJust options
        folder acc _ Nothing  = acc
        folder acc k (Just p) = if length p + steps >= acc then acc else min acc (tryNext k p acc)
        tryNext k path acc    = try2 chart'' doorLocs keyLocs' c acc (steps + length path)
          where c        = last path
                keyLocs' = Map.delete k keyLocs
                chart'   = Map.insert c '.' chart  -- key space is empty now
                chart''  = if Map.member (toUpper k) doorLocs  -- open corresponding door if it exists
                           then Map.insert (doorLocs Map.! toUpper k) '.' chart'
                           else chart'

prettyPrint :: Chart -> Coord -> String
prettyPrint chart start = intercalate "\n" ls ++ "\n"
  where list = Map.keysSet chart
        minX = fst $ minimumBy (\(a, _) (b, _) -> a `compare` b) list
        minY = snd $ minimumBy (\(_, a) (_, b) -> a `compare` b) list
        maxX = fst $ maximumBy (\(a, _) (b, _) -> a `compare` b) list
        maxY = snd $ maximumBy (\(_, a) (_, b) -> a `compare` b) list
        toChar coord
          | coord == start = '@'
          | otherwise      = Map.findWithDefault ' ' coord chart
        ls = [[toChar (x, y) | x <- [minX..maxX]] | y <- [minY..maxY]]

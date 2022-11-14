import System.Environment
import FsHelpers
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.HashSet as HS
import Data.Char (toLower, isLower, toUpper, isUpper)
import Data.List (foldl', minimumBy, maximumBy, intercalate)
import Data.Graph.AStar (aStar)
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace (trace)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day18.txt"
fileNameFromArgs (x:_) = x

type Coord = (Int, Int)
type Chart = Map.Map Coord Char
type Locs = Map.Map Char Coord
type Record = Set.Set Coord

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let (chart, doors, keyLocs, startPos) = parse ls
  putStrLn (prettyPrint chart startPos)
  putStrLn $ "Numkeys = " ++ show (Map.size keyLocs)
  putStrLn $ "Part 1: " ++ show (part1 chart startPos doors keyLocs)

availableMoves :: Chart -> Coord -> Set.Set Char -> Record -> [(Coord, Char)]
availableMoves chart (x, y) keys visited = (filter isOpen . map withTile) adjacent
  where adjacent = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        withTile c = (c, Map.findWithDefault '#' c chart)
        isOpen (c, tile)
          | tile == '.'  = Set.notMember c visited
          | tile == '#'  = False
          | isLower tile = Set.notMember c visited
          | toLower tile `Set.member` keys = Set.notMember c visited
          | otherwise = False

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

unlock :: Chart -> Locs -> Coord -> Char -> Chart
unlock chart doors cur key = chart''
  where chart'  = Map.insert cur '.' chart
        chart''
          | Map.member (toUpper key) doors = Map.insert (doors Map.! toUpper key) '.' chart'
          | otherwise                      = chart'

pathTo :: Chart -> Coord -> Coord -> Maybe [Coord]
pathTo chart from (gx, gy) = aStar graphFunc (\_ _ -> 1) heuristic (==(gx, gy)) from
  where heuristic (x, y) = (abs gx - x) + (abs gy - y)
        graphFunc (x, y) = foldl' isOpen HS.empty adjacent
          where adjacent = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                isOpen hs c
                  | Map.findWithDefault '#' c chart /= '#' = HS.insert c hs
                  | otherwise = hs

try :: Chart -> Coord -> Locs -> Set.Set Char -> Record -> Locs -> Int -> Int
try chart (x, y) doors keys visited keyLocs steps
  | Set.size keys == Map.size keyLocs = steps
  | null options = maxBound
  | otherwise = minimum (map tryNext options)
  where options          = availableMoves chart (x, y) keys visited
        tryNext (c, t)   = try chart' c doors keys' visited' keyLocs (steps + 1)
          where (chart', keys', visited')
                  | isLower t = let chrt   = unlock chart doors c t
                                    path = fromMaybe [] (pathTo chrt c (doors Map.! toUpper t))
                                in ( chrt
                                   , Set.insert t keys
                                   , foldl' (flip Set.delete) visited path)
                  | otherwise = (chart, keys, Set.insert (x, y) visited)

try2 :: Chart -> Locs -> Locs -> Coord -> Set.Set Char -> Int -> Int
try2 chart doorLocs keyLocs (x, y) keys steps
  | Set.size keys == Map.size keyLocs = steps
  | null options                      = maxBound
  | otherwise                         = minimum (map tryNext (trace (show options) options))
  where options = filter validPath (map (\(k, c) -> (k, c, pathTo chart (x, y) c)) (filter stillAround (Map.assocs keyLocs)))
        stillAround (k, c)   = chart Map.! c == k
        validPath (_, _, Nothing) = False
        validPath (k, _, Just path) = all unblocked (trace ("path = " ++ show path) path)
          where unblocked coord = let c = chart Map.! coord in c == '.' || c == toUpper k || isLower c
        tryNext (_, _, Nothing)   = maxBound
        tryNext (_, _, Just [])   = maxBound
        tryNext (k, c, Just path) = try2 chart'' doorLocs keyLocs c keys' (steps + length path)
          where keys'   = Set.insert k keys
                chart'  = Map.insert c '.' chart
                chart'' = if Map.member (toUpper k) doorLocs 
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

part1 :: Chart -> Coord -> Locs -> Locs -> Int
-- part1 chart startPos doors keyLocs = try chart startPos doors Set.empty visited keyLocs 0
  -- where visited = Set.singleton startPos
part1 chart startPos doorLocs keyLocs = try2 chart doorLocs keyLocs startPos Set.empty 0

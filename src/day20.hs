import System.Environment
import FsHelpers
import qualified Data.Map as Map
import qualified Data.HashSet as HS
import Data.List (foldl', minimumBy, maximumBy, intercalate)
import Data.Char (isUpper)
import Data.Maybe (fromJust)
import Algorithm.Search (dijkstra)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day20.txt"
fileNameFromArgs (x:_) = x

type Coord = (Int, Int)
type Chart = Map.Map Coord (HS.HashSet Coord)
type Portals = Map.Map Coord Coord

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let m = parse ls
  let (chart, aa, zz) = toChart m
  putStrLn $ "Part 1: " ++ (show . fst . fromJust) (pathTo chart aa zz)

parse :: [String] -> Map.Map Coord Char
parse ls = foldl' folder Map.empty coords
  where ys = [0..(length ls - 1)]
        xs = [0..(length (head ls) - 1)]
        coords = [(x, y) | x <- xs, y <- ys]
        folder chart (x, y) = Map.insert (x, y) t chart
          where t = ls !! y !! x

toChart :: Map.Map Coord Char -> (Chart, Coord, Coord)
toChart m = (chartWithPortals, aa, zz)
  where (portals, aa, zz) = getPortals m
        plainChart = foldl' folder Map.empty (Map.assocs m)
        folder chart ((x, y), c)
          | c == '.'  = Map.insert (x, y) (foldl' isOpen HS.empty adjacent) chart
          | otherwise = chart
          where adjacent = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                isOpen hs a
                  | aChar == '.' = HS.insert a hs
                  | otherwise    = hs
                  where aChar = Map.findWithDefault '#' a m
        chartWithPortals = Map.foldlWithKey' inserter plainChart portals
          where inserter :: Chart -> Coord -> Coord -> Chart
                inserter acc from to = acc''
                  where acc'  = Map.insertWith HS.union from (HS.singleton to) acc
                        acc'' = Map.insertWith HS.union to (HS.singleton from) acc'

getPortals :: Map.Map Coord Char -> (Portals, Coord, Coord)
getPortals m = (Map.foldl' reconfigure Map.empty finalP, finalAA, finalZZ)
  where (finalP, finalAA, finalZZ) = foldl' folder (Map.empty, (-1, -1), (-1, -1)) (Map.assocs m)
        folder (portals, aa, zz) ((x, y), c)
          | c == '.'  = helper ap
          | otherwise = (portals, aa, zz)
          where adjacent = [ ((x + 1, y), (x + 2, y))
                           , ((x - 1, y), (x - 2, y))
                           , ((x, y + 1), (x, y + 2))
                           , ((x, y - 1), (x, y - 2))]
                apMap (a, b) = (Map.findWithDefault ' ' a m, Map.findWithDefault ' ' b m)
                ap = filter (isUpper . fst) $ map apMap adjacent
                helper [] = (portals, aa, zz)
                helper ((p1, p2):_)
                  | [p1, p2] == "AA" = (portals, (x, y), zz)
                  | [p1, p2] == "ZZ" = (portals, aa, (x, y))
                  | otherwise        = (portals'', aa, zz)
                     where portals'  = Map.insertWith (++) [p1, p2] [(x, y)] portals
                           portals'' = Map.insertWith (++) [p2, p1] [(x, y)] portals'
        reconfigure acc (a:b:_) = Map.insert a b acc
        reconfigure _ _         = error "Not enough coords in list"

pathTo :: Chart -> Coord -> Coord -> Maybe (Int, [Coord])
pathTo chart start goal = dijkstra (chart Map.!) (\_ _ -> 1) (==goal) start

prettyPrint :: Map.Map Coord Char -> String
prettyPrint chart = intercalate "\n" ls ++ "\n"
  where list = Map.keysSet chart
        minX = fst $ minimumBy (\(a, _) (b, _) -> a `compare` b) list
        minY = snd $ minimumBy (\(_, a) (_, b) -> a `compare` b) list
        maxX = fst $ maximumBy (\(a, _) (b, _) -> a `compare` b) list
        maxY = snd $ maximumBy (\(_, a) (_, b) -> a `compare` b) list
        toChar coord = Map.findWithDefault ' ' coord chart
        ls = [[toChar (x, y) | x <- [minX..maxX]] | y <- [minY..maxY]]

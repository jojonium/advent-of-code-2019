import System.Environment
import FsHelpers
import qualified Data.Map as Map
import qualified Data.HashSet as HS
import Data.List (foldl')
import Data.Char (isUpper)
import Data.Maybe (fromJust)
import Algorithm.Search (dijkstra, aStar)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day20.txt"
fileNameFromArgs (x:_) = x

type Coord = (Int, Int)
type Chart = Map.Map Coord (HS.HashSet Coord)
type Portals = Map.Map Coord Coord
type PortalLocs = Map.Map String Coord

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let (m, maxX, maxY, thickness) = parse ls
  let (inner, outer, aa, zz) = getInnerOuterPortals m maxX maxY thickness
  let linked = linkPortals inner outer
  let plainChart = toChart m
  let p1c = part1Chart plainChart linked
  putStrLn $ "Part 1: " ++ show (part1 p1c aa zz)
  putStrLn $ "Part 2: " ++ show (part2 plainChart inner outer aa zz)

parse :: [String] -> (Map.Map Coord Char, Int, Int, Int)
parse ls = (foldl' folder Map.empty coords, maximum xs, maximum ys, thickness)
  where ys = [0..(length ls - 1)]
        xs = [0..(length (head ls) - 1)]
        thickness = length (filter (\c -> c=='.' || c == '#') (ls !! (length ls `div` 2))) `div` 2
        coords = [(x, y) | x <- xs, y <- ys]
        folder chart (x, y) = Map.insert (x, y) t chart
          where t = ls !! y !! x

part1 :: Chart -> Coord -> Coord -> Int
part1 chart aa zz = fst $ fromJust pathTo
  where pathTo = dijkstra (chart Map.!) (\_ _ -> 1) (==zz) aa

part2 :: Chart -> PortalLocs -> PortalLocs -> Coord -> Coord -> Int
part2 chart inner outer (sx, sy) (gx, gy) = fst $ fromJust $ aStar next (\_ _ -> 1) heuristic (==(gx, gy, 0)) (sx, sy, 0)
  where movingUp         = linkPortals outer inner
        movingDown       = linkPortals inner outer
        next (x, y, l) = hs'''
          where hs    = chart Map.! (x, y)
                up    = if l == 0 then Nothing else Map.lookup (x, y) movingUp
                dn    = Map.lookup (x, y) movingDown
                hs'   = HS.map (\(a, b) -> (a, b, l)) hs
                hs''  = combine up 1 hs'
                hs''' = combine dn (-1) hs''
                combine (Just (x', y')) delta s = HS.insert (x', y', l + delta) s
                combine Nothing _ s             = s
        heuristic (x, y, l) = abs (gx - x) + abs (gy - y) + l

linkPortals :: PortalLocs -> PortalLocs -> Portals
linkPortals inner outer = foldl' combiner Map.empty (Map.assocs inner)
  where combiner acc (str, (x, y)) = Map.insert (x, y) other acc
          where other = Map.findWithDefault (outer Map.! reverse str) str outer

part1Chart :: Chart -> Portals -> Chart
part1Chart plainChart portals = chartWithPortals
  where chartWithPortals = Map.foldlWithKey' inserter plainChart portals
          where inserter :: Chart -> Coord -> Coord -> Chart
                inserter acc from to = acc''
                  where acc'  = Map.insertWith HS.union from (HS.singleton to) acc
                        acc'' = Map.insertWith HS.union to (HS.singleton from) acc'

toChart :: Map.Map Coord Char -> Chart
toChart m = plainChart
  where plainChart = foldl' folder Map.empty (Map.assocs m)
        folder chart ((x, y), c)
          | c == '.'  = Map.insert (x, y) (foldl' isOpen HS.empty adjacent) chart
          | otherwise = chart
          where adjacent = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                isOpen hs a
                  | aChar == '.' = HS.insert a hs
                  | otherwise    = hs
                  where aChar = Map.findWithDefault '#' a m

getInnerOuterPortals :: Map.Map Coord Char -> Int -> Int -> Int -> (PortalLocs, PortalLocs, Coord, Coord)
getInnerOuterPortals m maxX maxY thickness = (inner, outer', aa, zz)
  where 
    outer           = getPortals' outerRim
    aa              = outer Map.! "AA"
    zz              = outer Map.! "ZZ"
    outer'          = Map.delete "AA" (Map.delete "ZZ" outer)
    inner           = getPortals' innerRim
    buffer          = thickness + 1
    outerRim        = [(x, y) | x <- [2..(maxX - 2)], y <- [2, maxY - 2]] ++
                      [(x, y) | x <- [2, maxX - 2], y <- [2..(maxY - 2)]]
    innerRim        = [(x, y) | x <- [buffer..(maxX - buffer)], y <- [buffer, maxY - buffer]] ++
                      [(x, y) | x <- [buffer, maxX - buffer], y <- [buffer..(maxY - buffer)]]
    getPortals' ps  = foldl' folder Map.empty (map (\c -> (c, m Map.! c)) ps)
      where 
        folder portals ((x, y), c)
          | c == '.'  = f ap
          | otherwise = portals
          where ap             = filter (isUpper . fst) $ map apMap adjacent
                apMap (a, b)   = (Map.findWithDefault ' ' a m, Map.findWithDefault ' ' b m)
                f []           = portals
                f ((p1, p2):_) = Map.insert [p1, p2] (x, y) portals
                adjacent       = [ ((x + 1, y), (x + 2, y))
                                 , ((x - 1, y), (x - 2, y))
                                 , ((x, y + 1), (x, y + 2))
                                 , ((x, y - 1), (x, y - 2))]


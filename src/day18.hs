import System.Environment
import FsHelpers
import qualified Data.Map as Map
import qualified Data.HashSet as HS
import Data.Char (isLower, toUpper, isUpper)
import Data.List (minimumBy, maximumBy, intercalate)
import Algorithm.Search (aStar, dijkstra)
import Data.Maybe (isJust, mapMaybe, fromJust)
import Debug.Trace (trace)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day18.txt"
fileNameFromArgs (x:_) = x

type Coord = (Int, Int)
type Chart = Map.Map Coord Char
type Locs = Map.Map Char Coord
data State = State
  { _sxy    :: Coord
  , _sc     :: Chart
  , _skeys  :: Locs
  , _sdoors :: Locs
  } deriving (Show, Eq, Ord)

main :: IO ()
main = do
  args <- getArgs
  ls   <- fileToLines $ fileNameFromArgs args
  let initialState = parse ls
  putStrLn (prettyPrint initialState)
  putStrLn $ "Numkeys = " ++ show (Map.size (_skeys initialState))
  putStrLn $ "Part 1: " ++ show (part1 initialState)

neighbors :: State -> [State]
neighbors (State (x, y) chart keys doors) = trace (show (Map.size keys)) states
  where paths  = mapMaybe (pathTo chart (x, y)) (Map.elems keys)
        states = map toState paths
        toState (_, ps) = State c chart'' keys' doors
          where c       = last ps
                k       = chart Map.! c
                keys'   = Map.delete k keys
                chart'  = Map.insert c '.' chart  -- key space is empty now
                chart'' = if Map.member (toUpper k) doors  -- open corresponding door if it exists
                           then Map.insert (doors Map.! toUpper k) '.' chart'
                           else chart'

cost :: State -> State -> Int
cost (State { _sxy = s1 }) (State s2 chart _ _) = fst $ fromJust $ pathTo chart s1 s2

part1 :: State -> Maybe Int
part1 s = case dijkstra neighbors cost (Map.null . _skeys) s of
    Nothing     -> Nothing
    Just (x, _) -> Just x

parse :: [String] -> State
parse ls = foldr folder (State (0, 0) Map.empty Map.empty Map.empty) coords
  where ys = [0..(length ls - 1)]
        xs = [0..(length (head ls) - 1)]
        coords = [(x, y) | x <- xs, y <- ys]
        folder (x, y) (State start chart keys doors) = State start' chart' keys' doors'
          where t      = ls !! y !! x
                t'     = if t == '@' then '.' else t
                start' = if t == '@' then (x, y) else start
                doors' = if isUpper t then Map.insert t (x, y) doors else doors
                chart' = Map.insert (x, y) t' chart
                keys'  = if isLower t then Map.insert t (x, y) keys else keys

pathTo :: Chart -> Coord -> Coord -> Maybe (Int, [Coord])
pathTo chart from (gx, gy) = aStar graphFunc (\_ _ -> 1) h (==(gx, gy)) from
  where h (x, y)         = (abs gx - x) + (abs gy - y)
        graphFunc (x, y) = foldr isOpen HS.empty adjacent
          where adjacent = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                isOpen c hs
                  | c == (gx, gy)                          = HS.insert c hs
                  | Map.findWithDefault '#' c chart == '.' = HS.insert c hs
                  | otherwise = hs

prettyPrint :: State -> String
prettyPrint (State start chart _ _) = intercalate "\n" ls ++ "\n"
  where list = Map.keysSet chart
        minX = fst $ minimumBy (\(a, _) (b, _) -> a `compare` b) list
        minY = snd $ minimumBy (\(_, a) (_, b) -> a `compare` b) list
        maxX = fst $ maximumBy (\(a, _) (b, _) -> a `compare` b) list
        maxY = snd $ maximumBy (\(_, a) (_, b) -> a `compare` b) list
        toChar coord
          | coord == start = '@'
          | otherwise      = Map.findWithDefault ' ' coord chart
        ls = [[toChar (x, y) | x <- [minX..maxX]] | y <- [minY..maxY]]

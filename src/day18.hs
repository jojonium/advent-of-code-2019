import System.Environment
import FsHelpers
import qualified Data.Map as Map
import qualified Data.HashSet as HS
import Data.Char (isLower, toUpper, isUpper)
import Data.List (minimumBy, maximumBy, intercalate)
import Algorithm.Search (aStar, dijkstra)
import Data.Maybe (mapMaybe)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day18.txt"
fileNameFromArgs (x:_) = x

type Coord = (Int, Int)
type Chart = Map.Map Coord Char
type Locs = Map.Map Char Coord
data State = State
  { _srb    :: [Coord] -- robot positions
  , _sc     :: Chart
  , _skeys  :: Locs
  , _sdoors :: Locs
  , _last   :: Int     -- last cost
  } deriving (Show, Eq, Ord)

main :: IO ()
main = do
  args <- getArgs
  ls   <- fileToLines $ fileNameFromArgs args
  let initialState = parse ls
  putStrLn $ "Part 1 (This will take a long time): " ++ show (solve initialState)
  let p2Init = toPart2State initialState
  putStrLn $ "Part 2 (This will take a long time): " ++ show (solve p2Init)

neighbors :: State -> [State]
neighbors (State robots chart keys doors _) = states
  where paths  = concatMap (\robot -> mapMaybe (pathTo chart robot) (Map.elems keys)) robots
        states = map toState paths
        toState (price, start, ps) = State robots' chart'' keys' doors price
          where end     = last ps
                robots' = end : filter (/=start) robots
                k       = chart Map.! end
                keys'   = Map.delete k keys
                chart'  = Map.insert end '.' chart  -- key space is empty now
                chart'' = if Map.member (toUpper k) doors  -- open corresponding door if it exists
                          then Map.insert (doors Map.! toUpper k) '.' chart'
                          else chart'

solve :: State -> Maybe Int
solve s = case dijkstra neighbors (\_ s2 -> _last s2) (Map.null . _skeys) s of
    Nothing     -> Nothing
    Just (x, _) -> Just x

pathTo :: Chart -> Coord -> Coord -> Maybe (Int, Coord, [Coord])
pathTo chart from (gx, gy) = case aStar graphFunc (\_ _ -> 1) h (==(gx, gy)) from of
    Nothing     -> Nothing
    Just (a, b) -> Just (a, from, b)
  where h (x, y)         = (abs gx - x) + (abs gy - y)
        graphFunc (x, y) = foldr isOpen HS.empty adjacent
          where adjacent = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                isOpen c hs
                  | c == (gx, gy)                          = HS.insert c hs
                  | Map.findWithDefault '#' c chart == '.' = HS.insert c hs
                  | otherwise = hs

prettyPrint :: State -> String
prettyPrint (State robots chart _ _ _) = intercalate "\n" ls ++ "\n"
  where list = Map.keysSet chart
        minX = fst $ minimumBy (\(a, _) (b, _) -> a `compare` b) list
        minY = snd $ minimumBy (\(_, a) (_, b) -> a `compare` b) list
        maxX = fst $ maximumBy (\(a, _) (b, _) -> a `compare` b) list
        maxY = snd $ maximumBy (\(_, a) (_, b) -> a `compare` b) list
        toChar coord
          | coord `elem` robots = '@'
          | otherwise      = Map.findWithDefault ' ' coord chart
        ls = [[toChar (x, y) | x <- [minX..maxX]] | y <- [minY..maxY]]

parse :: [String] -> State
parse ls = foldr folder (State [] Map.empty Map.empty Map.empty 0) coords
  where ys = [0..(length ls - 1)]
        xs = [0..(length (head ls) - 1)]
        coords = [(x, y) | x <- xs, y <- ys]
        folder (x, y) (State start chart keys doors _) = State start' chart' keys' doors' 0
          where t      = ls !! y !! x
                t'     = if t == '@' then '.' else t
                start' = if t == '@' then [(x, y)] else start
                doors' = if isUpper t then Map.insert t (x, y) doors else doors
                chart' = Map.insert (x, y) t' chart
                keys'  = if isLower t then Map.insert t (x, y) keys else keys

toPart2State :: State -> State
toPart2State (State ir chart keys doors _) = State robots chart' keys doors 0
    where (x, y) = head ir
          robots = [(x - 1, y + 1), (x + 1, y + 1), (x - 1, y - 1), (x + 1, y - 1)]
          newWalls = [(x, y) , (x - 1, y) , (x + 1, y) , (x, y - 1) , (x, y + 1)]
          chart' = foldr (`Map.insert` '#') chart newWalls


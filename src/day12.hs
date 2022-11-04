import System.Environment
import FsHelpers
import Data.List (intercalate)
import qualified Data.Map as Map

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day12.txt"
fileNameFromArgs (x:_) = x

-- velocity, position
type Velocity = (Integer, Integer, Integer)
type Position = (Integer, Integer, Integer)
data Moon = Moon { mPos :: Position
                 , mVel :: Velocity
                 } deriving (Show, Eq)

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let moons = map parseLine ls
  putStrLn $ "Part 1: " ++ show (sum (map energy (iterate step moons !! 1000)))
  -- putStrLn $ "Part 2: " ++ show (part2 moons)

parseLine :: String -> Moon
parseLine str = Moon (read pxs, read pys, read pzs) (0, 0, 0)
  where f          = filter (`notElem` "<xyz= >\n") str
        (pxs, f')  = span (/=',') f
        (pys, f'') = span (/=',') (tail f')
        pzs        = tail f''

spliceIndex :: Int -> [a] -> [a]
spliceIndex i l  = a ++ tail b
  where (a, b) = splitAt i l

applyGravity :: [Moon] -> Int -> Moon
applyGravity ms i = moon { mVel = newVel, mPos = applyVel newVel }
  where others = spliceIndex i ms
        moon   = ms !! i
        pull :: Position -> Moon -> Velocity -> Velocity
        pull (x, y, z) (Moon {mPos=(ox, oy, oz)}) (accX, accY, accZ) =
          (adjustOne accX x ox, adjustOne accY y oy, adjustOne accZ z oz)
        adjustOne acc a b 
          | b - a > 0 = acc + 1
          | b - a < 0 = acc - 1
          | otherwise = acc
        newVel = foldr (pull (mPos moon)) (mVel moon) others
        applyVel :: Velocity -> Position
        applyVel (vx, vy, vz)  = (px + vx, py + vy, pz + vz)
          where (px, py, pz) = mPos moon

step :: [Moon] -> [Moon]
step moons = map (applyGravity moons) [0..(length moons - 1)]

energy :: Moon -> Integer
energy m = potential * kinetic
  where (px, py, pz) = mPos m
        (vx, vy, vz) = mVel m
        potential    = abs px + abs py + abs pz
        kinetic      = abs vx + abs vy + abs vz


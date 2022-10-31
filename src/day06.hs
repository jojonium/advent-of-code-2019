import System.Environment
import FsHelpers
import qualified Data.Map as Map
import Data.List (intersect, elemIndex)

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day06.txt"
fileNameFromArgs (x:_) = x

type Body = String -- celestial body
type Orbits = Map.Map Body [Body]

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let orbits = parse (map splitLine ls) (Map.fromList [("COM", [])])
  putStrLn $ "Part 1: " ++ show (part1 orbits)
  putStrLn $ "Part 2: " ++ show (part2 orbits)

-- creates map of child -> parent
parse :: [(Body, Body)] -> Orbits -> Orbits
parse [] orbits = orbits
parse ((key,elt):ls) orbits = parse ls parsedLine
  where parsedLine = Map.insertWith (++) elt [key]
                    -- Insert left side in case it's a leaf
                    (Map.insertWith (++) key [] orbits)

totalChildren :: Body -> Orbits -> Int
totalChildren k m = foldr (\ body acc -> acc + totalChildren body m) direct children
  where children = Map.findWithDefault [] k m
        direct = length children

splitLine :: String -> (Body, Body)
splitLine "" = error "splitLine: invalid string \"\""
splitLine s
  | length parts < 2 = error $ "splitLine: invalid string " ++ s
  | otherwise = (head parts, parts!!1)
 where parts = split (==')') s

ancestors :: Body -> Orbits -> [Body]
ancestors key orbits = foldr (\ p a -> a ++ ancestors p orbits) [key] parents
  where parents = Map.findWithDefault [] key orbits

part1 :: Orbits -> Int
part1 orbits = Map.foldrWithKey (\ k _ acc -> acc + totalChildren k orbits) 0 orbits

part2 :: Orbits -> Int
part2 orbits = unMaybe (elemIndex pivot santaPath) + unMaybe (elemIndex pivot youPath)
  where youPath   = ancestors "YOU" orbits
        santaPath = ancestors "SAN" orbits
        pivot     = head $ youPath `intersect` santaPath
        unMaybe Nothing  = error "Pivot not found in list"
        unMaybe (Just x) = x - 1 -- Remove first and last transfer

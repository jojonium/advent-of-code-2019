import System.Environment
import FsHelpers
import qualified Data.Map as Map
import Debug.Trace (trace)
import Data.List (intercalate)
import Data.Ratio

fileNameFromArgs :: [String] -> String
fileNameFromArgs [] = "inputs/day14.txt"
fileNameFromArgs (x:_) = x

type Ingred = (Integer, String)
type Recipes = Map.Map String (Integer, [Ingred])
type Chemicals = Map.Map String Integer

main :: IO ()
main = do
  args <- getArgs
  ls <- fileToLines $ fileNameFromArgs args
  let parsed = parseLines ls
  putStrLn $ "Part 1: " ++ show (solve parsed (Map.fromList [("FUEL", 1)]) Map.! "ORE")

elements :: [(Integer, String)] -> String -> [(Integer, String)]
elements lst "" = lst
elements lst str = (read amt, filter (/=' ') name) : elements lst next
  where isJunk = (`elem` " ,=>")
        (left, right) = break (`elem` ",=") str
        (amt, name) = break (==' ') (dropWhile isJunk left)
        next = dropWhile isJunk right

parseLines :: [String] -> Map.Map String (Integer, [Ingred])
parseLines = foldl parseOne Map.empty
  where parseOne m line = Map.insert name recipes m
          where els  = elements [] line
                (amt, name) = last els
                recipes = (amt, init els)

solve :: Recipes -> Chemicals -> Chemicals
solve recipes required = if Map.null nonOreRequired then required else solve recipes required''
  where nonOreRequired = Map.filterWithKey (\s i -> s /= "ORE" && i > 0) required
        (name, amt)    = Map.findMin nonOreRequired
        (oAmt, os)     = recipes Map.! name
        batches        = ceiling (amt % oAmt)
        required'      = Map.insert name (amt - (batches * oAmt)) required
        required''     = foldl (addRequirement batches) required' os
        addRequirement bs req (ata, nta) = Map.insert nta newAmt' req
          where newAmt  = Map.findWithDefault 0 nta required
                newAmt' = newAmt + (bs * ata)

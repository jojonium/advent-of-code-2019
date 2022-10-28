import Data.List (sort)
import Text.Regex.TDFA

main :: IO ()
main = do
  putStrLn $ "Part 1: " ++ show (part1 284639 748759)
  putStrLn $ "Part 2: " ++ show (part2 284639 748759)

nonDec :: Int -> Bool
nonDec n = sort lst == lst 
  where lst = map (\ x -> read (x:"") :: Int) (show n)

dubs :: Int -> Bool
dubs n = any (uncurry (==)) $ zip <*> tail $ show n

strictDubs :: Int -> Bool
strictDubs n = any f [0..9]
  where lst = show n
        f :: Int -> Bool
        f x = lst =~ ("(^|[^" ++ show x ++ "])" ++ show x ++ "{2}($|[^" ++ show x ++ "])")

part1 :: Int -> Int -> Int
part1 a b = length [x | x <- [a..b], dubs x, nonDec x]

part2 :: Int -> Int -> Int
part2 a b = length $ filter strictDubs [x | x <- [a..b], dubs x, nonDec x]


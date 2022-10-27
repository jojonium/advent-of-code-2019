module FsHelpers (
  fileToLines,
  fileToIntegers,
  split
) where

fileToLines :: String -> IO [String]
fileToLines filename = do
  contents <- readFile filename
  return $ lines contents

fileToIntegers :: String -> IO [Integer]
fileToIntegers filename = do
  contents <- readFile filename
  return $ map read (lines contents)

-- split a string on a predicate
split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
                 "" -> []
                 s' -> w : split p s''
                       where (w, s'') = break p s'

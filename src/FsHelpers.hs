module FsHelpers (
  fileToLines,
  fileToIntegers
) where

fileToLines :: String -> IO [String]
fileToLines filename = do
  contents <- readFile filename
  return $ lines contents

fileToIntegers :: String -> IO [Integer]
fileToIntegers filename = do
  contents <- readFile filename
  return $ map read (lines contents)

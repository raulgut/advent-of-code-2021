-----------------------------------------------------------------------------
-- | First part of Day 7
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (minimum, maximum)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map read . splitOn "," $ input) :: [Int]
  let positions = [map (\x -> abs $ x - i) entries | i <- [minimum entries..maximum entries]]
  putStrLn . show . minimum . map sum $ positions

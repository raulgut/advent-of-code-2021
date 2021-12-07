-----------------------------------------------------------------------------
-- | Second part of Day 7
--
-- Brute force
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (minimum, maximum)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map read . splitOn "," $ input) :: [Int]
  let positions = [map (\x -> sum [j | j <- [0..abs $ x - i]]) entries | i <- [minimum entries..maximum entries]]
  putStrLn . show . minimum . map sum $ positions

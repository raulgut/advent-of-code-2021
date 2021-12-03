-----------------------------------------------------------------------------
-- | First part of Day 3
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Char (isDigit, digitToInt)
import Data.List (transpose)
import Data.MultiSet (fromList, occur)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  let report = map (filter isDigit) . lines $ input
  let distribution = map (fromList . map digitToInt) . transpose $ report
  let zeros = map (occur 0) distribution
  let ones = map (occur 1) distribution
  let gamma = zipWith (\x y -> if max x y == x then 0 else 1) zeros ones
  let epsilon = map (\x -> if x == 0 then 1 else 0) gamma
  putStrLn . show $ binListToInt 0 gamma * binListToInt 0 epsilon

-- | from a list of numbers in {0, 1} representing a binary number to Int
binListToInt :: Int -> [Int] -> Int
binListToInt acc [] = acc
binListToInt acc [b] = acc * 2 + b
binListToInt acc (b:bs) = binListToInt (acc * 2 + b) bs
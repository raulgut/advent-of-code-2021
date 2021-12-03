-----------------------------------------------------------------------------
-- | Second part of Day 3
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
  putStrLn . show $ binListToInt 0 (findOxigen 0 report) * binListToInt 0 (findCO2 0 report)

-- | from a list of numbers in {0, 1} representing a binary number to Int
binListToInt :: Int -> [Int] -> Int
binListToInt acc [] = acc
binListToInt acc [b] = acc * 2 + b
binListToInt acc (b:bs) = binListToInt (acc * 2 + b) bs

-- | extracts the oxygen generator rating
findOxigen pos [] = error "Empty list!"
findOxigen pos [result] = map digitToInt result
findOxigen pos oldReport
  = let distribution = fromList . map digitToInt . (!!pos) . transpose $ oldReport
        zeros = occur 0 distribution
        ones = occur 1 distribution
        maxOccur = if zeros > ones then '0' else '1'
    in findOxigen (pos + 1) (filter (\value -> value!!pos == maxOccur) oldReport)

-- | extracts the CO2 scrubber rating
findCO2 pos [] = error "Empty list!"
findCO2 pos [result] = map digitToInt result
findCO2 pos oldReport
  = let distribution = fromList . map digitToInt . (!!pos) . transpose $ oldReport
        zeros = occur 0 distribution
        ones = occur 1 distribution
        maxOccur = if zeros > ones then '1' else '0'
    in findCO2 (pos + 1) (filter (\value -> value!!pos == maxOccur) oldReport)
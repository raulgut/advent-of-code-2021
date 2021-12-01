-----------------------------------------------------------------------------
-- | Second part of Day 1
--
-- Precondition: Input list is greater than 2
-----------------------------------------------------------------------------

import System.Environment (getArgs)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map read . lines $ input) :: [Int]
  putStrLn . show . getIncreases . (\(e1:e2:es) -> zipWith3 (\x y z -> x + y + z) es (e2:es) (e1:e2:es)) $ entries

-- | returns the number of times the sliding window increases
getIncreases :: Ord a => [a] -> Int
getIncreases (e:es) = length . filter (==True) $ zipWith (>) es (e:es)


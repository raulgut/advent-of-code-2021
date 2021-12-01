-----------------------------------------------------------------------------
-- | First part of Day 1
-- 
-- Precondition: Input is not empty
-----------------------------------------------------------------------------

import System.Environment (getArgs)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map read . lines $ input) :: [Int]
  putStrLn . show . length . filter (==True) . (\(e:es) -> zipWith (>) es (e:es)) $ entries

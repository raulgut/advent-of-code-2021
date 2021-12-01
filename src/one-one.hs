-----------------------------------------------------------------------------
-- | First part of Day 1
-----------------------------------------------------------------------------

import System.Environment (getArgs)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map read . lines $ input) :: [Int]
  putStrLn . show $ entries

-----------------------------------------------------------------------------
-- | First part of Day 8
-----------------------------------------------------------------------------

import System.Environment (getArgs)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = concatMap (tail . snd . span (/= "|") . words) . lines $ input
  putStrLn . show . length . filter (\x -> x < 5 || x == 7) . map length $ entries

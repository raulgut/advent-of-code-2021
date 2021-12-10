-----------------------------------------------------------------------------
-- | First part of Day 9
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Matrix (Matrix, fromLists, safeGet)
import Data.Char (digitToInt, isDigit)
import Data.Maybe (catMaybes)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map (map digitToInt . filter isDigit) . lines $ input) :: [[Int]]
  let rows = length entries 
  let columns = length . head $ entries
  putStrLn . show . foldl (+) 0 . map (+1) . catMaybes . (\m -> [isLowPoint i j m | i <- [1..rows], j <- [1..columns]]) . fromLists $ entries


-- | checks if (i,j) is a low point
isLowPoint :: Int -> Int -> Matrix Int -> Maybe Int 
isLowPoint i j m 
    = let eMaybe = safeGet i j m
      in case eMaybe of
           Just e -> if (and . map (e <) . catMaybes $ [safeGet (i - 1) j m, safeGet (i + 1) j m, safeGet i (j - 1) m, safeGet i (j + 1) m]) then
                       Just e 
                     else
                       Nothing
           Nothing -> Nothing

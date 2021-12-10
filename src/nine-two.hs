-----------------------------------------------------------------------------
-- | Second part of Day 9
--
-- Precondition : There are, at least, 3 basins
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Matrix (Matrix, fromLists, safeGet)
import Data.Char (digitToInt, isDigit)
import Data.Maybe (catMaybes)
import Control.Monad.State (State (..), get, put, evalState)
import Data.Set (Set,empty, insert, member)
import Data.List (sort)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map (map digitToInt . filter isDigit) . lines $ input) :: [[Int]]
  let rows = length entries 
  let columns = length . head $ entries
  let m = fromLists entries
  let lowPoints = catMaybes . (\m -> [isLowPoint i j m | i <- [1..rows], j <- [1..columns]]) $ m
  let basins = take 3 . reverse . sort . map (length . (\x -> evalState (getBasin m x) empty)) $ lowPoints
  putStrLn . show . foldl1 (*) $ basins

-- | checks if (i,j) is a low point
isLowPoint :: Int -> Int -> Matrix Int -> Maybe (Int,Int,Int) 
isLowPoint i j m 
    = let eMaybe = safeGet i j m
      in case eMaybe of
           Just e -> if (and . map (e <) . catMaybes $ [safeGet (i - 1) j m, safeGet (i + 1) j m, safeGet i (j - 1) m, safeGet i (j + 1) m]) then
                       Just (i,j,e) 
                     else
                       Nothing
           Nothing -> Nothing

-- | gets the basins by expansion
getBasin :: Matrix Int -> (Int, Int, Int) -> State (Set (Int,Int)) [Int]
getBasin m (i,j,basin)
  = do { let addPos i j eMaybe = fmap (\x -> (i, j, x)) eMaybe
       ; let adjacents = catMaybes $ [addPos (i - 1) j $ safeGet (i - 1) j m, addPos (i + 1) j $ safeGet (i + 1) j m, addPos i (j - 1) $ safeGet i (j - 1) m, addPos i (j + 1) $ safeGet i (j + 1) m]
       ; adjValues <- processAdjacents m (i,j,basin) adjacents
       ; return $ basin:adjValues
       }

-- | process all the adjacents positions from a valid part of the basin
processAdjacents _ _ [] = return []
processAdjacents m (i,j,basin) ((i',j',e):adjs)
  = do { visited <- get
       ; if (not . member (i',j') $ visited) && (e > basin) && (e /= 9) then 
           do { put . insert (i',j') $ visited 
              ; newValues <- getBasin m (i',j',e)
              ; restValues <- processAdjacents m (i,j,basin) adjs
              ; return (newValues ++ restValues)
              }
         else
           processAdjacents m (i,j,basin) adjs
       }
-----------------------------------------------------------------------------
-- | First part of Day 11
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Map as M (Map, fromList, lookup, keys, filter, insert)
import Data.Char (digitToInt, isDigit)
import Data.Maybe (catMaybes)
import Control.Monad.State (State (..), get, put, execState)
import Data.Set as S (Set, empty, difference, union, fromList, elems)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Current State
data CState
  = CState { step :: Int
           , flashes :: Int
           , visited :: Set (Int,Int)
           , grid :: Map (Int,Int) Int
           } deriving Show

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map (map digitToInt . Prelude.filter isDigit) . lines $ input) :: [[Int]]
  putStrLn . show . (\octopuses -> execState (process 100) (CState 0 0 empty octopuses)) . M.fromList $ [((i,j),ientry!!(j-1)) | i <- [1..length entries], let ientry = entries!!(i-1), j <- [1..length ientry]]

-- | 
process :: Int -> State CState ()
process maxSteps
  = do { state <- get 
       ; if (step state == maxSteps) then
           return ()
         else
           do { let octopuses1 = fmap (+1) (grid state)
              ; let flashes = keys . M.filter (>9) $ octopuses1
              ; fixPointFlashes octopuses1 flashes
              ; process maxSteps
              } 
       }

-- |
fixPointFlashes :: Map (Int,Int) Int -> [(Int,Int)] -> State CState ()
fixPointFlashes octopuses [] 
  = do { state <- get
       ; let newFlashes = keys . M.filter (>9) $ octopuses
       ; put $ state {step = (step state) + 1
                     ,flashes = (flashes state) + (length newFlashes)
                     ,grid = fmap (\x -> if x > 9 then 0 else x) octopuses
                     ,visited = S.empty
                     }
       ; return ()
       }
fixPointFlashes octopuses currentFlashes
  = do { let adjacents = concat [[(i - 1, j - 1)
                                 ,(i - 1, j)
                                 ,(i - 1, j + 1)
                                 ,(i, j - 1)
                                 ,(i, j + 1)
                                 ,(i + 1, j - 1)
                                 ,(i + 1, j)
                                 ,(i + 1, j + 1)
                                 ] | (i,j) <- currentFlashes]
       ; let newOctopuses = foldl increaseGrid octopuses adjacents
       ; state <- get
       ; let newVisited = union (visited state) (S.fromList currentFlashes) 
       ; let newFlashes = elems . (\x -> difference x newVisited) . S.fromList . keys . M.filter (>9) $ newOctopuses
       ; put $ state {visited = newVisited}
       ; fixPointFlashes newOctopuses newFlashes
       }


-- |
increaseGrid :: Map (Int,Int) Int -> (Int,Int) -> Map (Int,Int) Int 
increaseGrid grid p = case M.lookup p grid of
                        Just value -> if value <= 9 then insert p (value + 1) grid else grid
                        Nothing -> grid

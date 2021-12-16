-----------------------------------------------------------------------------
-- | Second part of Day 15
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Char (digitToInt, isDigit)
import Data.Map as M (Map, fromList, lookup, insert, empty, assocs, delete)
import Control.Monad.State (State (..), get, put, evalState)
import Data.Maybe (isJust, fromJust)
import Data.List (sortBy)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

data AState = AState { current :: (Int, Int)
                     , weight :: Int
                     , prev :: Maybe (Int, Int)
                     , visited :: Map (Int,Int) Int
                     , remaining :: Map (Int,Int) Int
                     }
            deriving Show

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = (map (map digitToInt . filter isDigit) . lines $ input) :: [[Int]]
  let entriesBy5 = concat [(map (map (\entry -> let newEntry = entry + i in if newEntry > 9 then mod newEntry 9 else newEntry))) entries | i <- [0..4]]
  let entries5By5 = [concat [map (\entry -> let newEntry = entry + j in if newEntry > 9 then mod newEntry 9 else newEntry) (entriesBy5!!i) | j <- [0..4]] | i <- [0..length entriesBy5 - 1]]
  let bound = length entries5By5 - 1
  let weights = M.fromList $ [((i, j),(entries5By5!!i)!!j) | i <- [0..length entries5By5 - 1], j <- [0..length (entries5By5!!i) - 1]]
  putStrLn . show . weight $ evalState (findPath weights (bound,bound)) (AState (0,0) (getH (bound,bound) (0,0)) Nothing M.empty M.empty) 

-- | finds the sortest path usign the A* algorithm
findPath :: Map (Int,Int) Int -> (Int,Int) -> State AState AState
findPath weights goal
  = do { state <- get
       ; let (x,y) = current state 
       ; let currentWeight = weight state
       ; let prevPos = prev state 
       ; let visitedPs = visited state 
       ; let newVisited = M.insert (x,y) currentWeight visitedPs
       ; let newRemainings = foldl (updateRemainings newVisited) (remaining state) [(possibility,fromJust pWeight + currentWeight - getH goal (x,y) + getH goal possibility) | possibility <- [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                                                                                                                                                                             , let pWeight = M.lookup possibility weights
                                                                                                                                                                             , isJust pWeight]
       ; let precedence = sortBy (\e1 e2 -> compare (snd e1) (snd e2)) . assocs $ newRemainings
       ; if (null precedence) then  
           return $ state { visited = newVisited , remaining = newRemainings}
         else 
           let candidate = head precedence
               newState = state { current = fst candidate , weight = snd candidate, prev = Just (x,y), visited = newVisited , remaining = M.delete (fst candidate) newRemainings}
           in if ((==goal) . fst . head $ precedence) then
                return newState
              else 
                do { put newState
                   ; findPath weights goal
                   }
       } 

-- | updates the adjacents non-visited states
updateRemainings :: Map (Int,Int) Int -> Map (Int,Int) Int -> ((Int,Int),Int) -> Map (Int, Int) Int
updateRemainings visited remainings (pos1,v1)
  = case (M.lookup pos1 remainings, M.lookup pos1 visited) of 
      (Just v2, Just v3) -> if v1 < v3 then
                              if v1 < v2 then
                                M.insert pos1 v1 remainings
                              else 
                                remainings
                            else 
                              M.delete pos1 remainings
      (Just v2, Nothing) -> if v1 < v2 then
                              M.insert pos1 v1 remainings
                            else 
                                remainings
      (Nothing, Just v3) -> if v1 < v3 then
                              M.insert pos1 v1 remainings
                            else 
                              M.delete pos1 remainings
      (Nothing, Nothing) -> M.insert pos1 v1 remainings

-- | Our heuristic is the number of steps needed to arrive to the goal
getH :: (Int, Int) -> (Int,Int) -> Int
getH goal (x, y) = (fst goal) - x + (snd goal) - y
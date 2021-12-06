-----------------------------------------------------------------------------
-- | First and Second part of Day 6
--
-- Precondition: Two arguments: days (Natural number) and filename
-- First part, day = 80
-- Second part, day = 256
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.List.Split (splitOn)

main = do
  args <- getArgs
  let days = read . head $ args
  input <- readFile . head . tail $ args
  let entries = (map read . splitOn "," $ input) :: [Int]
  putStrLn . show . execute days . (\x -> (x,[0,0])) $ [length . filter (==i) $ entries  | i <- [0..6]]

-- | Simulates growth and returns population
execute :: Int -> ([Int],[Int]) -> Int
execute 0 (population1, population2) = foldl (+) 0 (population1 ++ population2)
execute days ((p1:ps1), (p2:ps2)) = execute (days - 1) (ps1 ++ [p2 + p1], (ps2 ++ [p1]))

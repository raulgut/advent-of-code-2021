-----------------------------------------------------------------------------
-- | First part of Day 5
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec.Char (digit, string, char)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (parse)
import Data.Map as M (Map, empty, lookup, insert, filter, size)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  let vents = (map extractVents . lines $ input) :: [((Int,Int), (Int, Int))]
  putStrLn . show . size . M.filter (>1) . foldl processVent empty $ vents

-- | Parses vent coordinates
extractVents :: String -> ((Int,Int), (Int, Int))
extractVents str
  = case (parse ventParser "" str) of
      Left err -> error . show $ err
      Right coord -> coord

-- | parses vent coordinates
ventParser :: Parser ((Int,Int), (Int, Int))
ventParser =
 do x1 <- many1 digit
    char ',' 
    y1 <- many1 digit
    string " -> " 
    x2 <- many1 digit
    char ',' 
    y2 <- many1 digit
    return $ ((read x1, read y1),(read x2,read y2))

-- | for each vent, we add 1 to its coordinates
processVent :: Map (Int,Int) Int -> ((Int, Int),(Int, Int)) -> Map (Int,Int) Int
processVent ventMap ((x1,y1),(x2,y2)) | x1 == x2 = if y1 < y2 then foldl updateVent ventMap [(x1,y) | y <- [y1..y2]] else foldl updateVent ventMap [(x1,y) | y <- [y2..y1]]
processVent ventMap ((x1,y1),(x2,y2)) | y1 == y2 = if x1 < x2 then foldl updateVent ventMap [(x,y1) | x <- [x1..x2]] else foldl updateVent ventMap [(x,y1) | x <- [x2..x1]]
processVent ventMap _ = ventMap

-- | given a coordinate, increase its occurrence
updateVent :: Map (Int,Int) Int -> (Int, Int) -> Map (Int,Int) Int
updateVent ventMap (x,y) = case M.lookup (x,y) ventMap of
                             Nothing -> M.insert (x,y) 1 ventMap
                             Just value -> M.insert (x,y) (value + 1) ventMap
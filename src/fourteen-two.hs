-----------------------------------------------------------------------------
-- | Second part of Day 14
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, many, newline, try, string)
import Text.Parsec.Char (digit, char, letter)
import Text.Parsec.Combinator (many1, sepEndBy, eof)
import Text.Parsec.Prim (parse, (<|>))
import Data.Map as M (Map, fromList, lookup, empty, insert, assocs)
import Data.List (sort, groupBy)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  case (parse parsePatternAndRules "" input) of
    Left err -> error . show $ err
    Right (pattern,rules) -> putStrLn . show . (\list -> (fst . last $ list) - (fst . head $ list)) . sort . complete (head pattern) . map (foldl1 getSum) . groupBy (\x y -> fst x == fst y) . sort 
                               . map (\([c1,c2],value) -> (c2,value)) . assocs $ process 40 (M.fromList rules) (getGroups pattern)
  where getSum (c1,v1) (c2,v2) = (c1, v1 + v2)  -- c1 == c2
        complete c1 [] = []
        complete c1 ((c2,v2):rest) = if c1 == c2 then (v2 + 1, c2):(complete c1 rest) else (v2,c2):(complete c1 rest) -- we duplicate each symbol in the computation except for the first one, when removing duplications we lose the initial symbol

-- | extracts the pattern and the rules to form polymers
parsePatternAndRules :: Parser (String,[(String,Char)])
parsePatternAndRules = do pattern <- many1 letter
                          (try (char '\r') >> newline) <|> try (char '\n')
                          (try (char '\r') >> newline) <|> try (char '\n')
                          rules <- many1 parseRules
                          return (pattern,rules)

-- | extracts the insertion rules 
parseRules :: Parser (String,Char)
parseRules = do l <- many1 letter
                string " -> " 
                r <- letter
                (try eof >> return '#') <|> (try (char '\r') >> newline) <|> try (char '\n') -- we can find the end of input at the end of the line
                return (l,r)

-- | executes the number of steps
process :: Int -> Map String Char -> Map String Int -> Map String Int
process 0 _ pattern = pattern 
process n rules pattern = let patterns = M.assocs pattern 
                              newPattern = foldl (applyRules rules pattern) M.empty patterns
                          in process (n - 1) rules newPattern

-- | we group by pairs duplicating the given pattern 
getGroups :: String -> Map String Int
getGroups (c1:c2:rest) = let group = getGroups (c2:rest)
                         in case M.lookup [c1,c2] group of
                              Just value -> M.insert [c1,c2] (value + 1) group 
                              Nothing -> M.insert [c1,c2] 1 group
getGroups _ = M.empty

-- | applies the rules and creates a new pattern
applyRules :: Map String Char -> Map String Int -> Map String Int -> (String, Int) -> Map String Int 
applyRules rules oldPattern newPattern ([c1,c2],value) 
  = case M.lookup [c1,c2] rules of 
      Nothing -> M.insert [c1,c2] value newPattern 
      Just c3 -> insertNewPattern [c3,c2] value . insertNewPattern [c1,c3] value $ newPattern
  where insertNewPattern [c,d] v1 gmap 
          = case M.lookup [c,d] gmap of 
                   Nothing -> M.insert [c,d] v1 gmap 
                   Just v2 -> M.insert [c,d] (v1 + v2) gmap


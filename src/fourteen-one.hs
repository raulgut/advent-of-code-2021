-----------------------------------------------------------------------------
-- | First part of Day 14
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, many, newline, try, string)
import Text.Parsec.Char (digit, char, letter)
import Text.Parsec.Combinator (many1, sepEndBy, eof)
import Text.Parsec.Prim (parse, (<|>))
import Data.Map as M (Map, fromList, lookup)
import Data.List (group, sort)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  case (parse parsePatternAndRules "" input) of
    Left err -> error . show $ err
    Right (pattern,rules) -> putStrLn . show . (\list -> (fst . last $ list) - (fst . head $ list)) . sort . map (\x -> (length x, head x)) . group . sort $ process 10 (M.fromList rules) pattern

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
process :: Int -> Map String Char -> String -> String
process 0 _ pattern = pattern 
process n rules pattern = let groups = getGroups pattern 
                              (r1:rest) = map (applyRules rules) groups 
                              result = concat $ r1:(map tail rest)
                          in process (n - 1) rules result

-- | we group by pairs duplicating the given pattern 
getGroups :: String -> [String]
getGroups (c1:c2:rest) = [[c1,c2]] ++ getGroups (c2:rest)
getGroups _ = []

-- | applies the rules and expands the pattern
applyRules :: Map String Char -> String -> String 
applyRules rules g@[c1,c2] = case M.lookup g rules of 
                               Nothing -> g 
                               Just c3 -> [c1,c3,c2] 

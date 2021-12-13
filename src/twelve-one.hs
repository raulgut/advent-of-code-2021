-----------------------------------------------------------------------------
-- | First part of Day 12
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, many, newline, try, string)
import Text.Parsec.Char (digit, char, letter)
import Text.Parsec.Combinator (many1, sepEndBy, eof)
import Text.Parsec.Prim (parse, (<|>))
import Data.Set as S (Set, fromList, elems, delete, member)
import Data.Map as M (Map,fromList, lookup, size, delete)
import Data.Char (isLower)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  case (parse parseArcs "" input) of
    Left err -> error . show $ err
    Right arcs -> do { let vertexes = elems . S.fromList . concatMap (\arc -> [fst arc,snd arc]) $ arcs
                     ; let getDirection vertex (x,y) = if vertex == x then y else x
                     ; let adjacentMap = M.fromList [(\arcs' -> (vertex, map (getDirection vertex) arcs')) . filter (\(x,y) -> x == vertex || y == vertex) $ arcs | vertex <- vertexes] 
                     ; let smallCaves = S.fromList . filter (\x -> and (map isLower x)) $ vertexes
                     ; putStrLn . show . length . filter ((=="end") . last) $ process "start" "end" smallCaves adjacentMap
                     }

-- | extracts the arcs
parseArcs :: Parser [(String,String)]
parseArcs = do arcs <- many1 arc
               return arcs

-- | extracts an arc
arc :: Parser (String,String)
arc = do node1 <- many1 letter
         hyphen 
         node2 <- many1 letter
         (try eof >> return '#') <|> (try (char '\r') >> newline) <|> try (char '\n') -- we can find the end of input at the end of the line
         return (node1,node2)

-- | extracts a hyphen
hyphen :: Parser Char
hyphen = char '-'

-- | extracts a space
space :: Parser Char
space = char ' '

-- | Extracts the paths
process :: String -> String -> Set String -> Map String [String] -> [[String]]
process actual end smallCaves adjacentMap
  = if actual == end then
      [[end]]
    else 
      let adjacents = case M.lookup actual adjacentMap of
                        Nothing -> [[]]
                        Just adjs -> [if member actual smallCaves then 
                                        process adj end (S.delete actual smallCaves) (M.delete actual adjacentMap)
                                      else
                                        process adj end smallCaves adjacentMap | adj <- adjs]
      in map (actual:) . concat $ adjacents

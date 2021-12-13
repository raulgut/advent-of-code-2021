-----------------------------------------------------------------------------
-- | First part of Day 13
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, many, newline, try, string)
import Text.Parsec.Char (digit, char)
import Text.Parsec.Combinator (many1, sepEndBy, eof)
import Text.Parsec.Prim (parse, (<|>))
import Data.Set as S (Set, union, partition, fromList, map, size)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  case (parse parseTransparentPaper "" input) of
    Left err -> error . show $ err
    Right (coordinates,foldings) -> putStrLn . show . size . applyFolding (head foldings) . fromList  $ coordinates

-- | extracts the tranparent paper coordinates and foldings
parseTransparentPaper :: Parser ([(Int,Int)],[(String,Int)])
parseTransparentPaper = do coordinates <- many1 coordinate 
                           (try (char '\r') >> newline) <|> try (char '\n')
                           foldings <- many1 folding
                           return (coordinates,foldings)

-- | extracts coordinate
coordinate :: Parser (Int, Int)
coordinate = do n1 <- natural
                comma 
                n2 <- natural
                (try (char '\r') >> newline) <|> try (char '\n')
                return (n1,n2)

-- | extracts folding phase
folding :: Parser (String, Int)
folding = do str <- try (string "fold along x=" >> return "x") <|> (string "fold along y=" >> return "y")
             n2 <- natural 
             (try eof >> return '#') <|> (try (char '\r') >> newline) <|> try (char '\n') -- we can find the end of input at the end of the line
             return (str,n2)

-- | extracts a natural number with spaces
natural :: Parser Int
natural = do many space
             digits <- many1 digit
             return . read $ digits

-- | extracts a comma
comma :: Parser Char
comma = char ','

-- | extracts a space
space :: Parser Char
space = char ' '

-- | apply the given folding instruction
applyFolding :: (String,Int) -> Set (Int,Int) -> Set (Int, Int)
applyFolding ("y",value) grid = let (over,below) = partition ((> value) . snd) grid
                                in union below (S.map (\(x,y) -> (x,(value * 2) - y)) over)
applyFolding ("x",value) grid = let (right,left) = partition ((> value) . fst) grid
                                in union left (S.map (\(x,y) -> ((value * 2) - x,y)) right)

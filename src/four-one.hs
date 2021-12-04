-----------------------------------------------------------------------------
-- | First part of Day 4
--
-- Precondition: Board is a 5x5 matrix
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, many, newline, try)
import Text.Parsec.Error(ParseError (..))
import Text.Parsec.Char (digit, char)
import Text.Parsec.Combinator (many1, sepEndBy, eof)
import Text.Parsec.Prim (parse, (<|>))
import Data.Maybe (isJust, fromJust)
import Data.List (transpose)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  case (parse parseBingo "" input) of
    Left err -> error . show $ err
    Right (numbers,boards) -> putStrLn . show $ play (addColumns boards) numbers

-- | extracts the game and the boards
parseBingo :: Parser ([Int],[[[Int]]])
parseBingo = do numbers <- natural `sepEndBy` comma -- first, we parse the list of numbers
                (try (char '\r') >> newline) <|> try (char '\n')
                boards <- many1 parseBoards
                return (numbers,boards)

-- | extracts the boards
parseBoards :: Parser [[Int]]
parseBoards =
 do (try (char '\r') >> newline) <|> try (char '\n') -- the board starts with an empty line
    boards <- many1 parseLine
    return boards

-- | extracts the lines
parseLine :: Parser [Int]
parseLine = do line <- many1 natural
               (try eof >> return '#') <|> (try (char '\r') >> newline) <|> try (char '\n') -- we can find the end of input at the end of the line
               return line

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

-- | adds both directions (horizontal and vertical) to the board 
addColumns :: [[[Int]]] -> [[[Int]]]
addColumns [] = []
addColumns (b:bs) = (b ++ (transpose b)):(addColumns bs)

-- | plays the bingo
play :: [[[Int]]] -> [Int] -> Int
play boards [] = error "There is no winner!"
play boards (n:ns) = let newboards = map (map (filter (/= n))) boards
                         winner = getWinner newboards
                     in if isJust winner then 
                          (*n) . foldl (+) 0 . map (foldl (+) 0) . take 5 . fromJust $ winner
                        else 
                          play newboards ns

-- | checks if there is a winner
getWinner :: [[[Int]]] -> Maybe [[Int]]
getWinner [] = Nothing
getWinner (b:bs) = if (or . map null $ b) then 
                     Just b 
                   else 
                     getWinner bs 
-----------------------------------------------------------------------------
-- | First part of Day 21
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, many, newline, try, string)
import Text.Parsec.Error(ParseError (..))
import Text.Parsec.Char (digit, char)
import Text.Parsec.Combinator (many1, sepEndBy, eof)
import Text.Parsec.Prim (parse, (<|>))
import Control.Monad.State (State (..), get, put, runState)
import Data.Map as M (Map, fromList, insert, lookup)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

data Game = Game { rolls :: Int
                 , die :: [Int] 
                 , points :: Map Int Int 
                 }
                 deriving Show 

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  case (parse parsePlayers "" input) of
    Left err -> error . show $ err
    Right players -> putStrLn . show $ runState (play players) (Game 0 [1..100] (M.fromList [(i,0) | i <- [1..length players]]))

-- | extracts the player's initial positions
parsePlayers :: Parser [(Int,Int)]
parsePlayers = many1 parsePlayer

-- | extracts a player
parsePlayer :: Parser (Int,Int)
parsePlayer 
  = do { string "Player "
       ; n <- natural
       ; string " starting position: "
       ; p <- natural
       ; (try eof >> return '#') <|> (try (char '\r') >> newline) <|> try (char '\n') -- we can find the end of input at the end of the line
       ; return (n,p)
       }
-- | extracts a natural number with spaces
natural :: Parser Int
natural = do many space
             digits <- many1 digit
             return . read $ digits

-- | extracts a space
space :: Parser Char
space = char ' '

-- | play the game
play :: [(Int,Int)] -> State Game [(Int,Int)]
play ((player,position):players) 
  = do { game <- get 
       ; let (one:two:three:dice) = die game 
       ; let newPosition = case mod (sum [one,two,three] + position) 10 of 
                             0 -> 10 
                             other -> other                    
       ; let newPoints = case (M.lookup player (points game)) of
                           Just oldPoints -> oldPoints + newPosition
                           Nothing -> error $ "Player " ++ (show player) ++ " does not exist!"
       ; put game { rolls = (rolls game) + 3 , die = dice ++ [one,two,three], points = M.insert player newPoints (points game) }
       ; if newPoints >= 1000 then 
           return $ players ++ [(player,newPosition)]
         else 
           play $ players ++ [(player,newPosition)]
       }

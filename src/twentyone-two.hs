-----------------------------------------------------------------------------
-- | First part of Day 21
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, many, newline, try, string)
import Text.Parsec.Error(ParseError (..))
import Text.Parsec.Char (digit, char)
import Text.Parsec.Combinator (many1, sepEndBy, eof)
import Text.Parsec.Prim (parse, (<|>))
import Data.Map as M (Map, insert, empty, null, assocs, lookup)
import Data.MultiSet as Ms (MultiSet, empty, insertMany, insert, union)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  case (parse parsePlayers "" input) of
    Left err -> error . show $ err
    Right [(player1,pos1),(player2,pos2)] -> putStrLn . show $ getPossibilities 21 [1..3] $ M.insert ((player1,pos1),(player2,pos2)) (M.insert (0,0) 1 M.empty) M.empty

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

-- | creates all the possibilities
getPossibilities :: Int -> [Int] -> Map ((Int,Int),(Int,Int)) (Map (Int,Int) Int) -> MultiSet Int
getPossibilities end die options
  = if M.null options then 
      Ms.empty
    else 
      let permutations = concat . concat $ [[[[i,j,k]| k <- die] | j <- die] | i <- die]
          (finished,unfinished) = foldl (updateGames end permutations) (Ms.empty,M.empty) (M.assocs options)
          newFinished = getPossibilities end die unfinished
      in Ms.union finished newFinished

-- | adds the die rolls
getNewPosition list p = case mod (sum list + p) 10 of 
                          0 -> 10 
                          other -> other

-- | updates the games with the new rolls
updateGames :: Int -> [[Int]] -> (MultiSet Int, Map ((Int,Int),(Int,Int)) (Map (Int,Int) Int)) -> (((Int,Int),(Int,Int)),Map (Int, Int) Int) -> (MultiSet Int, Map ((Int,Int),(Int,Int)) (Map (Int,Int) Int))
updateGames end permutations (finished,unfinished) (((player1,pos1),(player2,pos2)),games)
  = let possibilities1 = [let newPos1 = getNewPosition i pos1
                          in ((player1,newPos1), [let newPos2 = getNewPosition j pos2
                                                  in (player2,newPos2) | j <- permutations]) | i <- permutations]
        newGames = concat [[(((player1,newPos1),possibilities2),((l + newPos1, r),number)) | ((l,r), number) <- assocs games] | ((player1,newPos1),possibilities2) <- possibilities1]                                 
    in foldl (processNewGames1 end) (finished,unfinished) newGames 

-- | if player 1 wins, it stops, if not, it is player 2 turn
processNewGames1 :: Int -> (MultiSet Int, Map ((Int,Int),(Int,Int)) (Map (Int,Int) Int)) -> (((Int,Int),[(Int,Int)]),((Int, Int),Int)) -> (MultiSet Int, Map ((Int,Int),(Int,Int)) (Map (Int,Int) Int))
processNewGames1 end (finished,unfinished) (((player1,newPos1),possibilities2),((newPoints1, oldPoints2),number))
  = if newPoints1 >= end then
      (Ms.insertMany player1 number finished,unfinished)
    else
      foldl (processNewGames2 end) (finished,unfinished) [(((player1,newPos1),(player2,newPos2)),((newPoints1, oldPoints2 + newPos2),number)) | (player2,newPos2) <- possibilities2]

-- | player 2 turn, continues if there is no winner
processNewGames2 :: Int -> (MultiSet Int, Map ((Int,Int),(Int,Int)) (Map (Int,Int) Int)) -> (((Int,Int),(Int,Int)),((Int, Int),Int)) -> (MultiSet Int, Map ((Int,Int),(Int,Int)) (Map (Int,Int) Int))
processNewGames2 end (finished,unfinished) (((player1,newPos1),(player2,newPos2)),((newPoints1, newPoints2),number))
  = if newPoints2 >= end then
      (Ms.insertMany player2 number finished, unfinished)
    else 
      case M.lookup ((player1,newPos1),(player2,newPos2)) unfinished of 
        Nothing -> (finished,M.insert ((player1,newPos1),(player2,newPos2)) (M.insert (newPoints1,newPoints2) number M.empty) unfinished)
        Just points -> case M.lookup (newPoints1,newPoints2) points of 
                          Nothing -> (finished,M.insert ((player1,newPos1),(player2,newPos2)) (M.insert (newPoints1,newPoints2) number points) unfinished)
                          Just oldNumbers -> (finished,M.insert ((player1,newPos1),(player2,newPos2)) (M.insert (newPoints1,newPoints2) (oldNumbers + number) points) unfinished)

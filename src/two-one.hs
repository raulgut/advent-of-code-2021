-----------------------------------------------------------------------------
-- | First part of Day 2
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, many)
import Text.Parsec.Error(ParseError (..))
import Text.Parsec.Char (digit, string)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (parse, (<|>))
import Control.Monad.State (State (..), get, put, execState)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Submarine instructions
data Instruction a 
  = Forward a 
  | Down a 
  | Up a 
  deriving Show

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  let instructions = (foldl extractInstructions [] . lines $ input) :: [Instruction Int]
  let (hpos, depth) = execState (processInstructions instructions) (0,0) --- (hpos, depth)
  putStrLn . show $ hpos * depth

-- | Parses instructions
extractInstructions :: [Instruction Int] -> String -> [Instruction Int]
extractInstructions is str
  = case (parse instructionParser "" str) of
      Left err -> error . show $ err
      Right i -> is ++ [i]

-- | parses forward, down and up instructions
instructionParser :: Parser (Instruction Int)
instructionParser = forwardParser <|> downParser <|> upParser

-- | forward instruction
forwardParser :: Parser (Instruction Int)
forwardParser =
 do string "forward "
    value <- (many1 digit)
    return $ Forward (read value)

-- | down instruction
downParser :: Parser (Instruction Int)
downParser =
 do string "down "
    value <- (many1 digit)
    return $ Down (read value)

-- | up instruction
upParser :: Parser (Instruction Int)
upParser =
 do string "up "
    value <- (many1 digit)
    return $ Up (read value)

-- | process instruction and update horizontal position and depth
processInstructions :: Num a => [Instruction a] -> State (a,a) ()
processInstructions [] = return ()
processInstructions ((Forward value):is) 
  = do { (hpos,depth) <- get
       ; put (hpos + value, depth)
       ; processInstructions is }
processInstructions ((Down value):is) 
  = do { (hpos,depth) <- get
       ; put (hpos, depth + value)
       ; processInstructions is }
processInstructions ((Up value):is) 
  = do { (hpos,depth) <- get
       ; put (hpos, depth - value)
       ; processInstructions is }

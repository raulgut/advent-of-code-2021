-----------------------------------------------------------------------------
-- | First part of Day 22
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, try, string, sepEndBy, many)
import Text.Parsec.Char (digit, char)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (parse, (<|>))
import Data.Set as S (Set, empty, union, fromList, difference, size)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Reboot step state
data Step = On
          | Off
          deriving Show

data Instruction = Instruction { step :: Step, xcoords :: (Int,Int), ycoords :: (Int,Int), zcoords :: (Int,Int)}
                 deriving Show


-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  let instructions = (map extractInstruction . lines $ input) :: [Instruction]
  putStrLn . show . S.size . foldl (processInstructions (-50 :: Int) 50) S.empty $ instructions

-- | Parses the entry
extractInstruction :: String -> Instruction
extractInstruction str
  = case (parse instructionParser "" str) of
      Left err -> error . show $ err 
      Right instruction -> instruction

-- | Parses a reboot step
instructionParser :: Parser Instruction
instructionParser
  = do { st <- try (string "on " >> return On) <|> (string "off " >> return Off)
       ; string "x="
       ; xleft <- integer
       ; string ".."
       ; xright <- integer
       ; string ",y="
       ; yleft <- integer
       ; string ".."
       ; yright <- integer
       ; string ",z="
       ; zleft <- integer
       ; string ".."
       ; zright <- integer
       ; return $ Instruction st (xleft,xright) (yleft,yright) (zleft,zright) 
       }

-- | extracts an integer number with spaces
integer :: Parser Int
integer = ((try (char '-')) >> many1 digit >>= (return . read . ('-':))) <|> (many1 digit >>= return . read)

-- | extracts a space
space :: Parser Char
space = char ' '

-- | executes the instructions
processInstructions :: Int -> Int -> Set (Int,Int,Int) -> Instruction -> Set (Int,Int,Int)
processInstructions lbound rbound area (Instruction On (x1,x2) (y1,y2) (z1,z2))
  = S.union area $ S.fromList [(x,y,z) | x <- [x1..x2], x >= lbound && x <= rbound
                                       , y <- [y1..y2], y >= lbound && y <= rbound
                                       , z <- [z1..z2], z >= lbound && z <= rbound]
processInstructions lbound rbound area (Instruction Off (x1,x2) (y1,y2) (z1,z2))
  = S.difference area $ S.fromList [(x,y,z) | x <- [x1..x2], x >= lbound && x <= rbound
                                            , y <- [y1..y2], y >= lbound && y <= rbound
                                            , z <- [z1..z2], z >= lbound && z <= rbound]
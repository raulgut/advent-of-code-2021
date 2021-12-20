-----------------------------------------------------------------------------
-- | Second part of Day 18
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, try, string, sepEndBy)
import Text.Parsec.Char (digit, char)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (parse, (<|>))
import Data.List (maximum)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Current State
data List a
  = List [List a] 
  | Number a
  deriving Eq

-----------------------------------------------------------------------------
-- Instances
-----------------------------------------------------------------------------

instance Show a => Show (List a) where
  show (List ls) = show ls
  show (Number i) = show i
-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  let lists = (map extractLists . lines $ input) :: [List Int]
  putStrLn . show . maximum . map getSum . allPairs $ lists

-- | Parses the entry
extractLists :: String -> List Int
extractLists str
  = case (parse listOrNumberParser "" str) of
      Left err -> error . show $ err 
      Right list -> list

-- | Parses a list or a number
listOrNumberParser :: Parser (List Int)
listOrNumberParser 
  = (try listParser) <|> numberParser 

-- | Parses a list
listParser :: Parser (List Int)
listParser
  = do { char '['
       ; ls <- listOrNumberParser `sepEndBy` comma
       ; char ']'
       ; return . List $ ls 
       }

-- | Parses a number
numberParser :: Parser (List Int)
numberParser = do { n <- many1 digit
                  ; return . Number . read $ n
                  }

-- | extracts a comma
comma :: Parser Char
comma = char ','

-- | adds two lists
addition :: List Int -> List Int -> List Int
addition ls1 ls2 = normalize $ List ([ls1] ++ [ls2])

-- | applies explosions and splitings
normalize :: List Int -> List Int
normalize ls = case explode 0 ls of 
                 Nothing -> case split ls of 
                              Nothing -> ls
                              Just splitted -> normalize splitted
                 Just (lvalue,result,rvalue) -> normalize result
  
-- | applies explosions
explode :: Int -> List Int -> Maybe (List Int, List Int, List Int)
explode 4 (List [lvalue,rvalue]) = Just (lvalue,Number 0,rvalue)
explode depth (List [ls1,ls2]) = case explode (depth + 1) ls1 of
                                   Nothing -> case  explode (depth + 1) ls2 of
                                                Nothing -> Nothing
                                                Just (lvalue, result, rvalue) -> Just (Number 0, List [(applyExplosionLeft lvalue ls1), result], rvalue)
                                   Just (lvalue, result, rvalue) -> Just (lvalue, List [result,(applyExplosionRight rvalue ls2)], Number 0)
explode depth (Number n) = Nothing

-- | applies the expansion of the explosion of a rhs
applyExplosionRight :: List Int -> List Int -> List Int
applyExplosionRight (Number n) (Number m) = Number $ n + m
applyExplosionRight (Number n) (List [ls1,ls2]) = List [applyExplosionRight (Number n) ls1, ls2]

-- | applies the expansion of the explosion of a lhs
applyExplosionLeft :: List Int -> List Int -> List Int
applyExplosionLeft (Number n) (Number m) = Number $ n + m
applyExplosionLeft (Number n) (List [ls1,ls2]) = List [ls1, applyExplosionLeft (Number n) ls2]

-- | applies splitings
split :: List Int -> Maybe (List Int)
split (List [lvalue,rvalue]) = case split lvalue of
                                 Nothing -> case split rvalue of 
                                              Nothing -> Nothing
                                              Just ls -> Just (List [lvalue,ls])
                                 Just ls -> Just (List [ls,rvalue])
split (Number n) = if n >= 10 then
                     Just (List [Number $ div n 2, Number $ (div n 2) + (rem n 2)])
                   else 
                     Nothing

-- | obtains the magnitude
getSum :: List Int -> Int
getSum (Number n) = n
getSum (List [ls1,ls2]) = (3 * getSum ls1) + (2 * getSum ls2)

-- | obtains all pairs
allPairs :: [List Int] -> [List Int]
allPairs [] = []
allPairs (ls1:rest) = map (\ls2 -> addition ls1 ls2) rest ++ map (\ls2 -> addition ls2 ls1) rest ++ allPairs rest

-----------------------------------------------------------------------------
-- | First part of Day 10
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Either (lefts)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let output = (map (check []) . lines $ input) :: [Either Char ()]
  putStrLn . show . sum . map eval . lefts $ output

-- | Checks correction
check :: String -> String -> Either Char ()
check _ [] = Right ()
check list ('(':str) = check (')':list) str
check list ('[':str) = check (']':list) str
check list ('{':str) = check ('}':list) str
check list ('<':str) = check ('>':list) str
check (')':list) (')':str) = check list str
check (']':list) (']':str) = check list str
check ('}':list) ('}':str) = check list str
check ('>':list) ('>':str) = check list str
check _ (c:str) = Left c

-- | Evaluate expression
eval :: Char -> Int
eval ')' = 3 
eval ']' = 57 
eval '}' = 1197
eval '>' = 25137
eval _ = 0
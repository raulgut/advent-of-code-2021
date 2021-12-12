-----------------------------------------------------------------------------
-- | First part of Day 10
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Either (rights)
import Data.List (sort)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let output = (map (check []) . lines $ input) :: [Either Char String]
  putStrLn . show . (\l -> l!!(div (length l) 2)) . sort . map (foldl eval 0) . rights $ output

-- | Extracts completion
check :: String -> String -> Either Char String
check list [] = Right list
check list ('(':str) = check (')':list) str
check list ('[':str) = check (']':list) str
check list ('{':str) = check ('}':list) str
check list ('<':str) = check ('>':list) str
check (')':list) (')':str) = check list str
check (']':list) (']':str) = check list str
check ('}':list) ('}':str) = check list str
check ('>':list) ('>':str) = check list str
check list ('\r':str) = check list str
check _ (c:str) = Left c

-- | Evaluate expression
eval :: Int -> Char -> Int
eval acc ')' = (acc * 5) + 1
eval acc ']' = (acc * 5) + 2
eval acc '}' = (acc * 5) + 3
eval acc '>' = (acc * 5) + 4

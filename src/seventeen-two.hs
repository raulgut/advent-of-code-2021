-----------------------------------------------------------------------------
-- | Second part of Day 17
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, try, string)
import Text.Parsec.Char (digit, char)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (parse, (<|>))
import Control.Monad (liftM, (>>=))
import Data.List (maximum, minimum, nub)

main = do
  args <- getArgs
  input <- readFile . head $ args
  let target = (extractTA $ input) :: ((Int,Int),(Int,Int))
  putStrLn . show . length . nub . concatMap snd . map (processValidXs (fst target)) . filter (not . null . snd) . processValidYs 1 (snd . fst $ target) . snd $ target

-- | Parses the entry
extractTA :: String -> ((Int,Int),(Int,Int))
extractTA str
  = case (parse targetParser "" str) of
      Left err -> error . show $ err 
      Right ta -> ta

-- | Parses the target area
targetParser :: Parser ((Int,Int),(Int,Int))
targetParser 
  = do { string "target area: x="
       ; x0 <- ((try (char '-')) >> many1 digit >>= (return . ('-':))) <|> many1 digit
       ; string ".." 
       ; x1 <- ((try (char '-')) >> many1 digit >>= (return . ('-':))) <|> many1 digit
       ; string ", y="
       ; y0 <- ((try (char '-')) >> many1 digit >>= (return . ('-':))) <|> many1 digit
       ; string ".."
       ; y1 <- ((try (char '-')) >> many1 digit >>= (return . ('-':))) <|> many1 digit
       ; return ((read x0,read x1),(read y0,read y1))
       }

-- | for each number of jumps i, calculate the possible Y values
processValidYs :: Int -> Int -> (Int,Int) -> [(Int,[Int])]
processValidYs i limit (minY,maxY) = let validYi = (i,[div distance i | value <- [minY..maxY], let distance = value + sum [0..(i - 1)], rem distance i == 0 ])
                                     in if i > (limit * 2) then
                                          []
                                        else
                                          validYi:processValidYs (i + 1) limit (minY,maxY)

-- | for each valid y, find valid xs
processValidXs :: (Int,Int) -> (Int,[Int]) -> (Int,[(Int,Int)])
processValidXs (minX, maxX) (i,[]) = (i,[])
processValidXs (minX, maxX) (i,y:ys) = let (_,vs) = processValidXs (minX, maxX) (i,ys)
                                       in (i,[(value,y) | value <- [0..maxX], let sumOption = sum [let result = value - j in if result <= 0 then 0 else result  | j <- [0..i - 1]], sumOption >= minX, sumOption <= maxX] ++ vs)


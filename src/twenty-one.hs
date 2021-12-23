-----------------------------------------------------------------------------
-- | First and second part of Day 20
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Map as M (Map, fromList, lookup, size, filter)

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  let steps = read . head $ args
  input <- readFile . head . tail $ args
  let (algorithm:_:image) = (map (Prelude.filter (\x -> x /= '\r')) . lines $ input) :: [String]
  let mappingAlgorithm = M.fromList . zip [0..] $ algorithm
  let mappingImage =  M.fromList $ [((i,j),(image!!i)!!j) | i <- [0..length image - 1], j <- [0..length (image!!i) - 1]]
  let infinite =  '.'
  putStrLn . show . size . M.filter (=='#') . enhance 1 steps (length image - 1) mappingAlgorithm infinite $ mappingImage

-- | apply the enhancing algorithm
enhance :: Int -> Int -> Int -> Map Int Char -> Char -> Map (Int,Int) Char -> Map (Int,Int) Char
enhance step end bound algorithm infinite image
    = let newImage = M.fromList [((i,j),pixel) | i <- [(0 - step)..(bound + step)]
                                                 , j <- [(0 - step)..(bound + step)]
                                                 , let pixel = getPixel algorithm infinite image (i,j)
                                                 ]
      in if step == end then 
        newImage 
      else 
        case M.lookup (binListToInt 0 . take 9 . repeat $ infinite) algorithm of
          Just '.' -> enhance (step + 1) end bound algorithm '.' newImage
          Just '#' -> enhance (step + 1) end bound algorithm '#' newImage

-- | get output pixel
getPixel :: Map Int Char -> Char -> Map (Int,Int) Char -> (Int,Int) -> Char 
getPixel algorithm infinite image (i,j)
  = let positions = [(i - 1, j - 1), (i - 1, j) , (i - 1, j + 1), (i, j - 1), (i, j), (i, j + 1), (i + 1, j - 1), (i + 1, j), (i + 1, j + 1)]
        n = binListToInt 0 . map (findPixel image infinite) $ positions
    in case M.lookup n algorithm of 
         Just c -> c 
         Nothing -> error $ "Number " ++ (show n) ++ " out of bounds!"

-- | find a pixel in the image
findPixel :: Map (Int,Int) Char -> Char -> (Int,Int) -> Char 
findPixel image infinite pos 
  = case M.lookup pos image of
      Just c -> c 
      Nothing -> infinite

-- | from a list in {'#', '.'} representing a binary number to Int
binListToInt :: Int -> String -> Int
binListToInt acc [] = acc
binListToInt acc ['#'] = acc * 2 + 1
binListToInt acc ['.'] = acc * 2
binListToInt acc ('#':bs) = binListToInt (acc * 2 + 1) bs
binListToInt acc ('.':bs) = binListToInt (acc * 2) bs
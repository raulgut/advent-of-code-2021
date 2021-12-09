-----------------------------------------------------------------------------
-- | Second part of Day 8
--
-- Precondition: Input is well-formed
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Set as S (Set, fromList, intersection, union, difference)
import Data.List (find)
import Data.Map as M (Map, lookup, fromList)
import Data.Maybe (fromJust)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Segments possibilities
data Segments
  = Segments { top :: Set Char
             , center :: Set Char
             , bottom :: Set Char
             , topLeft :: Set Char
             , topRight :: Set Char
             , bottomLeft :: Set Char 
             , bottomRight :: Set Char
             } deriving Show

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  let entries = map (span (/= "|") . words) . lines $ input
  let numbers = map fst $ entries
  let codes = map (tail . snd) $ entries
  let translation = map (translate . decrypt [2,3,4,5] initSegments) numbers
  putStrLn . show . foldl1 (+) . zipWith applyTranslation translation $ codes

-- | initial state is all possibilities
initSegments :: Segments
initSegments = let options = S.fromList ['a'..'g'] in Segments options options options options options options options

-- | process the options in order
decrypt :: [Int] -> Segments -> [String] -> Segments
decrypt [] segments _ = segments
decrypt (2:rest) segments numbers -- number one
  = let (Just number) = find (\x -> length x == 2) numbers
        numberSet = S.fromList number
    in decrypt rest 
               segments { topRight = intersection numberSet (topRight segments)
                        , bottomRight = intersection numberSet (bottomRight segments)
                        } 
               numbers
decrypt (3:rest) segments numbers -- number 7
  = let (Just number) = find (\x -> length x == 3) numbers
        numberSet = S.fromList number
    in decrypt rest 
               segments { top = difference numberSet (topRight segments) -- 1 and 7 have two segments that are equal
                        }
               numbers
decrypt (4:rest) segments numbers -- number 4
  = let (Just number) = find (\x -> length x == 4) numbers
        numberSet = S.fromList number
    in decrypt rest
               segments { center = difference numberSet (topRight segments) -- remove segments from number 1
                        , topLeft = difference numberSet (topRight segments) -- remove segments from number 1
                        }
               numbers
decrypt (5:rest) segments numbers -- numbers 2, 3 and 5
  = let fiveSegments = filter (\x -> length x == 5) numbers
        numberSets = map S.fromList fiveSegments        
        commonSegments = foldl1 intersection numberSets -- common segments in 2, 3 and 5
        newCenter = intersection (center segments) commonSegments -- common segment in 2, 3, 4 and 5
        newBottom = difference commonSegments (union newCenter (top segments)) -- common segment in 2, 3, 4 and 5
        newTopLeft = difference (topLeft segments) newCenter
        (Just number5) = find (\x -> not . null . intersection newTopLeft $ x) numberSets
        newBottomRight = difference number5 (foldl1 union [commonSegments,newTopLeft])
        newTopRight = difference (topRight segments) newBottomRight
        newBottomLeft = difference (foldl1 union numberSets) (foldl1 union [top segments, newCenter, newBottom, newTopLeft, newTopRight, newBottomRight])
    in decrypt rest
               segments { center = newCenter
                        , bottom = newBottom
                        , topLeft = newTopLeft
                        , bottomRight = newBottomRight
                        , topRight = newTopRight
                        , bottomLeft = newBottomLeft
                        } 
               numbers     
decrypt (_:rest) segments numbers = decrypt rest segments numbers

-- | translate the segments into numbers
translate :: Segments -> Map (Set Char) Int
translate segments = M.fromList 
                       [(foldl1 union [top segments, bottom segments, topLeft segments, topRight segments, bottomLeft segments, bottomRight segments],0)
                       ,(foldl1 union [topRight segments, bottomRight segments],1)
                       ,(foldl1 union [top segments, center segments, bottom segments, topRight segments, bottomLeft segments],2)
                       ,(foldl1 union [top segments, center segments, bottom segments, topRight segments, bottomRight segments],3)
                       ,(foldl1 union [center segments, topLeft segments, topRight segments, bottomRight segments],4)
                       ,(foldl1 union [top segments, center segments, bottom segments, topLeft segments, bottomRight segments],5)
                       ,(foldl1 union [top segments, center segments, bottom segments, topLeft segments, bottomLeft segments, bottomRight segments],6)
                       ,(foldl1 union [top segments, topRight segments, bottomRight segments],7)
                       ,(foldl1 union [top segments, center segments, bottom segments, topLeft segments, topRight segments, bottomLeft segments, bottomRight segments],8)
                       ,(foldl1 union [top segments, center segments, bottom segments, topLeft segments, topRight segments, bottomRight segments],9)
                       ]

-- | apply the translation
applyTranslation :: Map (Set Char) Int -> [String] -> Int
applyTranslation translation strs = foldl (\x y -> (x * 10) + y) 0 [fromJust . M.lookup (S.fromList str) $ translation | str <- strs] 


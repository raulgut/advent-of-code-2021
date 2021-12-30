-----------------------------------------------------------------------------
-- | First part of Day 19
--
-- This can be simplified a lot!
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, many, newline, try, string)
import Text.Parsec.Error(ParseError (..))
import Text.Parsec.Char (digit, char)
import Text.Parsec.Combinator (many1, sepEndBy, eof)
import Text.Parsec.Prim (parse, (<|>))
import Data.Map as M (Map, fromList, insert, lookup, empty, keys, elems)
import Data.Maybe (catMaybes)
import Data.Set as S (Set, fromList, intersection, size, elems, union
                     , empty, insert, difference)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

data Scanners = Scanners { scans :: Map Int [((Int,Int,Int), [(Int,Int,Int)])]
                         , beacons :: Set Beacons
                         }
                         deriving Show

type Beacons = (((Int,Int,Int),(Int,Int,Int)), Set (Int,Int,Int))

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  case (parse parseScanners "" input) of
    Left err -> error . show $ err
    Right (s:scanners) -> putStrLn . show . size . foldl1 union . map (S.fromList . computeBeacons) . concat . M.elems . scans . analyzeSensors (Scanners (M.fromList [(fst s,[((0,0,0),snd s)])]) S.empty) S.empty . map (\(x,y) -> (x, getOrientations y)) $ scanners

-- | extracts the scanners
parseScanners :: Parser [(Int,[(Int,Int,Int)])]
parseScanners = many1 parseScanner

-- | extracts a scanner
parseScanner :: Parser (Int,[(Int,Int,Int)])
parseScanner 
  = do { (try (char '\r') >> newline >> string "--- scanner") <|> (try (char '\n') >> string "--- scanner") <|> string "--- scanner"
       ; n <- natural
       ; string " ---" 
       ; (try (char '\r') >> newline) <|> try (char '\n') -- the board starts with an empty line
       ; ps <- many1 parseCoordinates
       ; return (n, ps)
       }

-- | extracts the coordinates
parseCoordinates :: Parser (Int,Int,Int)
parseCoordinates 
  = do { x <- integer
       ; comma
       ; y <- integer
       ; comma 
       ; z <- integer    
       ; (try eof >> return '#') <|> (try (char '\r') >> newline) <|> try (char '\n') -- we can find the end of input at the end of the line
       ; return (x, y, z)
       }
       
-- | extracts a natural number with spaces
natural :: Parser Int
natural = do many space
             digits <- many1 digit
             return . read $ digits

-- | extracts an integer number with spaces
integer :: Parser Int
integer = ((try (char '-')) >> many1 digit >>= (return . read . ('-':))) <|> (many1 digit >>= return . read)

-- | extracts a comma
comma :: Parser Char
comma = char ','

-- | extracts a space
space :: Parser Char
space = char ' '

-- | gets all the 24? orientations (we consider all combinations)
getOrientations :: [(Int,Int,Int)] -> [[(Int,Int,Int)]]
getOrientations ls = let possibilities = [g . f | f <- [(\(x,y,z) -> (x,y,z)),(\(x,y,z) -> (x,z,y)),(\(x,y,z) -> (y,x,z)),(\(x,y,z) -> (z,x,y)),(\(x,y,z) -> (y,z,x)),(\(x,y,z) -> (z,y,x))]
                                          --      , g <- [(\(x,y,z) -> (x,y,z)),(\(x,y,z) -> (y,-1 * x,z)), (\(x,y,z) -> (-1 * x,-1 * y,z)),(\(x,y,z) -> (-1 * y,x,z))]]
                                                , g <- [(\(x,y,z) -> (x,y,z)),(\(x,y,z) -> (x, y,-1 * z)), (\(x,y,z) -> (x,-1 * y,z)),(\(x,y,z) -> (x,-1 * y,-1 * z)),(\(x,y,z) -> (-1 * x,y,z)), (\(x,y,z) -> (-1 * x,y,-1 * z)), (\(x,y,z) -> (-1 * x,-1 * y,z)), (\(x,y,z) -> (-1 * x,-1 * y,-1 * z))]]
                     in map (\x -> map x ls) possibilities

-- | checks possible orientations for each scanner
analyzeSensors :: Scanners -> Set Int -> [(Int,[[(Int,Int,Int)]])] -> Scanners
analyzeSensors scanner visited scanners 
  = let nonVisited = S.elems . (\s -> S.difference s visited) . S.fromList . keys . scans $ scanner
    in if null nonVisited then 
         scanner
       else 
         let next = head nonVisited
             dropFirst _ [] = []
             dropFirst cond (x:xs) = if cond x then xs else (x:dropFirst cond xs)
             rest = dropFirst (\x -> fst x == next) scanners
             newScanner = foldl (checkOrientations next) scanner rest
         in analyzeSensors newScanner (S.insert next visited) rest


-- | checks possible orientations for each scanner
checkOrientations :: Int -> Scanners -> (Int,[[(Int,Int,Int)]]) -> Scanners
checkOrientations m scanner (n,[]) = scanner
checkOrientations m scanner (n,orientation:orientations)
  = case getValidOrientation scanner m orientation of
      Just possibilities -> let withoutBeacons = map (\(x,y,z) -> (x,z)) possibilities
                                iBeacons = S.fromList . map (\(x,y,z) -> y) $ possibilities
                            in case M.lookup n (scans scanner) of
                                 Just oldPossibilities -> checkOrientations m (scanner { scans = M.insert n (oldPossibilities ++ withoutBeacons) (scans scanner) 
                                                                                       , beacons = S.union iBeacons (beacons scanner)}) (n,orientations)
                                 Nothing -> checkOrientations m (scanner { scans = M.insert n withoutBeacons (scans scanner)
                                                                         , beacons = S.union iBeacons (beacons scanner)}) (n,orientations)
      Nothing -> checkOrientations m scanner (n,orientations)

-- | checks if it is there is a possible valid orientation 
getValidOrientation :: Scanners -> Int -> [(Int,Int,Int)] -> Maybe [((Int,Int,Int), Beacons, [(Int,Int,Int)])]
getValidOrientation scanner n possibility 
  = case M.lookup n (scans scanner) of
      Just options -> checkValidOptions options possibility  
      Nothing -> error $ "Scanner " ++ (show n) ++ " has no valid coordinates."

-- |
checkValidOptions :: [((Int,Int,Int), [(Int,Int,Int)])] -> [(Int,Int,Int)] -> Maybe [((Int,Int,Int), Beacons,[(Int,Int,Int)])]
checkValidOptions [] _ = Nothing 
checkValidOptions (option:options) possibility = case checkValidOption option possibility of 
                                                   Just possibleSolution -> case checkValidOptions options possibility of 
                                                                              Just possibleSolutions -> Just $ possibleSolution ++ possibleSolutions 
                                                                              Nothing -> Just possibleSolution
                                                   Nothing -> checkValidOptions options possibility 

-- |
checkValidOption :: ((Int,Int,Int), [(Int,Int,Int)]) -> [(Int,Int,Int)] -> Maybe [((Int,Int,Int),Beacons,[(Int,Int,Int)])]
checkValidOption (n,nBeacons) mBeacons = let possibleCordinates = S.elems . S.fromList . concat $ [map (getPossibleCoordinates n beacon) mBeacons | beacon <- nBeacons]
                                         in case catMaybes . map (isValidCoordinates n nBeacons mBeacons) $ possibleCordinates of 
                                              [] -> Nothing 
                                              validCoordinates -> Just validCoordinates

-- |
getPossibleCoordinates :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
getPossibleCoordinates (x0,y0,z0) (x1,y1,z1) (x2,y2,z2) = (x0 + x1 - x2, y0 + y1 - y2, z0 + z1 - z2) 

-- |
isValidCoordinates :: (Int,Int,Int) -> [(Int,Int,Int)] -> [(Int,Int,Int)] -> (Int,Int,Int) -> Maybe ((Int,Int,Int),Beacons,[(Int,Int,Int)])
isValidCoordinates n nBeacons mBeacons m = let tupleSum = (\(x0,y0,z0) (x1,y1,z1) -> (x0 + x1, y0 + y1, z0 + z1))
                                               nBeaconSet = S.fromList . map (tupleSum n) $ nBeacons 
                                               mBeaconSet = S.fromList . map (tupleSum m) $ mBeacons
                                               iBeacons = intersection nBeaconSet mBeaconSet 
                                           in if (size iBeacons) >= 12 then
                                                Just (m,((n,m),iBeacons), mBeacons)
                                              else
                                                Nothing

-- |
computeBeacons :: ((Int,Int,Int),[(Int,Int,Int)]) -> [(Int,Int,Int)]
computeBeacons (sensor,sensorBeacons) = let addTriple (x0,y0,z0) (x1,y1,z1) = (x0 + x1, y0 + y1, z0 + z1)
                                        in map (addTriple sensor) sensorBeacons
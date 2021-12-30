-----------------------------------------------------------------------------
-- | First part of Day 22
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec (Parser, try, string, sepEndBy, many)
import Text.Parsec.Char (digit, char)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (parse, (<|>))

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

-- | Reboot step state
data Step = On
          | Off
          deriving Show

-- | Cuboid Coordinates
data Cuboid = Cuboid { xcoords :: (Int,Int)
                     , ycoords :: (Int,Int)
                     , zcoords :: (Int,Int)
                     } 
            deriving Show

-- | Instruction information
data Instruction = Instruction { step :: Step
                               , cuboid :: Cuboid 
                               }
                 deriving Show


-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  let instructions = (map extractInstruction . lines $ input) :: [Instruction]
  putStrLn . show . foldl getArea 0 . foldl processInstructions [] $ instructions

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
       ; return $ Instruction st (Cuboid (xleft,xright) (yleft,yright) (zleft,zright))
       }

-- | extracts an integer number with spaces
integer :: Parser Int
integer = ((try (char '-')) >> many1 digit >>= (return . read . ('-':))) <|> (many1 digit >>= return . read)

-- | extracts a space
space :: Parser Char
space = char ' '

-- | executes the instructions
processInstructions :: [Cuboid] -> Instruction -> [Cuboid]
processInstructions cuboids (Instruction On cub)
  = cub:(concatMap (removeCuboidX cub) cuboids)
processInstructions cuboids (Instruction Off cub)
  = concatMap (removeCuboidX cub) cuboids

-- | removes intersection between two cuboids - coordinate x
removeCuboidX :: Cuboid -> Cuboid -> [Cuboid]
removeCuboidX (Cuboid (x1,x2) (y1,y2) (z1,z2)) (Cuboid (x3,x4) (y3,y4) (z3,z4)) 
  = if (x1 >= x3) && (x1 <= x4) then -- x1 is between x3 and x4
      if (x2 >= x4) then -- the resulting cuboid is x3..(x1 - 1)
        removeCuboidY [(x3,(x1 - 1))] (Cuboid (x1,x2) (y1,y2) (z1,z2)) (Cuboid (x3,x4) (y3,y4) (z3,z4))
      else -- the resulting cuboids are split in two 
        removeCuboidY [(x3,(x1 - 1)), ((x2 + 1),x4)] (Cuboid (x1,x2) (y1,y2) (z1,z2)) (Cuboid (x3,x4) (y3,y4) (z3,z4))
    else -- x2 is between x3 and x4 or there is no overlap
      if (x2 >= x3) && (x2 <= x4) && (x1 <= x3) then -- the resulting cuboid is (x2 + 1)..x4
        removeCuboidY [((x2 + 1),x4)] (Cuboid (x1,x2) (y1,y2) (z1,z2)) (Cuboid (x3,x4) (y3,y4) (z3,z4))
      else 
        if (x1 <= x3) && (x2 >= x4) then -- everithing is overlap
          removeCuboidY [] (Cuboid (x1,x2) (y1,y2) (z1,z2)) (Cuboid (x3,x4) (y3,y4) (z3,z4))
        else -- there is no overlap
          [(Cuboid (x3,x4) (y3,y4) (z3,z4))]

-- | removes intersection between two cuboids - coordinate y
removeCuboidY :: [(Int,Int)] -> Cuboid -> Cuboid -> [Cuboid]
removeCuboidY xcuts (Cuboid (x1,x2) (y1,y2) (z1,z2)) (Cuboid (x3,x4) (y3,y4) (z3,z4)) 
  = if (y1 >= y3) && (y1 <= y4) then -- y1 is between y3 and y4
      if (y2 >= y4) then -- the resulting cuboid is y3..(y1 - 1)
        removeCuboidZ xcuts [(y3,(y1 - 1))] (Cuboid (x1,x2) (y1,y2) (z1,z2)) (Cuboid (x3,x4) (y3,y4) (z3,z4))
      else -- the resulting cuboids are split in two 
        removeCuboidZ xcuts [(y3,(y1 - 1)), ((y2 + 1),y4)] (Cuboid (x1,x2) (y1,y2) (z1,z2)) (Cuboid (x3,x4) (y3,y4) (z3,z4))
    else -- y2 is between y3 and y4 or there is no overlap
      if (y2 >= y3) && (y2 <= y4) && (y1 <= y3) then -- the resulting cuboid is (y2 + 1)..y4
        removeCuboidZ xcuts [((y2 + 1),y4)] (Cuboid (x1,x2) (y1,y2) (z1,z2)) (Cuboid (x3,x4) (y3,y4) (z3,z4))
      else -- everithing is overlap
        if (y1 <= y3) && (y2 >= y4) then
          removeCuboidZ xcuts [] (Cuboid (x1,x2) (y1,y2) (z1,z2)) (Cuboid (x3,x4) (y3,y4) (z3,z4))
        else -- there is no overlap
          [(Cuboid (x3,x4) (y3,y4) (z3,z4))]

-- | removes intersection between two cuboids - coordinate z
removeCuboidZ :: [(Int,Int)] -> [(Int,Int)] -> Cuboid -> Cuboid -> [Cuboid]
removeCuboidZ xcuts ycuts (Cuboid (x1,x2) (y1,y2) (z1,z2)) (Cuboid (x3,x4) (y3,y4) (z3,z4)) 
  = if (z1 >= z3) && (z1 <= z4) then -- z1 is between z3 and z4
      if (z2 >= z4) then -- the resulting cuboid is z3..(z1 - 1)
        removeCuboidCuts xcuts ycuts [(z3,(z1 - 1))] (Cuboid (x1,x2) (y1,y2) (z1,z2)) (Cuboid (x3,x4) (y3,y4) (z3,z4))
      else -- the resulting cuboids are split in two 
        removeCuboidCuts xcuts ycuts [(z3,(z1 - 1)), ((z2 + 1),z4)] (Cuboid (x1,x2) (y1,y2) (z1,z2)) (Cuboid (x3,x4) (y3,y4) (z3,z4))
    else -- z2 is between z3 and z4 or there is no overlap
      if (z2 >= z3) && (z2 <= z4) && (z1 <= z3) then -- the resulting cuboid is (z2 + 1)..z4
        removeCuboidCuts xcuts ycuts [((z2 + 1),z4)] (Cuboid (x1,x2) (y1,y2) (z1,z2)) (Cuboid (x3,x4) (y3,y4) (z3,z4))
      else -- everithing is overlap
        if (z1 <= z3) && (z2 >= z4) then
          removeCuboidCuts xcuts ycuts [] (Cuboid (x1,x2) (y1,y2) (z1,z2)) (Cuboid (x3,x4) (y3,y4) (z3,z4))
        else -- there is no overlap
          [(Cuboid (x3,x4) (y3,y4) (z3,z4))]

-- | removes intersection between two cuboids given the cuts
removeCuboidCuts :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)] -> Cuboid -> Cuboid -> [Cuboid]
removeCuboidCuts xcuts ycuts zcuts c1@(Cuboid (x1,x2) (y1,y2) (z1,z2)) c2@(Cuboid (x3,x4) (y3,y4) (z3,z4)) 
  = [if zcut1 == z3 then Cuboid (x3,x4) (y3,y4) (z3,zcut2) else Cuboid (x3,x4) (y3,y4) (zcut1,z4) | (zcut1,zcut2) <- zcuts]
      ++ [if ycut1 == y3 then Cuboid (x3,x4) (y3,ycut2) (if z1 < z3 then z3 else z1,if z2 > z4 then z4 else z2) else Cuboid (x3,x4) (ycut1,y4) (if z1 < z3 then z3 else z1,if z2 > z4 then z4 else z2) | (ycut1,ycut2) <- ycuts]
      ++ [if xcut1 == x3 then Cuboid (x3,xcut2) (if y1 < y3 then y3 else y1,if y2 > y4 then y4 else y2) (if z1 < z3 then z3 else z1,if z2 > z4 then z4 else z2) else Cuboid (xcut1,x4) (if y1 < y3 then y3 else y1,if y2 > y4 then y4 else y2) (if z1 < z3 then z3 else z1,if z2 > z4 then z4 else z2) | (xcut1,xcut2) <- xcuts]

-- | gets the area of a cuboid
getArea :: Int -> Cuboid -> Int
getArea acc (Cuboid (x1,x2) (y1,y2) (z1,z2))
  = acc + ((abs $ x2 - x1 + 1) * (abs $ y2 - y1 + 1) * (abs $ z2 - z1 + 1))
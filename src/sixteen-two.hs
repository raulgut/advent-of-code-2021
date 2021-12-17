-----------------------------------------------------------------------------
-- | Second part of Day 16
-----------------------------------------------------------------------------

import System.Environment (getArgs)
import Control.Monad.State (State (..), get, put, evalState)

-----------------------------------------------------------------------------
-- Data
-----------------------------------------------------------------------------

data Packet = Literal { pversion :: String
                      , pblocks :: Int
                      , pvalue :: String
                      }
            | Operator { pversion :: String
                       , ptype :: String
                       , plengthtype :: Char
                       , plist :: [Packet]
                       }
            deriving Show

-----------------------------------------------------------------------------
-- Functions
-----------------------------------------------------------------------------

main = do
  args <- getArgs
  input <- readFile . head $ args
  let packets = (map (translate . concatMap getBinary) . map (filter (\x -> x /= '\r')) . lines $ input) :: [Packet]
  putStrLn . show . sum . map eval $ packets

-- | translates a String into a Packet
translate :: String -> Packet 
translate bits = let (version,bits3) = splitAt 3 bits
                     (packetId,bits6) = splitAt 3 bits3
                 in case packetId of
                      "100" -> let (blocks,value) = getLiteral 0 bits6 in Literal { pversion = version, pblocks = blocks, pvalue = value}
                      operator -> let (pltype,packets) = extractPackets bits6 
                                  in Operator { pversion = version , ptype = operator, plengthtype = pltype, plist = packets } 

-- | the packet is a literal
getLiteral :: Int -> String -> (Int, String)
getLiteral blocks str = let (v:value,rest) = splitAt 5 str
                        in if v == '0' then
                              (blocks + 1, value)
                            else -- v == '1'
                              let (newBlocks, restLiteral) = getLiteral (blocks + 1) rest in (newBlocks, value ++ restLiteral) 

-- | extracts packets in an Operator
extractPackets :: String -> (Char, [Packet])
extractPackets (i:str) = if i == '0' then
                           let (lPacketList, packetList) = splitAt 15 str
                           in (i,processLPackets (fromBinaryToInt 0 lPacketList) packetList)
                         else -- i == 1
                           let (nPacketList, packetList) = splitAt 11 str 
                           in (i,processNPackets (fromBinaryToInt 0 nPacketList) packetList)

-- | the packets have a total size
processLPackets len packetList = let packet = translate packetList
                                     pSize = getSize packet
                                 in if len - pSize  == 0 then 
                                      [packet]
                                    else
                                      packet:(processLPackets (len - pSize) (drop pSize packetList))

-- | the packets have a total number
processNPackets n packetList = let packet = translate packetList
                                   pSize = getSize packet
                               in if n == 1 then 
                                      [packet]
                                    else 
                                      packet:(processNPackets (n - 1) (drop pSize packetList))

-- | extracts the size of the packet
getSize p@(Literal {}) = 6 + (let lpvalue = length . pvalue $ p in lpvalue + pblocks p)
getSize p@(Operator {}) = 7 + (if (plengthtype p) == '0' then 15 else 11) + (sum . map getSize . plist $ p)

-- | evaluates the packet
eval :: Packet -> Int
eval p@(Literal {}) = fromBinaryToInt 0 . pvalue $ p
eval p@(Operator {}) = let values = map eval (plist p)
                       in case (ptype p) of
                            "000" -> sum values 
                            "001" -> product values
                            "010" -> minimum values
                            "011" -> maximum values
                            "101" -> if (head values) > (last values) then 1 else 0
                            "110" -> if (head values) < (last values) then 1 else 0
                            "111" -> if (head values) == (last values) then 1 else 0

-- | from hexadecimal char to 4-digit binary number
getBinary :: Char -> String
getBinary '0' = "0000"
getBinary '1' = "0001"
getBinary '2' = "0010"
getBinary '3' = "0011"
getBinary '4' = "0100"
getBinary '5' = "0101"
getBinary '6' = "0110"
getBinary '7' = "0111"
getBinary '8' = "1000"
getBinary '9' = "1001"
getBinary 'A' = "1010"
getBinary 'B' = "1011"
getBinary 'C' = "1100"
getBinary 'D' = "1101"
getBinary 'E' = "1110"
getBinary 'F' = "1111"
getBinary str = error $ str:" is not a valid hexadecimal value"

-- | from a list of numbers in {0, 1} representing a binary number to Int
fromBinaryToInt :: Int -> String -> Int
fromBinaryToInt acc [] = acc
fromBinaryToInt acc ('0':bs) = fromBinaryToInt (acc * 2) bs
fromBinaryToInt acc ('1':bs) = fromBinaryToInt (acc * 2 + 1) bs

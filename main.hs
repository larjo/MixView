import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Word
import Control.Applicative

-- The datastructure that represents a plylist in Riff-format
type Format = String
type Id = String
type RawData = String --type RawData = BL.ByteString

data Chunk = Data Id RawData | List Chunks
data Chunks = Chunks Format [Chunk]
data Riff = Riff Chunks

-- functions for pretty printing the playlist
instance Show Chunk where
   show (Data i d) = "(" ++ i ++ ":" ++ d ++ ")"   
   show (List cs) = show cs

instance Show Chunks where
   show (Chunks f c) = f ++ ":" ++ (show c)
  
instance Show Riff where
   show (Riff cs) = show cs

-- low level parsing functions
parseFourCC :: Get String
parseFourCC = B8.unpack <$> getByteString 4

parseLength :: Get Int
parseLength = fromIntegral <$> getWord32le

parseData :: Int -> Get B.ByteString
parseData len = do
  dat <- getByteString len
  getByteString $ len `mod` 2 -- skip padding
  return dat

parse :: Get (String, Int, String)
parse = do
  h <- parseFourCC
  l <- parseLength
  h2 <- parseFourCC
  return (h, l, h2)
main :: IO ()
main = do
  input <- BL.getContents
  let r = runGet parse input
  print r
  
test = show r 
  where
    c = Data "DAT1" "sdsdf"
    c2 = Data "DAT2"  "sdfsdaf"
    cs = Chunks "CHKS" [c, c2]
    c3 = List cs
    c4 = Data "DAT3" "sdfasdf"
    r = Riff (Chunks "HEAD" [c3,c4])
    
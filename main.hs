import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Word
import Control.Applicative

  
-- The datastructure that represents a playlist in Riff-format
type Format = String
type Id = String
type RawData = BL.ByteString

data Chunk = Data Id RawData | List Chunks
data Chunks = Chunks Format [Chunk] 
data Riff = Riff Chunks

-- functions for pretty printing the playlist
instance Show Chunk where
   show (Data i d) = "(" ++ i ++ ")"   
   show cs = show cs

instance Show Chunks where
   show (Chunks f c) = f ++ ":" ++ (show c)
  
instance Show Riff where
   show (Riff cs) = show cs

-- low level parsing functions
parseFourCC :: Get String
parseFourCC = B8.unpack <$> getByteString 4

parseLength :: Get Int
parseLength = fromIntegral <$> getWord32le
  
parseLazyData :: Int -> Get RawData
parseLazyData len = do
  rawdata <- getLazyByteString (fromIntegral len)
  skip $ len `mod` 2 -- skip one byte padding if length is odd
  return rawdata

parseChunk :: Id -> Get Chunk
parseChunk "LIST" = do
  len <- parseLength
  sub <- parseFourCC
  dat <- parseRawData (len - 4)
  return (Data sub dat)
  -- runGet parseChunks dat
  
parseChunk id = do
  len <- parseLength
  rawData <- parseRawData len
  return (Data id rawData)

parseChunks :: Get Chunks
parseChunks = do
  subFormat <- parseFourCC
  
  len <- parseLength
  rawData <- parseRawData len
  return (Chunks format chunks)

  
parse :: Get (Chunk)
parse = do
  id <- parseFourCC
  d <- parseChunk id
  return (d)
  
main :: IO ()
main = BL.getContents >>= print . runGet parse
    
-- test = show r 
  -- where
    -- d = B8.pack "data"
    -- c = Data "DAT1" d
    -- c2 = Data "DAT2"  d
    -- cs = Chunks "CHKS" [c, c2]
    -- c3 = List cs
    -- c4 = Data "DAT3" d
    -- r = Riff (Chunks "HEAD" [c3,c4])
    
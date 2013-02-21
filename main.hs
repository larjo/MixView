import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
--import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Word
import Control.Applicative

type Format = String
type Id = String
type RawData = String
--type RawData = BL.ByteString

data Chunk = Data Id RawData | List Chunks
data Chunks = Chunks Format [Chunk]
data Riff = Riff Chunks

instance Show Chunk where
   show (Data i d) = "(" ++ i ++ ":" ++ d ++ ")"   
   show (List cs) = show cs

instance Show Chunks where
   show (Chunks f c) = f ++ ":" ++ (show c)
  
instance Show Riff where
   show (Riff cs) = show cs

parseRawHeader :: Get (String, Int)
parseRawHeader = do
  ident <- B8.unpack <$> getByteString 4
  len <- fromIntegral <$> getWord32le
  return (ident, len)

parseRawFormat :: Get (String)
parseRawHeader = do
  ident <- B8.unpack <$> getByteString 4
  return ident

parseRawFormat :: Get (String)
parseRawHeader = do
  ident <- B8.unpack <$> getByteString 4
  return ident

  
parseRawChunk :: Get (String, String, BL.ByteString)
parseRawHeader = do
  ident <- B8.unpack <$> getByteString 4
  len <- fromIntegral <$> getWord32le
  format <- B8.unpack <$> getByteString 4
  dat <- getLazyByteString (len - 4)
  getLazyByteString $ len `mod` 2
  return (ident, format, dat)

  
main :: IO ()
main = do
  input <- BL.getContents
  let (s1, s2, d1) = runGet parseRawHeader input
  let (s3, _d2) = runGet parseRawData d1
  print (s1, s2, s3)
  
test = show r 
  where
    c = Data "DAT1" "sdsdf"
    c2 = Data "DAT2"  "sdfsdaf"
    cs = Chunks "CHKS" [c, c2]
    c3 = List cs
    c4 = Data "DAT3" "sdfasdf"
    r = Riff (Chunks "HEAD" [c3,c4])
    
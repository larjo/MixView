module RiffTokens
(parseTokens) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Word
import Control.Applicative
import Data.Text as T
import Data.Text.Encoding as E

--tokens
data Token = Id String | Len Int | Sub String | RawData B.ByteString

decode s = if B.length s `notElem` [8, 12, 32, 56, 86]
              then show $ T.init $ E.decodeUtf16LEWith onerr s
              else ""
           where onerr _ _ = Nothing
           
instance Show Token where
   show (Id s) = "Id " ++ s
   show (Len i) = "Len " ++ show i
   show (Sub s) = "Sub " ++ s
   show (RawData d) = "Data " ++ decode d
   
--parse tokens
parseFourCC :: Get String
parseFourCC = B8.unpack <$> getByteString 4

parseInt:: Get Int
parseInt = fromIntegral <$> getWord32le

parseRawData :: Int -> Get B.ByteString
parseRawData len = do
  rawdata <- getByteString len
  skip $ len `mod` 2 -- skip one byte padding if length is odd
  return rawdata

parseList :: Get [Token] 
parseList = do
  len <- parseInt
  sub <- parseFourCC
  rest <- parseTokens
  return (Len len : Sub sub : rest)  

parseData :: Get [Token] 
parseData = do
  len <- parseInt 
  raw <- parseRawData len
  rest <- parseTokens
  return (Len len : RawData raw : rest)

parseChunks :: Get [Token]  
parseChunks = do
  id <- parseFourCC
  rest <- parseChunks' id
  return (Id id : rest)
  where
    parseChunks' "RIFF" = parseList
    parseChunks' "LIST" = parseList
    parseChunks' _ = parseData
  
parseTokens :: Get [Token]
parseTokens = do
  empty <- isEmpty
  if empty
     then return []
     else parseChunks

main :: IO ()
main = BL.getContents >>= mapM_ print . runGet parseTokens

    
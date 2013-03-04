module RiffTokens
    ( 
      Token (Data, List)
    , parseTokens
    ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Word
import Control.Applicative
import Data.Text as T
import Data.Text.Encoding as E

--tokens
type Id = String
type Len = Int
type Format = String
type RawData = B.ByteString

-- flat structure
data Token = Data Id Len RawData
           | List Id Len Format deriving (Show)

-- tree structure
--data Chunk = List Id Format [Chunk]
--           | Data Id RawData

showRaw = show . T.init . E.decodeUtf16LEWith onerr
        where
          onerr _ _ = Nothing

--parse tokens
parseString :: Int -> Get String
parseString len = B8.unpack <$> getByteString len

parseInt:: Get Int
parseInt = fromIntegral <$> getWord32le

parseByteString :: Int -> Get B.ByteString
parseByteString len = do
    bs <- getByteString len
    skip $ len `mod` 2 -- skip one byte of padding if the length is odd
    return bs

parseList :: Id -> Get Token
parseList id = do
    len <- parseInt
    format <- parseString 4
    return (List id len format)

parseData :: Id -> Get Token
parseData id = do
    len <- parseInt
    raw <- parseByteString len
    return (Data id len raw)

parseToken :: Get Token
parseToken = do
    id <- parseString 4
    case id of
        "RIFF" -> parseList id
        "LIST" -> parseList id
        id     -> parseData id

parseTokens :: Get [Token]
parseTokens = do
    empty <- isEmpty
    if empty
        then return []
        else do c <- parseToken
                cs <- parseTokens
                return (c : cs)

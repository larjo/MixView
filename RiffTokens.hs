module RiffTokens
    ( 
      Token (Data, List)
    , parseTokens
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Binary.Get
import Control.Applicative
import Control.Monad.Loops (whileM) -- requires "cabal install monad.loops"

type Id = String
type Len = Int
type Format = String
type RawData = ByteString

data Token = Data Id Len RawData
           | List Id Len Format

-- parse binary
parseFourCC :: Get String
parseFourCC = unpack <$> getByteString 4

parseInt:: Get Int
parseInt = fromIntegral <$> getWord32le

parseByteString :: Int -> Get ByteString
parseByteString len = do
    bs <- getByteString len
    skip $ len `mod` 2 -- skip one byte of padding if the length is odd
    return bs

-- parse a token
parseData :: Get Token
parseData = do
    id <- parseFourCC
    len <- parseInt
    raw <- parseByteString len
    return (Data id len raw)

parseList :: Get Token
parseList = do
    id <- parseFourCC
    len <- parseInt
    format <- parseFourCC
    return (List id (len - 4) format)

parseToken :: Get Token
parseToken = parseToken' =<< lookAhead parseFourCC
  where
    parseToken' "RIFF" = parseList
    parseToken' "LIST" = parseList
    parseToken' _ = parseData

-- parse a list of tokens
parseTokens :: Get [Token]
parseTokens = whileM (not <$> isEmpty) parseToken


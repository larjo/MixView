module RiffTokens
    ( 
      Token (DataToken, ListToken)
    , RiffTokens (RiffTokens)
    , parseRiffTokens
    , tokenLength
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Binary.Get
import Control.Applicative
import Control.Monad.Loops (whileM) -- requires "cabal install monad-loops"

type Id = String
type Len = Int
type Format = String
type RawData = ByteString

data Token = DataToken Id Len RawData
           | ListToken Len Format
data RiffTokens = RiffTokens Len Format [Token]

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
    return (DataToken id len raw)

parseList :: Get Token
parseList = do
    _id <- parseFourCC -- is always "LIST"
    len <- parseInt
    format <- parseFourCC
    return (ListToken (len - 4) format)

parseToken :: Get Token
parseToken = parseToken' =<< lookAhead parseFourCC
  where
    parseToken' "LIST" = parseList
    parseToken' _ = parseData

tokenLength :: Token -> Int
tokenLength (DataToken _ len _) = len + 8
tokenLength (ListToken len _) = len + 12

-- parse a list of tokens
parseTokens :: Get [Token]
parseTokens = whileM (not <$> isEmpty) parseToken

parseRiffTokens :: Get RiffTokens
parseRiffTokens = do
    _id <- parseFourCC -- is always "RIFF"
    len <- parseInt
    format <- parseFourCC
    tokens <- parseTokens
    return (RiffTokens (len - 4) format tokens)

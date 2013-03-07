module RiffTokens
    ( 
      Token (DataToken, ListToken)
    , RiffFile (RiffFile)
    , ListInfo
    , DataInfo
    , parseRiffFile
    , dataLength
    , listLength
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

type ListInfo = (Len, Format)
type DataInfo = (Id, Len, RawData)

data Token = DataToken DataInfo
           | ListToken ListInfo
data RiffFile = RiffFile ListInfo [Token]

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

-- parse ListInfo
parseListInfo :: Get ListInfo
parseListInfo = do
    _id <- parseFourCC
    len <- parseInt
    format <- parseFourCC
    return (len - 4, format)

-- parse DataInfo
parseDataInfo :: Get DataInfo
parseDataInfo = do
    ident <- parseFourCC
    len <- parseInt
    raw <- parseByteString len
    return (ident, len, raw)

-- parse Token
parseToken :: Get Token
parseToken = parseToken' =<< lookAhead parseFourCC
  where
    parseToken' "LIST" = ListToken <$> parseListInfo
    parseToken' _ = DataToken <$> parseDataInfo

-- parse a list of tokens
parseTokens :: Get [Token]
parseTokens = whileM (not <$> isEmpty) parseToken

-- parse a complete riff file
parseRiffFile :: Get RiffFile
parseRiffFile = RiffFile <$> parseListInfo <*> parseTokens

-- calculate the length of the surrounding block
dataLength :: DataInfo -> Int
dataLength (_, len, _) = len + 8

listLength :: ListInfo -> Int
listLength (len, _) = len + 12

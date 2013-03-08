module RiffTokens
    ( 
      Token (DataToken, ListToken)
    , RiffFile (RiffFile)
    , ListInfo (ListInfo)
    , DataInfo (DataInfo)
    , parseRiffFile
    , dataLength
    , listLength
    ) where

import qualified Data.ByteString as B
import Data.ByteString.Char8 (unpack)
import Data.Binary.Get
import Control.Applicative
import Control.Monad.Loops (whileM) -- requires "cabal install monad-loops"

data RiffFile = RiffFile ListInfo [Token]

data Token = DataToken DataInfo
           | ListToken ListInfo

data DataInfo = DataInfo Id RawData
data ListInfo = ListInfo Len Format

type Id = String
type Len = Int
type Format = String
type RawData = B.ByteString

-- parse binary
parseFourCC :: Get String
parseFourCC = unpack <$> getByteString 4

parseInt:: Get Int
parseInt = fromIntegral <$> getWord32le

parseByteString :: Get B.ByteString
parseByteString = do
    len <- parseInt    
    bs <- getByteString len
    skip $ len `mod` 2 -- skip one byte of padding if the length is odd
    return bs

-- parse ListInfo
parseListInfo :: Get ListInfo
parseListInfo = ListInfo <$> (parseFourCC >> flip (-) 4 <$> parseInt) <*> parseFourCC

-- parse DataInfo
parseDataInfo :: Get DataInfo
parseDataInfo = DataInfo <$> parseFourCC <*> parseByteString
    
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
dataLength (DataInfo _ rawData) = len + len `mod` 2 + 8
                        where
                          len = B.length rawData

listLength :: ListInfo -> Int
listLength (ListInfo len _) = len + 12

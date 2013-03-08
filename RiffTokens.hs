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
import Data.Binary.Get ( Get
                       , getByteString
                       , getWord32le
                       , isEmpty
                       , lookAhead
                       , skip
                       ) -- requires "cabal install binary"
import Control.Monad.Loops (whileM) -- requires "cabal install monad-loops"
import Control.Applicative

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
skipFourCC :: Get ()
skipFourCC = skip 4

parseFourCC :: Get String
parseFourCC = unpack <$> getByteString 4

parseInt :: Get Int
parseInt = fromIntegral <$> getWord32le

adjustListLength :: Int -> Get Int
adjustListLength i = return (i - 4)

skipIfOdd :: Int -> Get ()
skipIfOdd = skip . (`mod` 2)

parseByteString :: Int -> Get B.ByteString
parseByteString len = do
    bs <- getByteString len
    skipIfOdd len
    return bs

-- parse ListInfo
parseListInfo :: Get ListInfo
parseListInfo = ListInfo <$> (skipFourCC >> parseInt >>= adjustListLength) <*> parseFourCC

-- parse DataInfo
parseDataInfo :: Get DataInfo
parseDataInfo = DataInfo <$> parseFourCC <*> (parseInt >>= parseByteString)

-- parse Token
parseToken :: Get Token
parseToken = lookAhead parseFourCC >>= parseToken'
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

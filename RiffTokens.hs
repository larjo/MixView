module RiffTokens
    ( 
      Token (DataToken, ListToken)
    , RiffFile (RiffFile)
    , ListChunk (ListChunk)
    , DataChunk (DataChunk)
    , parseRiffFile
    , dataLength
    , listLength
    , tokenLength
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

data RiffFile = RiffFile ListChunk [Token]

data Token = DataToken DataChunk
           | ListToken ListChunk

data DataChunk = DataChunk Id RawData
data ListChunk = ListChunk Len Format

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

-- parse ListChunk
parseListChunk :: Get ListChunk
parseListChunk = ListChunk 
                <$> (skipFourCC >> parseInt >>= adjustListLength)
                <*> parseFourCC

-- parse DataChunk
parseDataChunk :: Get DataChunk
parseDataChunk = DataChunk 
                <$> parseFourCC 
                <*> (parseInt >>= parseByteString)

-- parse Token
parseToken :: Get Token
parseToken = lookAhead parseFourCC >>= parseToken'
           where
             parseToken' "LIST" = ListToken <$> parseListChunk
             parseToken' _ = DataToken <$> parseDataChunk

-- parse a list of tokens
parseTokens :: Get [Token]
parseTokens = whileM (not <$> isEmpty) parseToken

-- parse a complete riff file
parseRiffFile :: Get RiffFile
parseRiffFile = RiffFile <$> parseListChunk <*> parseTokens

tokenLength :: Token -> Len
tokenLength (DataToken x) = dataLength x
tokenLength (ListToken x) = listLength x

-- calculate the length of the surrounding block
dataLength :: DataChunk -> Len
dataLength (DataChunk _ rawData) = len + len `mod` 2 + 8
                                where
                                  len = B.length rawData

listLength :: ListChunk -> Len
listLength (ListChunk len _) = len + 12

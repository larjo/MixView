module RiffTokens
    ( 
      Chunk (DataChunk, ListChunk)
    , RiffChunks (RiffChunks)
    , List
    , Data
    , Id
    , Len
    , Format
    , RawData
    , parseRiffChunks
    , chunkLength
    ) where

import Data.ByteString as B (ByteString, length)
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

type Id = String
type Len = Int
type Format = String
type RawData = ByteString

type Data = (Id, RawData)
type List = (Len, Format)

data Chunk = DataChunk Data
           | ListChunk List

data RiffChunks = RiffChunks List [Chunk]

instance Show Chunk where
    show (DataChunk (i, r)) = i
    show (ListChunk (l, f)) = f ++ "(" ++ show l ++ ")"

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

parseByteString :: Int -> Get ByteString
parseByteString len = do
    bs <- getByteString len
    skipIfOdd len
    return bs

-- parse List
parseList :: Get List
parseList = liftA2 (,) (skipFourCC >> parseInt >>= adjustListLength)
                       parseFourCC

-- parse DataChunk
parseData :: Get Data
parseData = liftA2 (,) parseFourCC 
                       (parseInt >>= parseByteString)

-- parse Chunk
parseChunk :: Get Chunk
parseChunk = lookAhead parseFourCC >>= parseChunk'
           where
             parseChunk' "LIST" = ListChunk <$> parseList
             parseChunk' _ = DataChunk <$> parseData

-- parse a list of tokens
parseChunks :: Get [Chunk]
parseChunks = whileM (not <$> isEmpty) parseChunk

-- parse a complete riff file
parseRiffChunks :: Get RiffChunks
parseRiffChunks = RiffChunks <$> parseList <*> parseChunks

dataLength :: Data -> Len
dataLength (_, rawData) = len + len `mod` 2 + 8
                        where
                          len = B.length rawData

listLength :: List -> Len
listLength (len, _) = len + 12

chunkLength :: Chunk -> Len
chunkLength (DataChunk d) = dataLength d
chunkLength (ListChunk l) = listLength l

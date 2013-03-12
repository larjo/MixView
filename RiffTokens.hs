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

data RiffChunks = RiffChunks List [Chunk]

data Chunk = DataChunk Data
           | ListChunk List

type List = (Len, Format)
type Data = (Id, RawData)

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

-- parse List
parseList :: Get List
parseList = (,)
            <$> (skipFourCC >> parseInt >>= adjustListLength)
            <*> parseFourCC

-- parse DataChunk
parseData :: Get Data
parseData = (,)
            <$> parseFourCC 
            <*> (parseInt >>= parseByteString)

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

chunkLength :: Chunk -> Len
chunkLength (DataChunk (_, rawData)) = len + len `mod` 2 + 8
                                     where
                                       len = B.length rawData

chunkLength (ListChunk (len, _)) = len + 12

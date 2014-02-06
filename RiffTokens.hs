module RiffTokens
    (
      Chunk (DataChunk, ListChunk)
    , RiffChunks (RiffChunks)
    , List (List, listLength, listFormat)
    , Data (Data, dataId, dataRaw)
    , Id
    , Len
    , Format
    , Raw
    , parseRiffChunks
    , chunkLength
    , dataChunkLength
    , listChunkLength
    ) where

import Data.ByteString as B (ByteString, length)
import Data.ByteString.Char8 (unpack)
import Data.Binary.Get ( Get
                       , getByteString
                       , getWord32le
                       , isEmpty
                       , lookAhead
                       , skip
                       )

import Control.Monad.Loops (whileM)
-- requires "cabal install monad-loops"

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)

type Id = String
type Len = Int
type Format = String
type Raw = ByteString

data Data = Data
    { dataId  :: Id
    , dataRaw :: Raw
    }

data List = List
    { listLength :: Len
    , listFormat :: Format
    }

data Chunk = DataChunk Data
           | ListChunk List

data RiffChunks = RiffChunks List [Chunk] -- RiffChunks = List(Data|List)*

formatChunk :: [String] -> String
formatChunk = intercalate ":"

instance Show Data where
    show x = formatChunk [ show $ dataChunkLength x
                         , dataId x
                         ]

instance Show List where
    show x = formatChunk [ show $ listChunkLength x
                         , listFormat x
                         , show $ listLength x
                         ]

instance Show Chunk where
    show (DataChunk x) = show x
    show (ListChunk x) = show x

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

-- parse Data
parseData :: Get Data
parseData = Data <$> parseFourCC
                 <*> (parseInt >>= parseByteString)

-- parse List
parseList :: Get List
parseList = List <$> (skipFourCC >> parseInt >>= adjustListLength)
                 <*> parseFourCC

-- parse Chunk
parseChunk :: Get Chunk
parseChunk = lookAhead parseFourCC >>= parseChunk'
           where
             parseChunk' "LIST" = ListChunk <$> parseList
             parseChunk' _      = DataChunk <$> parseData

-- parse Chunks 
parseChunks :: Get [Chunk]
parseChunks = whileM (not <$> isEmpty) parseChunk

-- parse a complete riff file
parseRiffChunks :: Get RiffChunks
parseRiffChunks = RiffChunks <$> parseList <*> parseChunks

dataChunkLength :: Data -> Len
dataChunkLength = surroundingLength . B.length . dataRaw
  where
    surroundingLength len = len + len `mod` 2 + 8

listChunkLength :: List -> Len
listChunkLength = (+ 12) . listLength

chunkLength :: Chunk -> Len
chunkLength (DataChunk d) = dataChunkLength d
chunkLength (ListChunk l) = listChunkLength l

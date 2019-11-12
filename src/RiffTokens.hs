module RiffTokens
    ( Chunk (DataChunk, ListChunk)
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

import Control.Monad.Loops (whileM)
import Control.Applicative ((<$>), (<*>))
import Data.ByteString as B (ByteString, length)
import Data.ByteString.Char8 (unpack)
import Data.Binary.Get ( Get
                       , getByteString
                       , getWord32le
                       , isEmpty
                       , lookAhead
                       , skip
                       )
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

adjustListLength :: Int -> Int
adjustListLength i = i - 4

skipIfOdd :: Int -> Get ()
skipIfOdd = skip . (`mod` 2)

parseByteString :: Int -> Get ByteString
parseByteString len = do
    bs <- getByteString len
    skipIfOdd len
    return bs

-- parse Data
parseData :: Get Data
parseData = do
    id <- parseFourCC
    len <- parseInt
    rawData <- parseByteString len
    return $ Data id rawData

-- parse List
parseList :: Get List
parseList = do
    skipFourCC
    len <- parseInt
    fourCC <- parseFourCC
    return $ List (adjustListLength len) fourCC

-- parse Chunk
parseChunk :: Get Chunk
parseChunk = do
    id <- lookAhead parseFourCC
    go id
    where
        go "LIST" = ListChunk <$> parseList
        go _      = DataChunk <$> parseData

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

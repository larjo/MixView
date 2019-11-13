module RiffTokens
    ( Chunk (DataChunk, ListChunk)
    , Data (Data, dataId, dataRaw)
    , Format
    , Id
    , Len
    , List (List, listLength, listFormat)
    , Raw
    , RiffChunks (RiffChunks)
    , chunkLength
    , dataChunkLength
    , listChunkLength
    , listFiles
    , listTokens
    , parseRiffChunks ) where

import Control.Applicative
import Control.Monad.Loops
import Data.Binary.Get
import Data.ByteString.Char8
import Data.Maybe
import Data.Text.Encoding        
import qualified Data.List as DL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

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
formatChunk = DL.intercalate ":"

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
dataChunkLength =
    surroundingLength . B.length . dataRaw
    where
        surroundingLength len = len + len `mod` 2 + 8

listChunkLength :: List -> Len
listChunkLength = (+ 12) . listLength

chunkLength :: Chunk -> Len
chunkLength (DataChunk d) = dataChunkLength d
chunkLength (ListChunk l) = listChunkLength l

showRiff :: RiffChunks -> String
showRiff (RiffChunks l cs) =
    (show l) ++ ":" ++ (show cs)

listTokens :: BL.ByteString -> String
listTokens = showRiff . runGet parseRiffChunks

getChunks :: RiffChunks -> [Chunk]
getChunks (RiffChunks _ cs) = cs

chunkToFilename :: Chunk -> Maybe String
chunkToFilename (DataChunk (Data "TRKF" d)) = Just . T.unpack . T.init . decodeUtf16LE $ d
chunkToFilename (DataChunk (Data "info" d)) = Just . show $ d
chunkToFilename (DataChunk (Data "prof" d)) = Just . show $ d
chunkToFilename _ = Nothing

listFiles :: BL.ByteString -> String
listFiles = show . mapMaybe chunkToFilename . getChunks . runGet parseRiffChunks

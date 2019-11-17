module RiffTokens
    ( Chunk (DataChunk, ListChunk)
    , Data (Data, dataId, dataRaw)
    , Format
    , Id
    , Len
    , List (List, listLength, listFormat)
    , Raw
    , RiffFile (RiffFile)
    , chunkLength
    , dataChunkLength
    , listChunkLength
    , listFiles
    , listTokens
    , parseRiffFile ) where

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

data RiffFile = RiffFile List [Chunk] -- RiffFile = List(Data|List)*

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
    fourCC <- parseFourCC
    len <- parseInt
    rawData <- parseByteString len
    return $ Data fourCC rawData

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
    fourCC <- lookAhead parseFourCC
    go fourCC
    where
        go "LIST" = ListChunk <$> parseList
        go _      = DataChunk <$> parseData

-- parse Chunks 
parseChunks :: Get [Chunk]
parseChunks = whileM (not <$> isEmpty) parseChunk

-- parse a complete riff file
parseRiffFile :: Get RiffFile
parseRiffFile = RiffFile <$> parseList <*> parseChunks

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

showRiff :: RiffFile -> String
showRiff (RiffFile l cs) =
    (show l) ++ ":::" ++ (show cs)

listTokens :: BL.ByteString -> String
listTokens = showRiff . runGet parseRiffFile

getChunks :: RiffFile -> [Chunk]
getChunks (RiffFile _ cs) = cs

parseFilename :: Chunk -> Maybe String
parseFilename (DataChunk (Data "TRKF" d)) = Just . T.unpack . T.init . decodeUtf16LE $ d
parseFilename _ = Nothing

listFiles :: BL.ByteString -> String
listFiles = show . mapMaybe parseFilename . getChunks . runGet parseRiffFile

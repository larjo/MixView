module Id3
    ( id3
    , listIds
    , listTags
    , parseTitleArtist ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Data.Binary.Get
import Data.List
import Data.Maybe
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Encoding as BE
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.Text as T

data Frame = Frame String String
type FrameMap = M.Map String String

getHeader :: Get String
getHeader = B8.unpack <$> getByteString 3

getVersion :: Get String
getVersion = do
    v1 <- show <$> getWord8
    v2 <- show <$> getWord8
    return $ "2." ++ v1 ++ "." ++ v2

removeTerminator :: Int -> B.ByteString -> B.ByteString
removeTerminator n = B.reverse . B.drop n . B.reverse 

decodeText :: Word8 -> B.ByteString -> T.Text
decodeText encoding textBs =
    case encoding of
        0 -> BE.decode BE.latin1 textBs
        x | x == 1 || x == 2 -> BE.decode BE.utf16 $ removeTerminator 2 textBs
        3 -> BE.decode BE.utf8 $ removeTerminator 1 textBs
                  
showText :: String -> B.ByteString -> Maybe String
showText id bs =
    if head id == 'T'
    then do
        (encoding, textBs) <- B.uncons bs
        return $ T.unpack $ decodeText encoding textBs
    else Nothing

getFrame :: Get (Maybe Frame)
getFrame = do
    id <- B8.unpack <$> getByteString 4
    size <- fromIntegral <$> getWord32be
    skip 2 -- skip flags
    bs <- getByteString size
    br <- bytesRead
    return $ fmap (\raw -> Frame id raw) $ showText id bs

framesLeft :: Int -> Get Bool
framesLeft size = do
    br <- bytesRead
    if fromIntegral br + 10 <= size
    then do
        nextId <- lookAhead (getByteString 4)
        return $ not $ B.all (== 0) nextId
    else return False

getSize :: Get Int
getSize = do
    words <- replicateM 4 getWord8
    let size = foldl (\s x -> 128 * s + (fromIntegral x)) 0 words
    return $ size + 10

parseId3 :: Get [Frame]
parseId3 = do
    header <- getHeader
    if (header /= "ID3")
    then return []
    else do
        version <- getVersion
        skip 1 -- skip flags
        size <- getSize
        frames <- whileM (framesLeft size) getFrame
        return $ Frame "VER" version : catMaybes frames

insertFrame :: FrameMap -> Frame -> FrameMap
insertFrame fm (Frame i str) = M.insert i str fm

lookupFrame :: FrameMap -> String -> String
lookupFrame fm k = M.findWithDefault "" k fm

mapTags :: [String] -> FrameMap -> [String]
mapTags tags fm = map (lookupFrame fm) tags

parseTags :: [String] -> BL.ByteString -> [String]
parseTags tags = mapTags tags . foldl insertFrame M.empty . runGet parseId3

parseTitleArtist :: BL.ByteString -> [String]
parseTitleArtist = parseTags ["TIT2", "TPE1"]

id3 :: BL.ByteString -> String
id3 = show . parseTitleArtist

listTags :: BL.ByteString -> String
listTags = show . map (\(Frame id tag) -> id ++ ":" ++ tag) . runGet parseId3

listIds :: BL.ByteString -> String
listIds = show . map (\(Frame id tag) -> id) . runGet parseId3
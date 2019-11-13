module Id3
    ( parseTitleArtist, id3, listTags, listIds ) where

import           Control.Applicative   ((<$>))
import           Control.Monad         (replicateM, unless)
import           Control.Monad.Loops   (whileM)
import           Data.Binary.Get       (Get, bytesRead, getByteString,
                                        getWord32be, getWord8, lookAhead,
                                        runGet, skip)
import qualified Data.ByteString       as B (ByteString, all, drop , reverse, uncons )
import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy  as BL (ByteString, getContents)
import qualified Data.ByteString.Encoding as BE (decode, latin1, utf16, utf8)
import           Data.List             (intercalate)
import qualified Data.Map              as M (Map, empty, findWithDefault,
                                             insert)
import qualified Data.Text             as T (init, null, unpack, last)
import           Data.Text.Encoding    (decodeUtf16LEWith, decodeUtf16BEWith)
import           Data.Maybe            (fromMaybe, catMaybes)

data Frame = Frame String String
type FrameMap = M.Map String String

getHeader :: Get String
getHeader = unpack <$> getByteString 3

getVersion :: Get String
getVersion = do
    v1 <- show <$> getWord8
    v2 <- show <$> getWord8
    return $ "2." ++ v1 ++ "." ++ v2

removeTerminator :: Int -> B.ByteString -> B.ByteString
removeTerminator n = B.reverse . B.drop n . B.reverse 

showRaw :: String -> B.ByteString -> Maybe String
showRaw id bs =
    if head id == 'T'
    then do
        (encoding, textBs) <- B.uncons bs
        let decoded = case encoding of 0 -> BE.decode BE.latin1 textBs
                                       x | x == 1 || x == 2 -> BE.decode BE.utf16 $ removeTerminator 2 textBs
                                       3 -> BE.decode BE.utf8 $ removeTerminator 1 textBs
        return $ T.unpack decoded
    else Nothing

frame :: Get (Maybe Frame)
frame = do
    id <- unpack <$> getByteString 4
    size <- fromIntegral <$> getWord32be
    skip 2 -- skip flags
    bs <- getByteString size
    br <- bytesRead
    return $ fmap (\raw -> Frame id raw) $ showRaw id bs

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
    let size = foldl (\s x -> 128 * s + x) 0 $ map fromIntegral words
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
        frames <- whileM (framesLeft size) frame
        return $ Frame "VER" version : Frame "SIZE" (show size) : catMaybes frames

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
listTags bs = show $ map (\(Frame id tag) -> id ++ ":" ++ tag) $ runGet parseId3 bs

listIds :: BL.ByteString -> String
listIds bs = show $ map (\(Frame id tag) -> id) $ runGet parseId3 bs
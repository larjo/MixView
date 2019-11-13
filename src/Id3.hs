module Id3
    ( parseTitleArtist, id3, listTags, listIds ) where

import           Control.Applicative   ((<$>))
import           Control.Monad         (replicateM, unless)
import           Control.Monad.Loops   (whileM)
import           Data.Binary.Get       (Get, bytesRead, getByteString,
                                        getWord32be, getWord8, lookAhead,
                                        runGet, skip)
import qualified Data.ByteString       as B (any, ByteString, drop , reverse, uncons )
import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy  as BL (ByteString, getContents)
import qualified Data.ByteString.Encoding as BE (decode, latin1, utf16, utf8)
import           Data.List             (intercalate)
import qualified Data.Map              as M (Map, empty, findWithDefault,
                                             insert)
import qualified Data.Text             as T (init, null, unpack, last)
import           Data.Text.Encoding    (decodeUtf16LEWith, decodeUtf16BEWith)
import           Data.Maybe            (fromMaybe)
data Frame = Frame String String
type FrameMap = M.Map String String

getHeader :: Get String
getHeader = unpack <$> getByteString 3

getVersion :: Get String
getVersion = do
    words <- replicateM 2 getWord8
    return (intercalate "." ("2" : map show words))

removeTerminator :: Int -> B.ByteString -> B.ByteString
removeTerminator n = B.reverse . B.drop n . B.reverse 
--removeTerminator = B.reverse . B.dropWhile (\w8 -> w8 == 0) . B.reverse

showRaw :: B.ByteString -> Maybe String
showRaw bs = do
    (encoding, string) <- B.uncons bs
    let decoded = case encoding of 0 -> BE.decode BE.latin1 string
                                   x | x == 1 || x == 2 -> BE.decode BE.utf16 $ removeTerminator 2 string
                                   3 -> BE.decode BE.utf8 $ removeTerminator 1 string
    return (T.unpack decoded)
   
frame :: Get Frame
frame = do
    id <- getByteString 4
    size <- getWord32be
    skip 2 -- skip flags
    bs <- getByteString $ fromIntegral size
    br <- bytesRead
    return $ Frame (unpack id) (fromMaybe (show br) $ showRaw bs)

framesLeft :: Int -> Get Bool
framesLeft size = do
    br <- bytesRead
    -- i <- lookAhead (getByteString 4)
    -- return $ B.any (/= 0) i && (fromIntegral br < size)
    return (fromIntegral br < size)
getSize :: Get Int
getSize = do
    words <- replicateM 4 getWord8
    let size = foldl (\s x -> 128 * s + x) 0 $ map fromIntegral words
    return size

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
        return (Frame "VER" version : Frame "SIZE" (show size): frames)

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
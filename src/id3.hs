module Id3
    ( parseTitleArtist ) where

import           Control.Applicative   ((<$>))
import           Control.Monad         (replicateM)
import           Control.Monad.Loops   (whileM)
import           Data.Binary.Get       (Get, bytesRead, getByteString,
                                        getWord32be, getWord8, lookAhead,
                                        runGet, skip)
import qualified Data.ByteString       as B (any, ByteString)
import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy  as BL (ByteString, getContents)
import           Data.List             (intercalate)
import qualified Data.Map              as M (Map, empty, findWithDefault,
                                             insert)
import qualified Data.Text             as T (init, null, unpack)
import           Data.Text.Encoding    (decodeUtf16LEWith)

data Frame = Frame String String

type FrameMap = M.Map String String

header :: Get String
header = unpack <$> getByteString 3

version :: Get String
version = intercalate "." . map show <$> replicateM 2 getWord8

showRaw :: B.ByteString -> String
showRaw = T.unpack . safeInit . decodeUtf16LEWith (\_ _ -> Nothing)
  where
    safeInit x = if T.null x then x else T.init x

frame :: Get Frame
frame = do
    id <- getByteString 4
    size <- getWord32be
    skip 5 -- skip flags, extra, bom
    bs <- getByteString $ fromIntegral size - 3
    return $ Frame (unpack id) (showRaw bs)

framesLeft :: Int -> Get Bool
framesLeft size = do
    br <- fromIntegral <$> bytesRead
    i <- lookAhead (getByteString 4)
    return $ B.any ( /= 0) i && (br < size)

getSize :: Get Int
getSize = (+ 10) . foldl (\s x -> 128*s + x) 0 . map fromIntegral <$> replicateM 4 getWord8

parseId3 :: Get [Frame]
parseId3 = do
    _ <- header
    _ <- version
    skip 1 -- skip flags
    size <- getSize
    whileM (framesLeft size) frame

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

id3 :: IO ()
id3 = BL.getContents >>= print . parseTitleArtist


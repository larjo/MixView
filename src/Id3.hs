{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Id3
  ( listIds,
    listTags,
    listInfo,
    formatInfo,
  )
where

import Control.Monad (replicateM)
import Control.Monad.Loops (whileM)
import Data.Binary.Get
  ( Get,
    bytesRead,
    getByteString,
    getWord32be,
    getWord8,
    lookAhead,
    runGet,
    skip,
  )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Encoding as BE
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Word (Word8)

data Frame
  = Frame String String

type FrameMap = M.Map String String

getHeader :: Get String
getHeader = B8.unpack <$> getByteString 3

-- >>> runGet getHeader (BL.pack [51, 48, 50])
-- "302"

getVersion :: Get String
getVersion = do
  v1 <- show <$> getWord8
  v2 <- show <$> getWord8
  return $ "2." ++ v1 ++ "." ++ v2

-- >>> runGet getVersion (BL.pack [13, 27])
-- "2.13.27"

removeTerminator :: Int -> B.ByteString -> B.ByteString
removeTerminator n = B.reverse . B.drop n . B.reverse

-- >>> removeTerminator 3 "abcdef"
-- "abc"

decodeText :: Word8 -> B.ByteString -> T.Text
decodeText encoding textBs =
  case encoding of
    0 -> BE.decode BE.latin1 textBs
    x
      | x == 1 || x == 2 -> BE.decode BE.utf16 $ removeTerminator 2 textBs
    3 -> BE.decode BE.utf8 $ removeTerminator 1 textBs
    _ -> "<UNKNOWN ENCODING>"

-- >>> decodeText 0 "teståäö"
-- "test\229\228\246"

showText :: String -> B.ByteString -> Maybe String
showText frameid bs =
  if head frameid == 'T'
    then do
      (encoding, textBs) <- B.uncons bs
      return $ T.unpack $ decodeText encoding textBs
    else return "<BINARY>"

getFrame :: Get (Maybe Frame)
getFrame = do
  frameid <- B8.unpack <$> getByteString 4
  size <- fromIntegral <$> getWord32be
  skip 2 -- skip flags
  bs <- getByteString size
  return $ Frame frameid <$> showText frameid bs

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
  sizeW8 <- replicateM 4 getWord8
  let size = foldl (\s x -> 128 * s + fromIntegral x) 0 sizeW8
  return $ size + 10

parseId3 :: Get [Frame]
parseId3 = do
  header <- getHeader
  if header /= "ID3"
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

data Mp3Info = Mp3Info
  { title :: String,
    artist :: String
  }
  deriving (Show)

formatInfo :: Mp3Info -> String
formatInfo info = title info ++ " - " ++ artist info

-- >>> Mp3Info "titel" "artisten"
-- >>> formatInfo $ Mp3Info "titel" "artisten"
-- Mp3Info {title = "titel", artist = "artisten"}
-- "titel - artisten"

listInfo :: BL.ByteString -> Mp3Info
listInfo bs = Mp3Info {title = lookupTag "TIT2", artist = lookupTag "TPE1"}
  where
    lookupTag = lookupFrame frameMap
    frameMap = foldl insertFrame M.empty $ runGet parseId3 bs

padRight :: a -> Int -> [a] -> [a]
padRight p s l = take s $ l ++ repeat p

-- >>> padRight '.' 10 "Test"
-- "Test......"

formatId :: String -> String
formatId = padRight ' ' 4

listTags :: BL.ByteString -> [String]
listTags = map (\(Frame frameId tag) -> formatId frameId ++ " " ++ tag) . runGet parseId3

listIds :: BL.ByteString -> [String]
listIds = map (\(Frame frameId _tag) -> formatId frameId) . runGet parseId3

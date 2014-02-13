import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf16LE)
import Data.Binary.Get

import RiffTokens
import Id3

getChunks :: RiffChunks -> [Chunk]
getChunks (RiffChunks _ cs) = cs

isFilename :: Chunk -> Bool
isFilename (DataChunk (Data "TRKF" d)) = True
isFilename _ = False

chunkToFilename :: Chunk -> IO ()
chunkToFilename (DataChunk (Data "TRKF" d)) =
    showRaw d
  where
    showRaw = T.unpack . T.init . decodeUtf16LE
chunkToFilename _ = ""

main :: IO ()
main = BL.getContents >>= mapM_ printTRKF . getChunks . runGet parseRiffChunks

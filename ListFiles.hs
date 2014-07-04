import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf16LE)
import Data.Binary.Get
import Data.Maybe
import RiffTokens
import Id3

getChunks :: RiffChunks -> [Chunk]
getChunks (RiffChunks _ cs) = cs

chunkToFilename :: Chunk -> Maybe String
chunkToFilename (DataChunk (Data "TRKF" d)) =
    Just $ showRaw d
  where
    showRaw = T.unpack . T.init . decodeUtf16LE
chunkToFilename _ = Nothing

main :: IO ()
main = BL.getContents >>= mapM_ putStrLn . catMaybes . map chunkToFilename . getChunks . runGet parseRiffChunks

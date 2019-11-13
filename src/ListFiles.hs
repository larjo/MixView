module ListFiles
    (listFiles)
    where
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf16LE)
import Data.Binary.Get
import Data.Maybe
import RiffTokens

getChunks :: RiffChunks -> [Chunk]
getChunks (RiffChunks _ cs) = cs

chunkToFilename :: Chunk -> Maybe String
chunkToFilename (DataChunk (Data "TRKF" d)) = Just . T.unpack . T.init . decodeUtf16LE $ d
chunkToFilename _ = Nothing

listFiles :: BL.ByteString -> String
listFiles = show . mapMaybe chunkToFilename . getChunks . runGet parseRiffChunks

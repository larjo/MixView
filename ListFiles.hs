import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf16LE)
import Data.Binary.Get

import RiffTokens

getChunks :: RiffChunks -> [Chunk]
getChunks (RiffChunks _ cs) = cs

printTRKF :: Chunk -> IO ()
printTRKF (DataChunk (Data "TRKF" d)) =
    putStrLn $ showRaw d
  where 
    showRaw = T.unpack . T.init . decodeUtf16LE

printTRKF _ = return ()

main :: IO ()
main = BL.getContents >>= mapM_ printTRKF . getChunks . runGet parseRiffChunks

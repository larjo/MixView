import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf16LE)
import Data.Binary.Get

import RiffTokens

getTokens :: RiffFile -> [Token]
getTokens (RiffFile _ ts) = ts

printTRKF :: Token -> IO ()
printTRKF (DataToken (DataInfo "TRKF" d)) =
    putStrLn $ showRaw d
  where 
    showRaw = T.unpack . T.init . decodeUtf16LE

printTRKF _ = return ()

main :: IO ()
main = BL.getContents >>= mapM_ printTRKF . getTokens . runGet parseRiffFile

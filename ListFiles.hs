import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf16LE)
import Data.Binary.Get

import RiffTokens

printTRKF :: Token -> IO ()
printTRKF (Data "TRKF" _ d) = putStrLn $ showRaw d
    where showRaw = T.unpack . T.init . decodeUtf16LE
printTRKF _ = return ()

main :: IO ()
main = BL.getContents >>= mapM_ printTRKF . runGet parseTokens

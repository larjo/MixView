import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as BL (ByteString, getContents)
import RiffTokens

printRiff :: RiffChunks -> IO ()
printRiff (RiffChunks l cs) = do
    print l
    mapM_ print cs

getChunks :: BL.ByteString -> IO RiffChunks
getChunks = return . runGet parseRiffChunks

main :: IO ()
main = BL.getContents >>= getChunks >>= printRiff

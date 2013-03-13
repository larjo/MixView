import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as BL (ByteString, readFile, getContents)
import RiffTokens

printRiff :: RiffChunks -> IO ()
printRiff (RiffChunks l cs) = do
    print l
    mapM_ print cs

getChunks :: BL.ByteString -> IO RiffChunks
getChunks = return . runGet parseRiffChunks

load :: FilePath -> IO RiffChunks
load fn = BL.readFile fn >>= getChunks
   
main :: IO ()
main = BL.getContents >>= getChunks >>= printRiff

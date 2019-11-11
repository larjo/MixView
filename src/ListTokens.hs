module ListTokens where

import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as BL (ByteString, getContents)
import RiffTokens

printRiff :: RiffChunks -> IO ()
printRiff (RiffChunks l cs) = do
    print l
    mapM_ print cs

getChunks :: BL.ByteString -> RiffChunks
getChunks = runGet parseRiffChunks

listTokens :: IO ()
listTokens = BL.getContents >>= printRiff . getChunks

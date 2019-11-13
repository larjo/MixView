module ListTokens
    ( listTokens ) where

import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as BL (ByteString, getContents)
import RiffTokens

showRiff :: RiffChunks -> String
showRiff (RiffChunks l cs) =
    (show l) ++ ":" ++ (show cs)

getChunks :: BL.ByteString -> RiffChunks
getChunks = runGet parseRiffChunks

listTokens :: BL.ByteString -> String
listTokens = showRiff . getChunks

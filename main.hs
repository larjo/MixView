import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

import RiffTokens

getLength :: RiffFile -> Int
getLength (RiffFile l _) = listLength l

main :: IO ()
main = BL.getContents >>= print . getLength . runGet parseRiffFile

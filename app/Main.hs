import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

import CreateTree

main :: IO ()
main = BL.getContents >>= putStrLn . showRoot . riffFromBinary

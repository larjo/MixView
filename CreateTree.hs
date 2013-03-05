import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

import RiffTokens

type Id = String
type Len = Int
type Format = String
type RawData = ByteString

data Chunk = Chunks Id Format [Chunk]
           | RiffData Id RawData

createTree :: [Token] -> Chunk
createTree _ = Chunks "" "" []

showTree :: Chunk -> [String]
showTree _ = []

main :: IO ()
main = BL.getContents >>= mapM_ putStrLn . showTree . createTree . runGet parseTokens

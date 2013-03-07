import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.List (intercalate)

import RiffTokens (parseRiffFile, RiffFile(RiffFile))

type Id = String
type Len = Int
type Format = String
type RawData = ByteString

type ListInfo = (Format, [Chunk])
type DataInfo = (Id, RawData)

data Chunk = List ListInfo
           | Data DataInfo
data Riff = Riff ListInfo

-- createRiff :: RiffFile -> Riff
-- createRiff (RiffFile (len, format) tokens) = Riff (format, createChunks len tokens)

-- createChunks :: Int -> [Token] -> ([Token], [Chunk])
-- createChunks len (t@DataToken id dat : rest)
    -- | len > 0 = Data id dat : createChunks (len - tokenLength t)
    -- | otherwise = []
-- createChunks len (t@ListToken listlen format : rest)
    -- | len > 0 = List format (createChunks listlen rest) : createChunks (len - tokenlength t)   

showRiff :: Riff -> String
showRiff (Riff format cs) = "RIFF:" ++ format ++ showChunks cs

showChunks :: [Chunk] -> String
showChunks chunks = "(" ++ intercalate "," (map showChunk chunks) ++ ")"

showChunk :: Chunk -> String
showChunk (List format cs) = "LIST:" ++ format ++ showChunks cs
showChunk (Data i d) = i

main :: IO ()
main = BL.getContents >>= putStrLn . showRiff . createRiff . runGet parseRiffFile

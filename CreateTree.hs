import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.List (intercalate)

import RiffTokens

type Id = String
type Len = Int
type Format = String
type RawData = ByteString

data Chunk = Chunks Id Format [Chunk]
           | RiffData Id RawData

createTree :: [Token] -> Chunk
createTree _ = Chunks "RIFF" "FMTT" [RiffData "TTTT" (pack "hej"),Chunks "LIST" "XXXX" [],RiffData "UUUU" (pack "hopp")]

showTree :: Chunk -> String
showTree (Chunks i f cs) = i ++ ":" ++ f ++ "(" ++ intercalate "," $ map showTree cs ++ ")"
showTree (RiffData i d) = i

main :: IO ()
main = BL.getContents >>= putStrLn . showTree . createTree . runGet parseTokens

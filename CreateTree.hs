import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (runGet)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.List (intercalate)

import RiffTokens

type Id = String
type Len = Int
type Format = String
type RawData = ByteString

data List = List Format [Node]
data Data = Data Id RawData

data Node = ListNode List
          | DataNode Data 
type RiffTree = List

createRiff :: RiffFile -> RiffTree
createRiff (RiffFile (ListChunk len format) tokens) =
    List format (createNodes len tokens)

createNodes :: Len -> [Token] -> [Node]
createNodes len ts = []

sumNodes :: [Token] -> [Int]
sumNodes = map tokenLength

accNodes :: [Int] -> [Int]
accNodes = tail . scanl (+) 0

tokenSums :: [Token] -> [(Token, Int)]
tokenSums tokens = zip tokens (accNodes $ sumNodes tokens)

takeWhileSum sum = takeWhile (\(_, i) -> i <= sum)
-- createChunks len (t@DataToken id dat : rest)
    -- | len > 0 = Data id dat : createChunks (len - tokenLength t)
    -- | otherwise = []
-- createChunks len (t@ListToken listlen format : rest)
    -- | len > 0 = List format (createChunks listlen rest) : createChunks (len - tokenlength t)   

showRiff :: RiffTree -> String
showRiff (List format cs) = "RIFF:" ++ format ++ showNodes cs

showNodes :: [Node] -> String
showNodes nodes = "(" ++ intercalate "," (map showNode nodes) ++ ")"

showNode :: Node -> String
showNode (ListNode (List format cs)) = "LIST:" ++ format ++ showNodes cs
showNode (DataNode (Data i d)) = i

main :: IO ()
--main = BL.getContents >>= putStrLn . showRiff . createRiff . runGet parseRiffFile
main = BL.getContents >>= print . map snd . showTokenSum . runGet parseRiffFile

showTokenSum (RiffFile _ tokens) = tokenSums tokens

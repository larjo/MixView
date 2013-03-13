import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (runGet)
import Data.List (intercalate)

import RiffTokens

data Node = TreeNode Tree
          | DataNode Id Raw

data Tree = Tree Format [Node]

createRiff :: RiffChunks -> Tree
createRiff (RiffChunks (List len format) chunks) =
    Tree format (createNodes len chunks)
    
createNodes :: Len -> [Chunk] -> [Node]
createNodes len chunks = 
                       where (l, r) = splitChunks len chunks

splitChunks :: Len -> [Chunk] -> ([Chunk], [Chunk])
splitChunks len chunks =
    let accLen = scanl1 (+) $ map chunkLength chunks
        (a, b) = span ((<= len) . snd) $ zip chunks accLen in
    (map fst a, map fst b)

-- createChunks len (t@DataChunk id dat : rest)
    -- | len > 0 = Data id dat : createChunks (len - chunkLength t)
    -- | otherwise = []
-- createChunks len (t@ListChunk listlen format : rest)
    -- | len > 0 = List format (createChunks listlen rest) : createChunks (len - chunkLength t)   

showRiff :: Tree -> String
showRiff (Tree format cs) = "RIFF:" ++ format ++ showNodes cs

showNodes :: [Node] -> String
showNodes nodes = "(" ++ intercalate "," (map showNode nodes) ++ ")"

showNode :: Node -> String
showNode (TreeNode (Tree format cs)) = "LIST:" ++ format ++ showNodes cs
showNode (DataNode i d) = i

parseFile :: String -> IO ()
parseFile fn = BL.readFile fn >>= putStrLn . showPair . runGet parseRiffChunks

main :: IO ()
main = BL.getContents >>= putStrLn . showRiff . createRiff . runGet parseRiffChunks


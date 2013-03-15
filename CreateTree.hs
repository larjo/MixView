import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as BL (getContents, readFile)
import Data.List (intercalate)

import RiffTokens

data Node = DataNode Id Raw
          | TreeNode Tree

data Tree = Tree Format [Node]

-- mapConsume maps a function over a list. 
-- The function itself decides how much of the list it consumes.
-- mapConsume returns the unconsumed part of the list
mapConsume :: Len -> ([a] -> (Len, b, [a])) -> [a] -> ([b], [a])
mapConsume _ _ [] = ([], [])
mapConsume 0 _ r = ([], r)
mapConsume len f xs = (y : ys, rest')
  where
    (l, y, rest) = f xs
    (ys, rest') = mapConsume (len - l) f rest

createNode :: [Chunk] -> (Len, Node, [Chunk])
createNode (DataChunk dat:cs) = (dataChunkLength dat, DataNode (dataId dat) (dataRaw dat), cs)
createNode (ListChunk list:cs) = (listChunkLength list, TreeNode tree, rest)
  where
    (tree, rest) = createTree list cs

createTree :: List -> [Chunk] -> (Tree, [Chunk])
createTree (List len format) cs = (Tree format nodes, rest)
  where
    (nodes, rest) = mapConsume len createNode cs

createRiff :: RiffChunks -> (Tree, [Chunk])
createRiff (RiffChunks list cs) = createTree list cs

showRiff :: Tree -> String
showRiff (Tree format cs) = "RIFF:" ++ format ++ showNodes 1 cs

indent :: Int -> String
indent ind = '\n' : replicate (ind * 2) ' '

showNodes :: Int -> [Node] -> String
showNodes ind nodes = '(' : intercalate "," (map (showNode ind) nodes) ++ ")"

showNode :: Int -> Node -> String
showNode ind (TreeNode (Tree format cs)) = indent ind ++ "LIST:" ++ format ++ showNodes (ind + 1) cs
showNode _ (DataNode i _) = i

parseFile :: String -> IO (Tree, [Chunk])
parseFile fn = BL.readFile fn >>= return . createRiff . runGet parseRiffChunks

main :: IO ()
main = BL.getContents >>= putStrLn . showRiff . fst . createRiff . runGet parseRiffChunks


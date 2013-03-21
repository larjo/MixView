import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as BL (getContents, readFile)
import Data.List (intercalate)
import Control.Monad.State
import Control.Monad.Loops
import Control.Applicative

import RiffTokens

data Node = DataNode Id Raw
          | TreeNode Tree

data Tree = Tree Format [Node]

data ChunkState = ChunkState
    { chunks :: [Chunk]
    } deriving Show

type ChunkMonad = State ChunkState

getChunk :: ChunkMonad Chunk
getChunk = state (\(ChunkState (c:cs)) -> (c, ChunkState cs))

createNodes :: Int -> ChunkMonad [Node]
createNodes 0 = return []
createNodes len = do
    c <- getChunk
    n <- createNode c
    ns <- createNodes (len - chunkLength c)
    return (n : ns)

createNode :: Chunk -> ChunkMonad Node
createNode (DataChunk dat) = return (DataNode (dataId dat) (dataRaw dat))
createNode (ListChunk list) = TreeNode <$> tree
  where
    tree = createTree list

createTree :: List -> ChunkMonad Tree
createTree (List len format) = do
        nodes <- createNodes len
        return (Tree format nodes)

createRiff :: RiffChunks -> Tree
createRiff (RiffChunks list cs) = evalState (createTree list) (ChunkState cs)

showRiff :: Tree -> String
showRiff (Tree format cs) = "RIFF:" ++ format ++ showNodes 1 cs

indent :: Int -> String
indent ind = '\n' : replicate (ind * 2) ' '

showNodes :: Int -> [Node] -> String
showNodes ind nodes = '(' : intercalate "," (map (showNode ind) nodes) ++ ")"

showNode :: Int -> Node -> String
showNode ind (TreeNode (Tree format cs)) = indent ind ++ "LIST:" ++ format ++ showNodes (ind + 1) cs
showNode _ (DataNode i _) = i

parseFile :: String -> IO Tree
parseFile fn = return . createRiff . runGet parseRiffChunks =<< BL.readFile fn

main :: IO ()
main = putStrLn . showRiff . createRiff . runGet parseRiffChunks =<< BL.getContents


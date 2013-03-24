import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as BL (getContents, readFile)
import Data.List (intercalate)
import Control.Monad.State
import Control.Applicative

import RiffTokens

data Tree = DataNode Data
          | ListNode Riff
data Riff = Riff Format [Tree]

type ChunkMonad = State [Chunk]

getChunk :: ChunkMonad Chunk
getChunk = state (\(c:cs) -> (c, cs))

createTrees :: Int -> ChunkMonad [Tree]
createTrees 0 = return []
createTrees len = do
    c <- getChunk
    t <- createTree c
    ts <- createTrees (len - chunkLength c)
    return (t : ts)

createTree :: Chunk -> ChunkMonad Tree
createTree (DataChunk dat) = return (DataNode dat)
createTree (ListChunk list) = ListNode <$> tree
  where
    tree = createRiff list

createRiff :: List -> ChunkMonad Riff
createRiff (List len format) = do
        ts <- createTrees len
        return (Riff format ts)

evalRiff :: RiffChunks -> Riff
evalRiff (RiffChunks list cs) = evalState (createRiff list) cs

showRiff :: Riff -> String
showRiff (Riff format cs) = "RIFF:" ++ format ++ showTrees 1 cs

indent :: Int -> String
indent ind = '\n' : replicate (ind * 2) ' '

showTrees :: Int -> [Tree] -> String
showTrees ind nodes = '(' : intercalate "," (map (showTree ind) nodes) ++ ")"

showTree :: Int -> Tree -> String
showTree ind (ListNode (Riff format cs)) = indent ind ++ "LIST:" ++ format ++ showTrees (ind + 1) cs
showTree _ (DataNode dat) = dataId dat

parseFile :: String -> IO Riff
parseFile fn = return . evalRiff . runGet parseRiffChunks =<< BL.readFile fn

main :: IO ()
main = putStrLn . showRiff . evalRiff . runGet parseRiffChunks =<< BL.getContents


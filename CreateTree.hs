import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as BL (getContents)
import Data.List (intercalate)
import Control.Monad.State (State, state, evalState)
import Control.Applicative ((<$>), (<*>))

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
createTree (DataChunk dat) = DataNode <$> return dat
createTree (ListChunk list) = ListNode <$> createRiff list

createRiff :: List -> ChunkMonad Riff
createRiff (List len format) = Riff <$> return format <*> createTrees len

evalRiff :: RiffChunks -> Riff
evalRiff (RiffChunks list cs) = evalState (createRiff list) cs

showRoot :: Riff -> String
showRoot riff = "RIFF:" ++ showRiff riff

showRiff :: Riff -> String
showRiff (Riff format cs) = format ++ showTrees 1 cs

indent :: Int -> String
indent ind = '\n' : replicate (ind * 2) ' '

showTrees :: Int -> [Tree] -> String
showTrees ind nodes = '(' : intercalate "," (map (showTree ind) nodes) ++ ")"

showTree :: Int -> Tree -> String
showTree ind (ListNode riff) = indent ind ++ "LIST:" ++ showRiff riff
showTree _ (DataNode dat) = show dat

main :: IO ()
main = putStrLn . showRoot . evalRiff . runGet parseRiffChunks =<< BL.getContents


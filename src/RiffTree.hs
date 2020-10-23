module RiffTree
  ( Tree (Leaf, Node),
    Riff,
    riffFromBinary,
    showRoot,
  )
where

import Control.Monad.State
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Data.List
import RiffTokens

data Tree
  = Leaf Data
  | Node Riff

data Riff
  = Riff Format [Tree]

type ChunkMonad = State [Chunk]

getChunk :: ChunkMonad Chunk
getChunk = state (\(c : cs) -> (c, cs))

createTree :: Chunk -> ChunkMonad Tree
createTree (DataChunk dat) = return $ Leaf dat
createTree (ListChunk list) = Node <$> createRiff list

createTrees :: Int -> ChunkMonad [Tree]
createTrees 0 = return []
createTrees len = do
  c <- getChunk
  t <- createTree c
  ts <- createTrees (len - chunkLength c)
  return (t : ts)

createRiff :: List -> ChunkMonad Riff
createRiff (List len format) = do
  trees <- createTrees len
  return $ Riff format trees

evalRiff :: RiffFile -> Riff
evalRiff (RiffFile list cs) = evalState (createRiff list) cs

indent :: Int -> String
indent ind = '\n' : replicate (ind * 2) ' '

showTree :: Int -> Tree -> String
showTree ind (Node riff) = indent ind ++ "LIST:" ++ showRiff (ind + 1) riff
showTree _ (Leaf dat) = show dat

showTrees :: Int -> [Tree] -> String
showTrees ind nodes = '(' : intercalate "," (map (showTree ind) nodes) ++ ")"

showRiff :: Int -> Riff -> String
showRiff ind (Riff format cs) = format ++ showTrees ind cs

showRoot :: Riff -> String
showRoot riff = "RIFF:" ++ showRiff 1 riff

riffFromBinary :: BL.ByteString -> Riff
riffFromBinary = evalRiff . runGet parseRiffFile

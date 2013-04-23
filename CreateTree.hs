module RiffTree (
                Tree (Leaf, Forest)
              , Riff
              , riffFromBinary) where

import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as BL (getContents, ByteString)
import Data.List (intercalate)
import Control.Monad.State (State, state, evalState)
import Control.Applicative ((<$>), (<*>))

import RiffTokens

data Tree = Leaf Data
          | Forest Riff

data Riff = Riff Format [Tree]

type ChunkMonad = State [Chunk]

getChunk :: ChunkMonad Chunk
getChunk = state (\(c:cs) -> (c, cs))

createTree :: Chunk -> ChunkMonad Tree
createTree (DataChunk dat) = Leaf <$> return dat
createTree (ListChunk list) = Forest <$> createRiff list

createTrees :: Int -> ChunkMonad [Tree]
createTrees 0 = return []
createTrees len = do
    c <- getChunk
    t <- createTree c
    ts <- createTrees (len - chunkLength c)
    return (t : ts)

createRiff :: List -> ChunkMonad Riff
createRiff (List len format) = Riff <$> return format <*> createTrees len

evalRiff :: RiffChunks -> Riff
evalRiff (RiffChunks list cs) = evalState (createRiff list) cs

indent :: Int -> String
indent ind = '\n' : replicate (ind * 2) ' '

showTree :: Int -> Tree -> String
showTree ind (Forest riff) = indent ind ++ "LIST:" ++ showRiff (ind + 1) riff
showTree _ (Leaf dat) = show dat

showTrees :: Int -> [Tree] -> String
showTrees ind nodes = '(' : intercalate "," (map (showTree ind) nodes) ++ ")"

showRiff :: Int -> Riff -> String
showRiff ind (Riff format cs) = format ++ showTrees ind cs

showRoot :: Riff -> String
showRoot riff = "RIFF:" ++ showRiff 1 riff

riffFromBinary :: BL.ByteString -> Riff
riffFromBinary = evalRiff . runGet parseRiffChunks

main :: IO ()
main = putStrLn . showRoot . riffFromBinary =<< BL.getContents


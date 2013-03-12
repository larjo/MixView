import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

import RiffTokens
                       
showListInfo :: String -> List -> String
showListInfo i (l, f) = i ++ ":" ++ f ++ "(" ++ show l ++ ")"

showChunk :: Chunk -> String
showChunk c@(DataChunk (i, _)) = i ++ "(" ++ show (chunkLength c) ++ ")"
showChunk (ListChunk x) = showListInfo "LIST" x

printChunk :: Chunk -> IO ()
printChunk = putStrLn . showChunk

printRiff :: RiffChunks -> IO ()
printRiff (RiffChunks l cs) = do
    putStrLn $ showListInfo "RIFF" l
    mapM_ printChunk cs

main :: IO ()
main = BL.getContents >>= printRiff . runGet parseRiffChunks

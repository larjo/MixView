import Data.Binary.Get (runGet)
import qualified Data.ByteString.Lazy as BL (ByteString, readFile, getContents)
import RiffTokens
                       
showListInfo :: String -> List -> String
showListInfo i l = i ++ ":" ++ listFormat l ++ "(" ++ show (listLength l) ++ ")"

showChunk :: Chunk -> String
showChunk (DataChunk d) = dataId d ++ "(" ++ show (dataLength d) ++ ")"
showChunk (ListChunk x) = showListInfo "LIST" x

printChunk :: Chunk -> IO ()
printChunk = putStrLn . showChunk

printRiff :: RiffChunks -> IO ()
printRiff (RiffChunks l cs) = do
    putStrLn $ showListInfo "RIFF" l
    mapM_ printChunk cs

getChunks :: BL.ByteString -> IO RiffChunks
getChunks = return . runGet parseRiffChunks
    
main :: IO ()
main = BL.getContents >>= getChunks >>= printRiff

run :: FilePath -> IO RiffChunks
run fn = BL.readFile fn >>= getChunks

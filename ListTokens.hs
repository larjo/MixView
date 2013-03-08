import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

import RiffTokens
                       
showListInfo :: String -> ListInfo -> String
showListInfo i (ListInfo l f) = i ++ ":" ++ f ++ "(" ++ show l ++ ")"

showDataInfo :: DataInfo -> String
showDataInfo d@(DataInfo i _) = i ++ "(" ++ show (dataLength d) ++ ")"

showToken :: Token -> String
showToken (DataToken x) = showDataInfo x
showToken (ListToken x) = showListInfo "LIST" x

printToken :: Token -> IO ()
printToken = putStrLn . showToken

printRiff :: RiffFile -> IO ()
printRiff (RiffFile l ts) = do
    putStrLn $ showListInfo "RIFF" l
    mapM_ printToken ts
    
main :: IO ()
main = BL.getContents >>= printRiff . runGet parseRiffFile

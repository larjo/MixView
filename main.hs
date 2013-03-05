import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf16LE)
import Data.Binary.Get

import RiffTokens
                                                        
showToken :: Token -> String
showToken (Data i l _) = i ++ "(" ++ show l ++ ")"
showToken (List i l f) = i ++ ":" ++ f ++ "(" ++ show l ++ ")"

printToken :: Token -> IO ()
printToken = putStrLn . showToken

printTRKF :: Token -> IO ()
printTRKF (Data "TRKF" _ d) = putStrLn $ showRaw d
    where showRaw = T.unpack . T.init . decodeUtf16LE
printTRKF _ = return ()

main :: IO ()
main = BL.getContents >>= mapM_ printToken . runGet parseTokens

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

import RiffTokens
                                                        
showToken :: Token -> String
showToken (Data i l _) = i ++ "(" ++ show l ++ ")"
showToken (List i l f) = i ++ ":" ++ f ++ "(" ++ show l ++ ")"

printToken :: Token -> IO ()
printToken = putStrLn . showToken

main :: IO ()
main = BL.getContents >>= mapM_ printToken . runGet parseTokens

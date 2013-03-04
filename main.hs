import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Word
import Control.Applicative
import RiffTokens

printToken :: Token -> String
printToken (Data i l _) = i ++ "(" ++ show l ++ ")"
printToken (List i l f) = i ++ ":" ++ f ++ "(" ++ show l ++ ")"

printAll :: [Token] -> IO ()
printAll = mapM_ (print . printToken)

main :: IO ()
main = BL.getContents >>= printAll . runGet parseTokens

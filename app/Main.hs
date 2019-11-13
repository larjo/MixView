import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Control.Applicative ((<$>))

import CreateTree
import Id3
import ListFiles
import ListTokens

import System.Environment
import System.Exit

execute :: String -> BL.ByteString -> IO String
execute "tree" bs = return $ showRoot $ riffFromBinary bs
execute "id3" bs = return $ id3 bs
execute "listFiles" bs = return $ listFiles bs
execute "listTokens" bs = return $ listTokens bs
execute "listTags" bs = return $ listTags bs
execute "listIds" bs = return $ listIds bs
execute _ _ = usage >> exit

parse :: [ String ] -> IO String
parse [ command ] = do
    bs <- BL.getContents
    execute command bs
parse [ command, filePath ] = do
    bs <- BL.readFile filePath
    execute command bs
parse _ = usage >> exit

usage   = putStrLn "Usage: mixview command [file]"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)

main :: IO ()
main = getArgs >>= parse >>= putStrLn 

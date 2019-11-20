import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy as BL

import qualified RiffTree
import qualified Id3
import qualified RiffTokens

readFiles :: String -> IO String
readFiles fileName = show . Id3.listInfo <$> BL.readFile fileName

playlist :: BL.ByteString -> IO [ String ]
playlist = mapM readFiles . RiffTokens.listFiles

execute :: String -> BL.ByteString -> IO String
execute "id3" bs = return $ show $ Id3.listInfo bs
execute "id3-tags" bs = return $ Id3.listTags bs
execute "id3-ids" bs = return $ Id3.listIds bs
execute "riff-tree" bs = return $ RiffTree.showRoot $ RiffTree.riffFromBinary bs
execute "riff-files" bs = return $ show $ RiffTokens.listFiles bs
execute "riff-tokens" bs = return $ RiffTokens.listTokens bs
execute "songs" playlistBs = show <$> playlist playlistBs
execute _ _ = usage >> exit

parse :: [ String ] -> IO String
parse [ command ] = do
    bs <- BL.getContents
    execute command bs
parse [ command, filePath ] = do
    bs <- BL.readFile filePath
    execute command bs
parse _ = usage >> exit

usage :: IO ()
usage = do
    putStrLn "Usage: mixview command [file]"
    putStrLn "command:"
    putStrLn "id3"
    putStrLn "id3-tags"
    putStrLn "id3-ids"
    putStrLn "riff-tree"
    putStrLn "riff-files"
    putStrLn "riff-tokens"    
exit :: IO a
exit    = exitWith ExitSuccess
-- die     = exitWith (ExitFailure 1)

main :: IO ()
main = getArgs >>= parse >>= putStrLn 

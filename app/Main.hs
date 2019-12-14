import           Control.Arrow
import qualified Data.ByteString.Lazy as BL
import           Data.List
import           System.Environment
import           System.Exit
import           Text.Printf

import           Id3
import           RiffTokens
import           RiffTree

frequency :: Ord a => [a] -> [(Int, a)]
frequency = map (length &&& head) . group . sort

-- >>> frequency [ "a", "b", "a", "c", "c", "b", "d"]
-- [(2,"a"),(2,"b"),(2,"c"),(1,"d")]
--
formatInfo :: Mp3Info -> String
formatInfo info = title info ++ " - " ++ artist info

readFiles :: String -> IO String
readFiles fileName = formatInfo . listInfo <$> BL.readFile fileName

formatIndex :: Int -> String -> String
formatIndex i s = printf "%2i" i ++ ". " ++ s

formatList :: [String] -> String
formatList = intercalate "\n" . zipWith formatIndex [1 ..]

-- >>> take 10 $ zipWith (*) [1 ..] [2 ..] :: [ Int ]
-- [2,6,12,20,30,42,56,72,90,110]
--
duplicates :: [String] -> String
duplicates = intercalate "\n" . map snd . filter ((> 1) . fst) . frequency

execute :: String -> BL.ByteString -> IO String
execute "id3" bs         = return $ show $ listInfo bs
execute "id3-tags" bs    = return $ listTags bs
execute "id3-ids" bs     = return $ listIds bs
execute "riff-tree" bs   = return $ showRoot $ riffFromBinary bs
execute "riff-files" bs  = return $ show $ listFiles bs
execute "riff-tokens" bs = return $ listTokens bs
execute "playlist" bs    = fmap formatList $ mapM readFiles $ listFiles bs
execute "duplicates" bs  = fmap duplicates $ mapM readFiles $ listFiles bs
execute _ _              = usage >> exit

parse :: [String] -> IO String
parse [command] = do
    bs <- BL.getContents
    execute command bs
parse [command, filePath] = do
    bs <- BL.readFile filePath
    execute command bs
parse ["playlist", n, filePath] = do
    bs <- BL.readFile filePath
    fmap (formatList . take (read n)) $ mapM readFiles $ listFiles bs
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
    putStrLn "playlist"
    putStrLn "playlist n"

exit :: IO a
exit = exitSuccess

-- die     = exitWith (ExitFailure 1)
main :: IO ()
main = getArgs >>= parse >>= putStrLn

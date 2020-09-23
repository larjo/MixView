module Main where

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

-- >>> frequency [ "a", "b", "a", "a", "b", "a", "c", "c", "b", "d"]
-- [(4,"a"),(3,"b"),(2,"c"),(1,"d")]

formatInfo :: Mp3Info -> String
formatInfo info = title info ++ " - " ++ artist info

readFiles :: [String] -> IO [String]
readFiles = mapM (fmap (formatInfo . listInfo) . BL.readFile)

formatIndex :: Int -> String -> String
formatIndex i s = printf "%2i" i ++ ". " ++ s

formatList :: [String] -> String
formatList = unlines . zipWith formatIndex [1 ..]

-- >>> take 10 $ zipWith (*) [1 ..] [2 ..] :: [ Int ]
-- [2,6,12,20,30,42,56,72,90,110]

duplicates :: [String] -> [String]
duplicates = map snd . filter ((> 1) . fst) . frequency

-- >>> duplicates ["a", "b", "a", "a", "b", "a", "c", "c", "b", "d"]
-- ["a","b","c"]


execute :: String -> BL.ByteString -> IO String
execute "id3" bs         = return $ show $ listInfo bs
execute "id3-tags" bs    = return $ unlines $ listTags bs
execute "id3-ids" bs     = return $ unlines $ listIds bs
execute "riff-tree" bs   = return $ showRoot $ riffFromBinary bs
execute "riff-files" bs  = return $ unlines $ listFiles bs
execute "riff-tokens" bs = return $ listTokens bs
execute "playlist" bs    = formatList <$> readFiles (listFiles bs)
execute "duplicates" bs  = unlines . duplicates <$> readFiles (listFiles bs)
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
    formatList . take (read n) <$> readFiles (listFiles bs)
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

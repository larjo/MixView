module Main where

import Control.Arrow (Arrow ((&&&)))
import qualified Data.ByteString.Lazy as BL
import Data.List (group, sort)
import Data.Maybe (mapMaybe)
import Id3 (Mp3Info (..), listIds, listInfo, listTags)
import RiffTokens (listFiles, listTokens)
import RiffTree (riffFromBinary, showRoot)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Text.Printf (printf)
import Data.Functor ((<&>))

-- >>> frequency [ "a", "b", "a", "a", "b", "a", "c", "c", "b", "d"]
-- [(4,"a"),(3,"b"),(2,"c"),(1,"d")]

formatInfo :: Mp3Info -> String
formatInfo info = title info ++ " - " ++ artist info

readFiles :: [String] -> IO [String]
readFiles = mapM (fmap (formatInfo . listInfo) . BL.readFile)

formatIndex :: Int -> String -> String
formatIndex = printf "%2i. %s"

-- >>> formatIndex 3 "test"
-- " 3. test"
-- >>> formatIndex 13 "test"
-- "13. test"

formatList :: [String] -> String
formatList = unlines . zipWith formatIndex [1 ..]

-- >>> formatList [ "aaa", "bbb" ]
-- " 1. aaa\n 2. bbb\n"

frequency :: Ord a => [a] -> [(Int, a)]
frequency = map (length &&& head) . group . sort

-- >>> group . sort $ [1, 5, 4, 5, 3, 2, 1, 1, 4]
-- [[1,1,1],[2],[3],[4,4],[5,5]]

-- >>> frequency [1, 5, 4, 5, 3, 2, 1, 1, 4]
-- [(3,1),(1,2),(1,3),(2,4),(2,5)]

keepDuplicate :: (Int, b) -> Maybe b
keepDuplicate (count, a)
  | count > 1 = Just a
  | otherwise = Nothing

-- >>> keepDuplicate (3, "b")
-- Just "b"

-- >>> keepDuplicate (1, "c")
-- Nothing

duplicates :: [String] -> [String]
duplicates = mapMaybe keepDuplicate . frequency

-- >>> duplicates ["a", "b", "a", "a", "b", "a", "c", "c", "b", "d"]
-- ["a","b","c"]

execute :: String -> BL.ByteString -> IO String
execute "id3" = return . show . listInfo 
execute "id3-tags" = return . unlines . listTags 
execute "id3-ids" = return . unlines . listIds 
execute "riff-tree" = return . showRoot . riffFromBinary 
execute "riff-files" = return . unlines . listFiles 
execute "riff-tokens" = return . listTokens 
execute "playlist" = fmap formatList . readFiles . listFiles  
execute "duplicates" = return . unlines . duplicates . listFiles
execute _ = const $ usage >> exit


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

module Main where

import Control.Arrow ( Arrow((&&&)) )
import qualified Data.ByteString.Lazy as BL
import Data.List ( group, sort )
import Data.Maybe ( mapMaybe )
import Id3 ( Mp3Info(..), listInfo, listTags, listIds )
import RiffTokens ( listFiles, listTokens )
import RiffTree ( riffFromBinary, showRoot )
import System.Environment ( getArgs )
import System.Exit ( exitSuccess )
import Text.Printf ( printf )

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

select :: (Int, b) -> Maybe b
select (count, a)
  | count > 1 = Just a
  | otherwise = Nothing

-- >>> select (3, "b")
-- Just "b"

-- >>> select (1, "c")
-- Nothing

duplicates :: [String] -> [String]
duplicates = mapMaybe select . frequency

-- >>> duplicates ["a", "b", "a", "a", "b", "a", "c", "c", "b", "d"]
-- ["a","b","c"]

execute :: String -> BL.ByteString -> IO String
execute "id3" bs = return $ show $ listInfo bs
execute "id3-tags" bs = return $ unlines $ listTags bs
execute "id3-ids" bs = return $ unlines $ listIds bs
execute "riff-tree" bs = return $ showRoot $ riffFromBinary bs
execute "riff-files" bs = return $ unlines $ listFiles bs
execute "riff-tokens" bs = return $ listTokens bs
execute "playlist" bs = formatList <$> readFiles (listFiles bs)
execute "duplicates" bs = unlines . duplicates <$> readFiles (listFiles bs)
execute _ _ = usage >> exit

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

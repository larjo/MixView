import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (runGet)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.List (intercalate)
import Control.Applicative

import RiffTokens

type Id = String
type Len = Int
type Format = String
type RawData = ByteString

data List = List Format [Node]
data Data = Data Id RawData

data Node = ListNode List
          | DataNode Data 
type RiffTree = List

createRiff :: RiffFile -> RiffTree
createRiff (RiffFile (ListChunk len format) tokens) =
    List format (createNodes len tokens)

createNodes :: Len -> [Token] -> [Node]
createNodes len ts = []

idMap :: (a -> b) -> [a] -> [(a, b)]
idMap f = map $ (,) <$> id <*> f

scanSecond :: (b -> b -> b) -> [(a,b)] -> [(a,b)]
scanSecond f = scanl1 g
             where
               g (_, y) (x, y') = (x, f y y')

splitTokens :: Int -> [Token] -> ([Token], [Token])
splitTokens s  l =
            (map fst a, map fst b)
          where
            (a, b) = span ((<= s) . snd) . scanSecond (+) . idMap tokenLength $ l

sumNodes :: [Token] -> [(Token, Int)]
sumNodes = map $ (,) <$> id <*> tokenLength

accNodes :: [Int] -> [Int]
accNodes = tail . scanl (+) 0

-- tokenSums :: [Token] -> [(Token, Int)]
-- tokenSums tokens = zip tokens (accNodes $ sumNodes tokens)
-- 
-- splitTokens :: [Token] -> Len -> ([Token], [Token])
-- splitTokens tokens len =
--     span (<= len) acc
--   where
--     acc = accNodes $ map tokenLength tokens
-- 
-- createChunks len (t@DataToken id dat : rest)
    -- | len > 0 = Data id dat : createChunks (len - tokenLength t)
    -- | otherwise = []
-- createChunks len (t@ListToken listlen format : rest)
    -- | len > 0 = List format (createChunks listlen rest) : createChunks (len - tokenlength t)   

showRiff :: RiffTree -> String
showRiff (List format cs) = "RIFF:" ++ format ++ showNodes cs

showNodes :: [Node] -> String
showNodes nodes = "(" ++ intercalate "," (map showNode nodes) ++ ")"

showNode :: Node -> String
showNode (ListNode (List format cs)) = "LIST:" ++ format ++ showNodes cs
showNode (DataNode (Data i d)) = i

main :: IO ()
--main = BL.getContents >>= putStrLn . showRiff . createRiff . runGet parseRiffFile
main = BL.getContents >>= print . map snd . showTokenSum . runGet parseRiffFile

showTokenSum (RiffFile _ tokens) = [("", "")] -- tokenSums tokens

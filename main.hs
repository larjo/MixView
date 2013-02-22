import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary.Get
import Data.Word
import Control.Applicative
import RiffTokens
  
-- The datastructure that represents a playlist in Riff-format
type Format = String
type Id = String
type RawData = B.ByteString

data Chunk = Data Id RawData | List Chunks
data Chunks = Chunks Format [Chunk] 
data Riff = Riff Chunks

-- functions for pretty printing the playlist
instance Show Chunk where
   show (Data i d) = "(" ++ i ++ ")"
   show cs = show cs

instance Show Chunks where
   show (Chunks f c) = f ++ ":" ++ (show c)
  
instance Show Riff where
   show (Riff cs) = show cs
  
main :: IO ()
main = BL.getContents >>= print . runGet parseTokens
    
-- test = show r 
  -- where
    -- d = B8.pack "data"
    -- c = Data "DAT1" d
    -- c2 = Data "DAT2"  d
    -- cs = Chunks "CHKS" [c, c2]
    -- c3 = List cs
    -- c4 = Data "DAT3" d
    -- r = Riff (Chunks "HEAD" [c3,c4])
    
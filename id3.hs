import           Control.Applicative   ((<$>))
import           Control.Monad         (replicateM)
import           Control.Monad.Loops   (whileM)
import           Data.Binary.Get       (Get, bytesRead, getByteString,
                                        getWord32be, getWord8, lookAhead,
                                        runGet, skip)
import           Data.ByteString       as B (any)
import           Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy  as BL (ByteString, getContents)
import           Data.List             (intercalate)
import qualified Data.Map              as M (Map, empty, findWithDefault,
                                             insert)
import qualified Data.Text             as T (init, null, unpack)
import           Data.Text.Encoding    (decodeUtf16LEWith)

data Frame = Frame
    { tag     :: String
    , content :: String
    } deriving Show

type FrameMap = M.Map String String

header :: Get String
header = unpack <$> getByteString 3

version :: Get String
version = intercalate "." . map show <$> replicateM 2 getWord8

showRaw = T.unpack . safeInit . decodeUtf16LEWith (\_ _ -> Nothing)
  where
    safeInit x = if T.null x then x else T.init x

frame :: Get Frame
frame = do
    id <- getByteString 4
    size <- fromIntegral <$> getWord32be
    skip 2 -- skip flags
    skip 3 -- extra + bom
    bs <- getByteString $ size - 3
    return $ Frame (unpack id) (showRaw bs)

framesLeft :: Int -> Get Bool
framesLeft size = do
    br <- fromIntegral <$> bytesRead
    id <- lookAhead (getByteString 4)
    return $ (B.any ( /= 0) id) && (br < size)

getSize :: Get Int
getSize = (+ 10) . foldl (\s x -> 128*s + x) 0 . map fromIntegral <$> replicateM 4 getWord8

parseId3 :: Get [Frame]
parseId3 = do
    header
    version
    skip 1 -- skip flags
    size <- getSize
    whileM (framesLeft size) frame

insertFrame :: FrameMap -> Frame -> FrameMap
insertFrame fm (Frame id str) = M.insert id str fm

lookupFrame :: FrameMap -> String -> String
lookupFrame fm k = M.findWithDefault "" k fm

titleArtist :: FrameMap -> [String]
titleArtist fm = map (lookupFrame fm) ["TIT2", "TPE1"]

parseTags :: BL.ByteString -> [String]
parseTags = titleArtist . foldl insertFrame M.empty . runGet parseId3

main :: IO ()
main = BL.getContents >>= print . parseTags

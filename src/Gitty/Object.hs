module Gitty.Object (makeContent, hashContent, writeContent) where

import Codec.Compression.Zlib (compress, decompress)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import Data.Function ((&))
import Data.List (intercalate)
import qualified Data.Map as Map
import Gitty.Prelude (WorkDir, bsToHex)
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import Text.Printf (printf)
import Prelude hiding (init)

type ObjectHash = String

type ObjectContent = BS.ByteString

type ObjectRawContent = BS.ByteString

data ObjectKind = Blob | Commit | Tree | Tag
  deriving (Show)

dir :: FilePath -> FilePath
dir = (++ "/objects") . gittyDir

kindToString :: ObjectKind -> String
kindToString = (map toLower) . show

makeContent :: ObjectRawContent -> ObjectKind -> ObjectContent
makeContent rawContent kind = content
  where
    contentLength = BS.length rawContent
    contentHeader = C.pack $ kindToString kind <> " " <> show contentLength
    content = contentHeader <> C.pack "\0" <> rawContent

hashContent :: WorkDir -> ObjectContent -> ObjectHash
hashContent workDir content = ()
  where
    hashedContent = content & SHA1.hash & bsToHex
    fileDir = dir workDir <> "/" <> take 2 hashedContent
    fileName = fileDir <> "/" <> drop 2 hashedContent

writeContent :: WorkDir -> (ObjectHash, ObjectContent) -> IO ()
writeContent workDir = do
  compressedContent = content & BS.fromStrict & compress & BS.toStrict
  Directory.createDirectory fileDir
  BS.writeFile fileName compressedContent

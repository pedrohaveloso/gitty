module Gitty.Object
  ( Hash,
    Content,
    RawContent,
    makeContent,
    hashContent,
    writeContent,
    readContent,
    kindFromString,
    kindToString,
  )
where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import Data.Function ((&))
import Gitty.Compression (compress, decompress)
import Gitty.Prelude (WorkDir, bsToHex, repoDir)
import qualified System.Directory as Directory

type Hash = String

type Content = BS.ByteString

type RawContent = BS.ByteString

data ObjectKind = Blob | Commit | Tree | Tag
  deriving (Show)

dir :: FilePath -> FilePath
dir = (++ "/objects") . repoDir

kindFromString :: String -> ObjectKind
kindFromString "commit" = Commit
kindFromString "tree" = Tree
kindFromString "tag" = Tag
kindFromString _ = Blob

kindToString :: ObjectKind -> String
kindToString = (map toLower) . show

makeContent :: RawContent -> ObjectKind -> Content
makeContent rawContent kind = content
  where
    contentLength = BS.length rawContent
    contentHeader = C.pack $ kindToString kind <> " " <> show contentLength
    content = contentHeader <> C.pack "\0" <> rawContent

hashContent :: Content -> Hash
hashContent content = content & SHA1.hash & bsToHex

makeFileDir :: WorkDir -> Hash -> FilePath
makeFileDir workDir hash = dir workDir <> "/" <> take 2 hash

makeFilePath :: WorkDir -> Hash -> FilePath
makeFilePath workDir hash = (makeFileDir workDir hash) <> "/" <> drop 2 hash

writeContent :: WorkDir -> Hash -> Content -> IO ()
writeContent workDir hash content = do
  Directory.createDirectory fileDir
  BS.writeFile filePath compressed
  where
    compressed = compress content
    fileDir = makeFileDir workDir hash
    filePath = makeFilePath workDir hash

readContent :: WorkDir -> Hash -> IO (Maybe Content)
readContent workDir hash = do
  exists <- Directory.doesFileExist filePath

  if exists
    then readContent'
    else return Nothing
  where
    filePath = makeFilePath workDir hash

    readContent' :: IO (Maybe Content)
    readContent' = do
      content <- BS.readFile filePath
      let decompressed = decompress content

      return $ Just decompressed
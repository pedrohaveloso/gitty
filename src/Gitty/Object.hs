module Gitty.Object
  ( Hash,
    Content,
    RawContent,
    Kind (..),
    makeContent,
    hashContent,
    writeContent,
    readContent,
    kindFromString,
    kindToString,
    hashFile,
  )
where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.Char (toLower)
import Data.Function ((&))
import Gitty.Compression (compress, decompress)
import Gitty.Prelude (WorkDir, bsToHex, makeAbsoluteFrom, repoDir)
import qualified Gitty.Validation as Validation
import qualified System.Directory as Directory

type Hash = String

type Content = ByteString.ByteString

type RawContent = ByteString.ByteString

data Kind = Blob | Commit | Tree | Tag
  deriving (Show)

dir :: FilePath -> FilePath
dir = (++ "/objects") . repoDir

kindFromString :: String -> Kind
kindFromString "commit" = Commit
kindFromString "tree" = Tree
kindFromString "tag" = Tag
kindFromString _ = Blob

kindToString :: Kind -> String
kindToString = (map toLower) . show

makeContent :: RawContent -> Kind -> Content
makeContent rawContent kind = content
  where
    contentLength = ByteString.length rawContent
    contentHeader = Char8.pack $ kindToString kind <> " " <> show contentLength
    content = contentHeader <> Char8.pack "\0" <> rawContent

hashContent :: Content -> Hash
hashContent content = content & SHA1.hash & bsToHex

makeFileDir :: WorkDir -> Hash -> FilePath
makeFileDir workDir hash = dir workDir <> "/" <> take 2 hash

makeFilePath :: WorkDir -> Hash -> FilePath
makeFilePath workDir hash = (makeFileDir workDir hash) <> "/" <> drop 2 hash

writeContent :: WorkDir -> Hash -> Content -> IO ()
writeContent workDir hash content = do
  Directory.createDirectory fileDir
  ByteString.writeFile filePath compressed
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
      content <- ByteString.readFile filePath
      let decompressed = decompress content

      return $ Just decompressed

hashFile :: WorkDir -> Bool -> Kind -> FilePath -> IO (Either String Hash)
hashFile workDir write kind file = do
  fileError <- Validation.fileAccess workDir file

  case fileError of
    Nothing -> hashFile'
    Just err -> return $ Left err
  where
    absoluteFile :: String
    absoluteFile = makeAbsoluteFrom workDir file

    hashFile' :: IO (Either String Hash)
    hashFile' = do
      rawContent <- ByteString.readFile absoluteFile

      let content = makeContent rawContent kind
          hash = hashContent content

      if write
        then do
          writeContent workDir hash content
          return $ Right hash
        else return $ Right hash
module Gitty.Index (updateIndex) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.Function ((&))
import Data.Typeable (typeOf)
import Gitty.Compression (compress, decompress)
import qualified Gitty.Object as Object
import Gitty.Prelude (WorkDir, repoDir)
import qualified Gitty.Validation as Validation
import qualified System.Directory as Directory
import Text.Read (readMaybe)

file :: WorkDir -> FilePath
file = repoDir . (<> "/index")

data Entry = Entry
  { mode :: Int,
    hash :: Object.Hash,
    path :: FilePath
  }
  deriving (Show, Read)

data Index = Index
  { entries :: [Entry],
    checksum :: String
  }
  deriving (Show, Read)

defaultIndex :: Index
defaultIndex = Index {entries = [], checksum = ""}

writeIndex :: WorkDir -> Index -> IO ()
writeIndex workDir index = do
  ByteString.writeFile (file workDir) compressed
  where
    content :: Index -> ByteString.ByteString
    content idx = show idx & Char8.pack

    makeChecksum :: String
    makeChecksum = SHA1.hash (content index) & Char8.unpack

    compressed :: ByteString.ByteString
    compressed = compress (index {checksum = makeChecksum} & content)

readIndex :: WorkDir -> IO Index
readIndex workDir = do
  exists <- Directory.doesFileExist $ file workDir

  if exists then readIndex' else return defaultIndex
  where
    readIndex' :: IO Index
    readIndex' = do
      compressed <- ByteString.readFile (file workDir)
      let content = decompress compressed & Char8.unpack

      -- todo validate checksum

      case readMaybe content of
        Nothing -> return defaultIndex
        Just index ->
          case show (typeOf index) of
            "Index" -> return index
            _ -> return defaultIndex

updateIndex :: WorkDir -> Bool -> FilePath -> Maybe (String, Object.Hash) -> IO ()
updateIndex workDir add file cacheinfo = do
  fileError <- Validation.fileAccess workDir file

  case fileError of
    Nothing -> hashFile'
    Just err -> return $ Left err

  index <- readIndex workDir

  print ""

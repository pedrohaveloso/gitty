{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Index
  ( updateIndex,
    defaultUpdateIndexOptions,
    UpdateIndexOptions (..),
  )
where

import qualified Crypto.Hash.SHA1 as SHA1
import Data.Bits ((.&.))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.Function ((&))
import Gitty.Compression (compress, decompress)
import qualified Gitty.Object as Object
import Gitty.Prelude (WorkDir, makeAbsoluteFrom, repoDir)
import qualified Gitty.Validation as Validation
import qualified System.Directory as Directory
import System.Posix.Files (fileMode, getFileStatus)
import Text.Read (readMaybe)

indexFile :: WorkDir -> FilePath
indexFile workDir = repoDir workDir <> "/index"

data Entry = Entry
  { mode :: String,
    hash :: Object.Hash,
    path :: FilePath
  }
  deriving (Show, Read, Eq)

instance Ord Entry where
  compare e1 e2 = compare e1.path e2.path

type Checksum = String

data Index = Index
  { entries :: [Entry],
    checksum :: Checksum
  }
  deriving (Show, Read)

defaultIndex :: Index
defaultIndex =
  index {checksum = makeChecksum index}
  where
    index = Index {entries = [], checksum = ""}

showIndex :: Index -> ByteString.ByteString
showIndex idx = show idx & Char8.pack

makeChecksum :: Index -> String
makeChecksum index = SHA1.hash (showIndex index) & Char8.unpack

checksumIsValid :: Index -> Bool
checksumIsValid index = index.checksum == makeChecksum index

writeIndex :: WorkDir -> Index -> IO ()
writeIndex workDir index =
  index {checksum = makeChecksum index}
    & showIndex
    & compress
    & ByteString.writeFile (indexFile workDir)

readIndex :: WorkDir -> IO (Either String Index)
readIndex workDir = do
  exists <- Directory.doesFileExist $ indexFile workDir

  if exists then readIndex' else return $ Right defaultIndex
  where
    readIndex' :: IO (Either String Index)
    readIndex' = do
      compressed <- ByteString.readFile (indexFile workDir)
      let content = decompress compressed & Char8.unpack

      case readMaybe content of
        Nothing -> return $ Left "Invalid index file"
        Just index
          | checksumIsValid index -> return $ Right index
          | otherwise -> return $ Left "Bad index file sha1 signature, index file corrupt"

getFileMode :: FilePath -> IO String
getFileMode filePath = do
  status <- getFileStatus filePath
  let rawMode = fileMode status
      isExecutable = rawMode .&. 0o111 /= 0
  return $ if isExecutable then "100755" else "100644"

data UpdateIndexOptions = UpdateIndexOptions
  { file :: FilePath,
    add :: Bool,
    mode :: Maybe String,
    object :: Maybe Object.Hash
  }

defaultUpdateIndexOptions :: UpdateIndexOptions
defaultUpdateIndexOptions =
  UpdateIndexOptions
    { file = "",
      add = False,
      mode = Nothing,
      object = Nothing
    }

updateIndex :: WorkDir -> UpdateIndexOptions -> IO (Either String ())
updateIndex workDir options =
  Validation.fileAccess workDir options.file >>= \case
    Just err -> return $ Left err
    Nothing -> updateIndex'
  where
    updateIndex' :: IO (Either String ())
    updateIndex' = do
      mode <- makeFileMode
      hash <- case options.object of
        Just hs -> return $ Object.validateHash hs
        Nothing -> Object.hashFile workDir True Object.Blob options.file
      index <- readIndex workDir

      case (index, hash) of
        (Left err, _) -> return $ Left err
        (_, Left err) -> return $ Left err
        (Right idx, Right h) -> applyEntry idx h mode

    applyEntry :: Index -> Object.Hash -> String -> IO (Either String ())
    applyEntry index hash mode = do
      let newEntry = Entry {mode = mode, path = absoluteFile, hash = hash}
          filtered = filter (\e -> e.path /= newEntry.path) index.entries
          isNew = length filtered == length index.entries

      if not options.add && isNew
        then return $ Left $ "Cannot add " <> options.file <> " to the index, missing --add option?"
        else do
          writeIndex workDir index {entries = newEntry : filtered}
          return $ Right ()

    absoluteFile :: String
    absoluteFile = makeAbsoluteFrom workDir options.file

    makeFileMode :: IO String
    makeFileMode = case options.mode of
      Nothing -> getFileMode options.file
      Just fm -> return fm

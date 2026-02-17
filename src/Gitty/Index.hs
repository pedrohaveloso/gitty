module Gitty.Index (addCacheInfo, addFiles) where

import qualified Crypto.Hash.SHA1 as SHA1
import Data.Bits ((.&.))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.Function ((&))
import Data.List (sort)
import Data.Typeable (typeOf)
import Gitty.Compression (compress, decompress)
import qualified Gitty.Object as Object
import Gitty.Prelude (WorkDir, makeAbsoluteFrom, repoDir)
import qualified Gitty.Validation as Validation
import qualified System.Directory as Directory
import System.Posix.Files (fileMode, getFileStatus)
import Text.Read (readMaybe)

file :: WorkDir -> FilePath
file workDir = repoDir workDir <> "/index"

data Entry = Entry
  { mode :: Int,
    hash :: Object.Hash,
    path :: FilePath
  }
  deriving (Show, Read, Eq)

instance Ord Entry where
  compare e1 e2 = compare (path e1) (path e2)

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

      case readMaybe content of
        Nothing -> return defaultIndex
        Just index ->
          case show (typeOf index) of
            "Index" -> return index
            _ -> return defaultIndex

addOrReplaceEntry :: Entry -> [Entry] -> [Entry]
addOrReplaceEntry newEntry entries = sort $ newEntry : filteredEntries
  where
    filteredEntries = filter (\e -> path e /= path newEntry) entries

parseMode :: String -> Either String Int
parseMode modeStr = case readMaybe modeStr of
  Just m -> Right m
  Nothing -> Left $ "Invalid mode: " <> modeStr

validateHash :: String -> Either String Object.Hash
validateHash h
  | length h == 40 = Right h
  | otherwise = Left $ "Invalid hash length: expected 40 characters, got " <> show (length h)

addCacheInfo :: WorkDir -> String -> String -> FilePath -> IO (Either String ())
addCacheInfo workDir modeStr hashStr entryPath = do
  index <- readIndex workDir

  case parseMode modeStr of
    Left err -> return $ Left err
    Right modeInt -> case validateHash hashStr of
      Left err -> return $ Left err
      Right validHash -> do
        let entry = Entry {mode = modeInt, hash = validHash, path = entryPath}
            newEntries = addOrReplaceEntry entry (entries index)
            newIndex = index {entries = newEntries}

        writeIndex workDir newIndex
        return $ Right ()

getFileMode :: FilePath -> IO Int
getFileMode filePath = do
  status <- getFileStatus filePath
  let rawMode = fileMode status
      isExecutable = rawMode .&. 0o111 /= 0
  return $ if isExecutable then 100755 else 100644

addFile :: WorkDir -> FilePath -> IO (Either String ())
addFile workDir filePath = do
  fileError <- Validation.fileAccess workDir filePath

  case fileError of
    Just err -> return $ Left err
    Nothing -> do
      hashResult <- Object.hashFile workDir True Object.Blob filePath
      case hashResult of
        Left err -> return $ Left err
        Right h -> do
          let absoluteFile = makeAbsoluteFrom workDir filePath
          m <- getFileMode absoluteFile
          index <- readIndex workDir

          let entry = Entry {mode = m, hash = h, path = filePath}
              newEntries = addOrReplaceEntry entry (entries index)
              newIndex = index {entries = newEntries}

          writeIndex workDir newIndex
          return $ Right ()

addFiles :: WorkDir -> [FilePath] -> IO (Either String ())
addFiles workDir files = addFiles' files
  where
    addFiles' [] = return $ Right ()
    addFiles' (f : rest) = do
      result <- addFile workDir f
      case result of
        Left err -> return $ Left err
        Right () -> addFiles' rest

data UpdateIndexOptions = UpdateIndexOptions
  {
  }

updateIndex :: WorkDir -> FilePath -> UpdateIndexOptions -> IO ()
updateIndex workDir file options = undefined

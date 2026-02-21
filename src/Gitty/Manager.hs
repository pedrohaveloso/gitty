{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Manager
  ( Object (..),
    ObjectId (..),
    ObjectKind (..),
    objectKindFromString,
    makeObject,
    writeObject,
    readObject,
  )
where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.Char (toLower)
import Data.Function ((&))
import Gitty.Common (WorkDir, byteStringToHex, compress, decompress, makeRepoDir)
import qualified System.Directory as Directory
import System.FilePath ((</>))
import Text.Read (readMaybe)

newtype ObjectId = ObjectId String
  deriving (Read)

instance Show ObjectId where
  show :: ObjectId -> String
  show (ObjectId oid) = oid

data ObjectKind = Blob | Commit | Tree | Tag
  deriving (Show, Read)

objectKindFromString :: String -> ObjectKind
objectKindFromString raw
  | kind == "commit" = Commit
  | kind == "tree" = Tree
  | kind == "tag" = Tag
  | otherwise = Blob
  where
    kind = map toLower raw

data Object = Object
  { kind :: ObjectKind,
    len :: Int,
    content :: ByteString.ByteString
  }

type ObjectRawContent = ByteString.ByteString

serializeObject :: Object -> ByteString.ByteString
serializeObject object = header <> object.content
  where
    header :: ByteString.ByteString
    header = Char8.pack (show object.kind <> " " <> show object.len <> "\0")

deserializeObject :: ByteString.ByteString -> Maybe Object
deserializeObject rawObject = do
  let (header, rest) = ByteString.break (== 0) rawObject

  content <-
    if ByteString.null rest
      then Nothing
      else Just $ ByteString.drop 1 rest

  let (rawKind, rawLength) = break (== ' ') (Char8.unpack header)

  kind <- readMaybe rawKind
  len <- readMaybe $ dropWhile (== ' ') rawLength

  Just Object {kind = kind, len = len, content = content}

makeObject :: ObjectKind -> ObjectRawContent -> (ObjectId, Object)
makeObject kind rawContent = (oid, object)
  where
    object :: Object
    object =
      Object
        { kind = kind,
          len = ByteString.length rawContent,
          content = rawContent
        }

    oid :: ObjectId
    oid = ObjectId (object & serializeObject & SHA1.hash & byteStringToHex)

writeObject :: WorkDir -> (ObjectId, Object) -> IO ()
writeObject workDir (ObjectId oid, object) =
  Directory.createDirectoryIfMissing True fileDir
    >> ByteString.writeFile filePath content
  where
    content = object & serializeObject & compress
    fileDir = makeRepoDir workDir </> "/objects/" </> take 2 oid
    filePath = fileDir </> "/" </> drop 2 oid

readObject :: WorkDir -> ObjectId -> IO (Maybe Object)
readObject workDir (ObjectId oid) = do
  exists <- Directory.doesFileExist filePath

  if exists
    then do
      content <- ByteString.readFile filePath
      return $ deserializeObject $ decompress content
    else return Nothing
  where
    fileDir = makeRepoDir workDir </> "/objects/" </> take 2 oid
    filePath = fileDir </> "/" </> drop 2 oid

data IndexEntry = IndexEntry
  { mode :: String,
    oid :: ObjectId,
    path :: FilePath
  }
  deriving (Eq)

instance Ord IndexEntry where
  compare e1 e2 = compare e1.path e2.path

newtype Index = Index {entries :: [IndexEntry]}

defaultIndex :: Index
defaultIndex = Index {entries = []}

serializeIndex :: Index -> ByteString.ByteString
serializeIndex index =
  map serializeEntry index.entries
    & ByteString.intercalate (Char8.pack "\n")
  where
    serializeEntry :: IndexEntry -> ByteString.ByteString
    serializeEntry entry =
      Char8.pack (entry.mode <> ";" <> show entry.oid <> ";" <> entry.path)

deserializeIndex :: ByteString.ByteString -> Maybe Index
deserializeIndex rawObject = do
  entries <- mapM deserializeEntry (Char8.lines rawObject)
  Just Index {entries = entries}
  where
    deserializeEntry :: ByteString.ByteString -> Maybe IndexEntry
    deserializeEntry line =
      case Char8.split ';' line of
        [rawMode, rawOid, rawPath] -> do
          oid <- readMaybe (Char8.unpack rawOid)
          Just
            IndexEntry
              { mode = Char8.unpack rawMode,
                oid = oid,
                path = Char8.unpack rawPath
              }
        _ -> Nothing

writeIndex :: WorkDir -> Index -> IO ()
writeIndex workDir index =
  index
    & serializeIndex
    & compress
    & ByteString.writeFile (makeRepoDir workDir </> "/index")

readIndex :: WorkDir -> IO (Either String Index)
readIndex workDir = do
  exists <- Directory.doesFileExist (makeRepoDir workDir </> "/index")

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

data UpdateIndexOptions = UpdateIndexOptions
  { file :: FilePath,
    add :: Bool,
    mode :: String,
    oid :: ObjectId
  }
  deriving (Show)

updateIndex :: WorkDir -> UpdateIndexOptions -> IO (Either String ())
updateIndex workDir options =
  Validation.fileAccess workDir options.file >>= \case
    Just err -> return $ Left err
    Nothing -> updateIndex'
  where
    updateIndex' :: IO (Either String ())
    updateIndex' =
      readIndex workDir >>= \case
        Left err -> return $ Left err
        Right index -> do
          let newIndexEntry =
                IndexEntry
                  { mode = options.mode,
                    path = absoluteFile,
                    oid = options.object
                  }
              filtered = filter (\e -> e.path /= newIndexEntry.path) index.entries
              isNew = length filtered == length index.entries

          if not options.add && isNew
            then
              return $
                Left $
                  "Cannot add " <> options.file <> " to the index"
            else do
              writeIndex workDir index {entries = newIndexEntry : filtered}
              return $ Right ()

    absoluteFile :: String
    absoluteFile = makeAbsoluteFrom workDir options.file

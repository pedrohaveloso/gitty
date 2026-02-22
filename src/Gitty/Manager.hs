{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Manager
  ( Obj (..),
    ObjId (..),
    ObjKind (..),
    objKindFromString,
    validateObjId,
    makeObj,
    writeObj,
    readObj,
    Idx (..),
    IdxEntry (..),
    readIdx,
    writeIdx,
    idxEntryExists,
    insertIdxEntry,
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

newtype ObjId = ObjId String
  deriving (Read, Eq)

instance Show ObjId where
  show :: ObjId -> String
  show (ObjId oid) = oid

data ObjKind = ObjBlob | ObjCommit | ObjTree | ObjTag
  deriving (Show, Read)

objKindFromString :: String -> ObjKind
objKindFromString raw
  | kind == "commit" = ObjCommit
  | kind == "tree" = ObjTree
  | kind == "tag" = ObjTag
  | otherwise = ObjBlob
  where
    kind = map toLower raw

data Obj = Obj
  { kind :: ObjKind,
    len :: Int,
    content :: ByteString.ByteString
  }

type ObjRawContent = ByteString.ByteString

serializeObj :: Obj -> ByteString.ByteString
serializeObj obj = header <> obj.content
  where
    header :: ByteString.ByteString
    header = Char8.pack (show obj.kind <> " " <> show obj.len <> "\0")

deserializeObj :: ByteString.ByteString -> Maybe Obj
deserializeObj rawObj = do
  let (header, rest) = ByteString.break (== 0) rawObj

  content <-
    if ByteString.null rest
      then Nothing
      else Just $ ByteString.drop 1 rest

  let (rawKind, rawLength) = break (== ' ') (Char8.unpack header)

  kind <- readMaybe rawKind
  len <- readMaybe $ dropWhile (== ' ') rawLength

  Just Obj {kind = kind, len = len, content = content}

validateObjId :: ObjId -> Either String ObjId
validateObjId (ObjId oid)
  | length oid == 40 = Right $ ObjId oid
  | otherwise =
      Left $
        "Invalid object hash length: expected 40 characters, got " <> show (length oid)

makeObj :: ObjKind -> ObjRawContent -> (ObjId, Obj)
makeObj kind rawContent = (oid, obj)
  where
    obj :: Obj
    obj =
      Obj
        { kind = kind,
          len = ByteString.length rawContent,
          content = rawContent
        }

    oid :: ObjId
    oid = ObjId (obj & serializeObj & SHA1.hash & byteStringToHex)

writeObj :: WorkDir -> (ObjId, Obj) -> IO ()
writeObj workDir (ObjId oid, obj) =
  Directory.createDirectoryIfMissing True fileDir
    >> ByteString.writeFile filePath content
  where
    content = obj & serializeObj & compress
    fileDir = makeRepoDir workDir </> "objects" </> take 2 oid
    filePath = fileDir </> drop 2 oid

readObj :: WorkDir -> ObjId -> IO (Maybe Obj)
readObj workDir (ObjId oid) = do
  exists <- Directory.doesFileExist filePath

  if exists
    then do
      content <- ByteString.readFile filePath
      return $ deserializeObj $ decompress content
    else return Nothing
  where
    fileDir = makeRepoDir workDir </> "objects" </> take 2 oid
    filePath = fileDir </> drop 2 oid

data IdxEntry = IdxEntry
  { mode :: String,
    oid :: ObjId,
    path :: FilePath
  }
  deriving (Eq)

instance Ord IdxEntry where
  compare :: IdxEntry -> IdxEntry -> Ordering
  compare e1 e2 = compare e1.path e2.path

newtype Idx = Idx {entries :: [IdxEntry]}

defaultIdx :: Idx
defaultIdx = Idx {entries = []}

serializeIdx :: Idx -> ByteString.ByteString
serializeIdx idx =
  map serializeEntry idx.entries
    & ByteString.intercalate (Char8.pack "\n")
  where
    serializeEntry :: IdxEntry -> ByteString.ByteString
    serializeEntry entry =
      Char8.pack (entry.mode <> ";" <> show entry.oid <> ";" <> entry.path)

deserializeIdx :: ByteString.ByteString -> Maybe Idx
deserializeIdx rawObj = do
  entries <- mapM deserializeEntry (Char8.lines rawObj)
  Just Idx {entries = entries}
  where
    deserializeEntry :: ByteString.ByteString -> Maybe IdxEntry
    deserializeEntry line =
      case Char8.split ';' line of
        [rawMode, rawOid, rawPath] -> do
          oid <- readMaybe (Char8.unpack rawOid)
          Just
            IdxEntry
              { mode = Char8.unpack rawMode,
                oid = oid,
                path = Char8.unpack rawPath
              }
        _ -> Nothing

writeIdx :: WorkDir -> Idx -> IO ()
writeIdx workDir idx =
  idx
    & serializeIdx
    & compress
    & ByteString.writeFile (makeRepoDir workDir </> "index")

readIdx :: WorkDir -> IO Idx
readIdx workDir = do
  exists <- Directory.doesFileExist (makeRepoDir workDir </> "index")

  if exists
    then do
      compressed <- ByteString.readFile (makeRepoDir workDir </> "index")

      case deserializeIdx (decompress compressed) of
        Nothing -> return defaultIdx
        Just idx -> return idx
    else return defaultIdx

idxEntryExists :: Idx -> FilePath -> Bool
idxEntryExists idx path = any (\entry -> entry.path == path) idx.entries

insertIdxEntry :: Idx -> IdxEntry -> Idx
insertIdxEntry idx entry =
  idx {entries = entry : filtered}
  where
    filtered = filter ((/= entry.path) . (.path)) idx.entries

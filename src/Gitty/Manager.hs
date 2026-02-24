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
    TreeStruct (..),
    mountTreeStructFromIdx,
    deserializeTreeEntries,
    writeTreeObj,
    readTreeObj,
    resolveRef,
    readSymbolicRef,
    writeSymbolicRef,
    writeRefFile,
    deleteRefFile,
  )
where

import Control.Monad (guard, unless)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.Char (isHexDigit, toLower)
import Data.Either (partitionEithers)
import Data.Function ((&))
import qualified Data.Map as Map
import Gitty.Common (WorkDir, byteStringToHex, compress, decompress, hexToByteString, makeRepoDir)
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import Text.Read (readMaybe)

newtype ObjId = ObjId String
  deriving (Read, Eq)

instance Show ObjId where
  show :: ObjId -> String
  show (ObjId oid) = oid

data ObjKind = ObjBlob | ObjCommit | ObjTree | ObjTag
  deriving (Show, Read, Eq)

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
writeObj workDir (ObjId oid, obj) = do
  Directory.createDirectoryIfMissing True fileDir
  exists <- Directory.doesFileExist filePath
  unless exists $ ByteString.writeFile filePath content
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
        [rawMode, rawOid, rawPath] ->
          Just
            IdxEntry
              { mode = Char8.unpack rawMode,
                oid = ObjId (Char8.unpack rawOid),
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

data TreeStruct
  = TreeStruct FilePath [TreeStruct]
  | TreeEntry IdxEntry

mountTreeStructFromIdx :: Idx -> TreeStruct
mountTreeStructFromIdx idx = TreeStruct "." (mountTreeStructFromIdx' idx.entries)
  where
    mountTreeStructFromIdx' :: [IdxEntry] -> [TreeStruct]
    mountTreeStructFromIdx' entries = do
      let (files, dirs) = partitionEithers $ map classify entries

          groupedDirs =
            Map.fromListWith
              (++)
              [(dir, [entry]) | (dir, entry) <- dirs]

          dirNodes =
            [ TreeStruct
                dir
                (mountTreeStructFromIdx' es)
            | (dir, es) <- Map.toList groupedDirs
            ]

          fileNodes = map TreeEntry files

      fileNodes ++ dirNodes

    classify :: IdxEntry -> Either IdxEntry (FilePath, IdxEntry)
    classify entry = case FilePath.splitDirectories entry.path of
      [_] -> Left entry
      (dir : rest) -> Right (dir, entry {path = FilePath.joinPath rest})
      _ -> Left entry

serializeTreeEntry :: (String, FilePath, ObjId) -> ByteString.ByteString
serializeTreeEntry (mode, entryName, ObjId hex) =
  Char8.pack (mode <> " " <> entryName <> "\0") <> hexToByteString hex

deserializeTreeEntries :: ByteString.ByteString -> Maybe [(String, FilePath, ObjId)]
deserializeTreeEntries bs
  | ByteString.null bs = Just []
  | otherwise = do
      let (rawMode, restA) = Char8.break (== ' ') bs
      guard (not $ ByteString.null restA)

      let (rawName, restB) = ByteString.break (== 0) (ByteString.drop 1 restA)
      guard (not $ ByteString.null restB)

      let (rawOid, restC) = ByteString.splitAt 20 (ByteString.drop 1 restB)
      guard (ByteString.length rawOid == 20)

      remaining <- deserializeTreeEntries restC

      Just
        ( ( Char8.unpack rawMode,
            Char8.unpack rawName,
            ObjId (byteStringToHex rawOid)
          )
            : remaining
        )

makeTreeObj :: [(String, FilePath, ObjId)] -> (ObjId, Obj)
makeTreeObj entries = makeObj ObjTree content
  where
    content = ByteString.concat $ map serializeTreeEntry entries

writeTreeObj :: WorkDir -> TreeStruct -> IO ObjId
writeTreeObj _ (TreeEntry entry) = return entry.oid
writeTreeObj workDir (TreeStruct _ children) = do
  entries <- mapM (resolveChild workDir) children
  let treeObj = makeTreeObj entries
  writeObj workDir treeObj

  return $ fst treeObj
  where
    resolveChild :: WorkDir -> TreeStruct -> IO (String, FilePath, ObjId)
    resolveChild _ (TreeEntry entry) = return (entry.mode, entry.path, entry.oid)
    resolveChild wd tree@(TreeStruct dirName _) = do
      treeOid <- writeTreeObj wd tree
      return ("40000", dirName, treeOid)

readTreeObj :: WorkDir -> ObjId -> IO (Maybe TreeStruct)
readTreeObj workDir rootOid = do
  maybeObj <- readObj workDir rootOid

  case maybeObj >>= \obj -> deserializeTreeEntries obj.content of
    Nothing -> return Nothing
    Just entries -> do
      children <- mapM resolveEntry entries
      return $ Just $ TreeStruct "." children
  where
    resolveEntry :: (String, FilePath, ObjId) -> IO TreeStruct
    resolveEntry ("40000", entryName, entryOid) = do
      maybeObj <- readObj workDir entryOid

      case maybeObj >>= \obj -> deserializeTreeEntries obj.content of
        Nothing -> return $ TreeStruct entryName []
        Just subEntries -> do
          children <- mapM resolveEntry subEntries
          return $ TreeStruct entryName children
    resolveEntry (entryMode, entryName, entryOid) =
      return $ TreeEntry IdxEntry {mode = entryMode, oid = entryOid, path = entryName}

resolveRef :: WorkDir -> String -> IO (Maybe ObjId)
resolveRef workDir ref
  | length ref == 40 && all isHexDigit ref = return $ Just (ObjId ref)
  | ref == "HEAD" = resolveHead workDir
  | otherwise = readRefFile workDir ("refs/heads/" <> ref)

resolveHead :: WorkDir -> IO (Maybe ObjId)
resolveHead workDir = do
  let headPath = makeRepoDir workDir </> "HEAD"
  exists <- Directory.doesFileExist headPath

  if exists
    then do
      content <- readFile headPath
      readRefFile workDir (trim content)
    else return Nothing

readRefFile :: WorkDir -> FilePath -> IO (Maybe ObjId)
readRefFile workDir refPath = do
  let fullPath = makeRepoDir workDir </> refPath
  exists <- Directory.doesFileExist fullPath

  if exists
    then do
      content <- readFile fullPath
      case validateObjId (ObjId (trim content)) of
        Right oid -> return $ Just oid
        Left _ -> return Nothing
    else return Nothing

readSymbolicRef :: WorkDir -> FilePath -> IO (Maybe FilePath)
readSymbolicRef workDir name = do
  let fullPath = makeRepoDir workDir </> name
  exists <- Directory.doesFileExist fullPath

  if exists
    then do
      content <- readFile fullPath
      let ref = trim content
      return $ if null ref then Nothing else Just ref
    else return Nothing

writeSymbolicRef :: WorkDir -> FilePath -> FilePath -> IO ()
writeSymbolicRef workDir name =
  writeFile (makeRepoDir workDir </> name)

writeRefFile :: WorkDir -> FilePath -> ObjId -> IO ()
writeRefFile workDir refPath (ObjId oid) = do
  let fullPath = makeRepoDir workDir </> refPath
  Directory.createDirectoryIfMissing True (FilePath.takeDirectory fullPath)
  writeFile fullPath oid

deleteRefFile :: WorkDir -> FilePath -> IO Bool
deleteRefFile workDir refPath = do
  let fullPath = makeRepoDir workDir </> refPath
  exists <- Directory.doesFileExist fullPath

  if exists
    then do
      Directory.removeFile fullPath
      return True
    else return False

trim :: String -> String
trim = reverse . dropWhile (== '\n') . reverse . dropWhile (== '\n')
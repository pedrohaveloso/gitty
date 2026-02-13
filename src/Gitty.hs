{-# LANGUAGE TupleSections #-}

module Gitty (init, add) where

import Codec.Compression.Zlib (compress, decompress)
import Control.Exception (IOException, try)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import Data.Function ((&))
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Gitty.Object as Object
import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import Text.Printf (printf)
import Prelude hiding (init)

sha1 :: (Foldable t) => t BS.ByteString -> BS.ByteString
sha1 content = result
  where
    ctx = SHA1.init
    updatedCtx = foldl SHA1.update ctx content
    result = SHA1.finalize updatedCtx

gittyDir :: FilePath -> FilePath
gittyDir workDir = workDir ++ "/.gitty"

-- todo: move to object module
objectsDir :: FilePath -> FilePath
objectsDir = (++ "/objects") . gittyDir

init :: String -> IO String
init workDir = do
  exists <- Directory.doesDirectoryExist dir
  if exists then reinitialize else initialize
  where
    dir :: String
    dir = gittyDir workDir

    reinitialize :: IO String
    reinitialize = return "Reinitialized existing repository"

    initialize :: IO String
    initialize = do
      Directory.createDirectoryIfMissing True dir
      return "Initialized empty repository"

type FileHash = String

data Index = Index
  { indexFiles :: Map.Map FilePath FileHash
  }

writeIndex :: FilePath -> Index -> IO ()
writeIndex workDir (Index files) = do
  let indexPath = gittyDir workDir ++ "/index"
      lines = map (\(p, h) -> BS.Char8.pack $ p ++ "," ++ h) (Map.toList files)
      content = BS.Char8.intercalate (str2bs "\n") lines
      compressed = BS.toStrict $ compress $ BS.fromStrict content
  BS.writeFile indexPath compressed

readIndex :: FilePath -> IO Index
readIndex workDir = do
  let indexPath = gittyDir workDir ++ "/index"
  exists <- Directory.doesFileExist indexPath
  if not exists
    then return $ Index Map.empty
    else do
      compressed <- BS.readFile indexPath
      let content = BS.toStrict $ decompress $ BS.fromStrict compressed
          lines = filter (not . BS.null) $ BS.Char8.split '\n' content
          entries =
            map
              ( \line ->
                  let [p, h] = BS.Char8.split ',' line
                   in (BS.Char8.unpack p, BS.Char8.unpack h)
              )
              lines
      return $ Index $ Map.fromList entries

bs2hex :: BS.ByteString -> String
bs2hex = concatMap (printf "%02x") . BS.unpack

str2bs :: String -> BS.ByteString
str2bs = BS.Char8.pack

bs2str :: BS.ByteString -> String
bs2str = BS.Char8.unpack

-- hashFile =

makeAbsoluteFrom :: FilePath -> FilePath -> FilePath
makeAbsoluteFrom baseDir path
  | FilePath.isAbsolute path = path
  | otherwise = FilePath.normalise (baseDir </> path)

getNonExistentPaths :: [FilePath] -> IO [FilePath]
getNonExistentPaths paths = result
  where
    relation = mapM (\path -> (path,) <$> Directory.doesPathExist path) paths
    result = fmap (map (\(path, _) -> path) . filter (\(_, exists) -> not exists)) relation

add :: FilePath -> [FilePath] -> IO (Either String String)
add workDir paths = do
  let absolutePaths = map (makeAbsoluteFrom workDir) paths
  nonExistentPaths <- getNonExistentPaths absolutePaths

  if null nonExistentPaths
    then add' absolutePaths >>= return . Right
    else return $ Left $ nonExistentPathsError nonExistentPaths
  where
    nonExistentPathsError :: [FilePath] -> String
    nonExistentPathsError nonExistentPaths =
      "The following files do not exist in the root directory of the current repository:\n"
        ++ (intercalate "\n" $ map ("• " ++) nonExistentPaths)

    add' :: [FilePath] -> IO String
    add' [] = return ""
    add' (path : rest) = do
      added <- add' rest

      isFile <- Directory.doesFileExist path

      result <-
        if isFile
          then addFile path
          else addDir path

      return $ added ++ result

    addFile :: FilePath -> IO String
    addFile filePath = do
      originalContent <- BS.readFile filePath

      let contentLength = BS.length originalContent
          contentHeader = str2bs $ Object.blobType <> " " <> show contentLength

          content = contentHeader <> str2bs "\0" <> originalContent

          hashedContent = content & SHA1.hash & bs2hex
          compressedContent = content & BS.fromStrict & compress & BS.toStrict

          hashedPath = take 2 hashedContent <> "/" <> drop 2 hashedContent

          fileDir = objectsDir workDir <> "/" <> take 2 hashedContent
          fileName = fileDir <> "/" <> drop 2 hashedContent

      Directory.createDirectoryIfMissing True fileDir
      BS.writeFile fileName compressedContent

      index <- readIndex workDir
      let idxFiles = indexFiles index & Map.insert filePath hashedPath
      writeIndex workDir (Index {indexFiles = idxFiles})

      let relativeFilepath = FilePath.makeRelative workDir filePath

      return $ "File '" <> relativeFilepath <> "' added"

    addDir :: FilePath -> IO String
    addDir dirPath = undefined

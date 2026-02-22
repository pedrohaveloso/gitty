module Gitty.Common
  ( WorkDir,
    RepoDir,
    makeRepoDir,
    isInsideRepoDir,
    makeAbsoluteFrom,
    getFileMode,
    getRecursiveFiles,
    compress,
    decompress,
    byteStringToHex,
    needRepo,
  )
where

import qualified Codec.Compression.Zlib as Zlib
import Control.Monad (forM)
import Data.Bits ((.&.))
import qualified Data.ByteString as ByteString
import Data.Function ((&))
import Data.List (isPrefixOf)
import System.Directory (doesDirectoryExist, listDirectory)
import qualified System.Directory as Directory
import System.FilePath (isAbsolute, normalise, (</>))
import System.Posix (fileMode, getFileStatus)
import Text.Printf (printf)

type WorkDir = FilePath

type RepoDir = FilePath

makeRepoDir :: WorkDir -> RepoDir
makeRepoDir workDir = workDir </> ".gitty"

makeAbsoluteFrom :: FilePath -> FilePath -> FilePath
makeAbsoluteFrom baseDir path
  | isAbsolute path = path
  | otherwise = normalise (baseDir </> path)

isInsideRepoDir :: WorkDir -> FilePath -> Bool
isInsideRepoDir workDir path =
  absolute == path
    || repoDir `isPrefixOf` absolute
  where
    repoDir = makeRepoDir workDir
    absolute = makeAbsoluteFrom workDir path

getFileMode :: FilePath -> IO String
getFileMode filePath = do
  status <- getFileStatus filePath
  let rawMode = fileMode status
      isExecutable = rawMode .&. 0o111 /= 0
  return $ if isExecutable then "100755" else "100644"

getRecursiveFiles :: FilePath -> IO [FilePath]
getRecursiveFiles topDir = do
  names <- listDirectory topDir
  let properNames = filter (`notElem` [".", "..", ".gitty"]) names

  paths <- forM properNames $ \name -> do
    let path = topDir </> name
    doesDir <- doesDirectoryExist path

    if doesDir
      then getRecursiveFiles path
      else return [path]

  return (concat paths)

compress :: ByteString.ByteString -> ByteString.ByteString
compress bs = bs & ByteString.fromStrict & Zlib.compress & ByteString.toStrict

decompress :: ByteString.ByteString -> ByteString.ByteString
decompress bs = bs & ByteString.fromStrict & Zlib.decompress & ByteString.toStrict

byteStringToHex :: ByteString.ByteString -> String
byteStringToHex = concatMap (printf "%02x") . ByteString.unpack

needRepo :: WorkDir -> IO () -> IO ()
needRepo workDir fn = do
  exists <- repoExists
  maybe fn putStrLn exists
  where
    repoExists :: IO (Maybe String)
    repoExists = do
      let repoDir = makeRepoDir workDir

      exists <- Directory.doesDirectoryExist repoDir

      if exists
        then return Nothing
        else return $ Just $ "There is no repository in that folder (" <> workDir <> ")."

module Gitty.FileSystem (getFileMode, getRecursiveFiles) where

import Control.Monad (forM)
import Data.Bits ((.&.))
import qualified System.Directory as Directory
import System.FilePath ((</>))
import System.Posix.Files (fileMode, getFileStatus)

getFileMode :: FilePath -> IO String
getFileMode filePath = do
  status <- getFileStatus filePath
  let rawMode = fileMode status
      isExecutable = rawMode .&. 0o111 /= 0
  return $ if isExecutable then "100755" else "100644"

getRecursiveFiles :: FilePath -> IO [FilePath]
getRecursiveFiles topDir = do
  names <- Directory.listDirectory topDir
  let properNames = filter (`notElem` [".", "..", ".gitty"]) names

  paths <- forM properNames $ \name -> do
    let path = topDir </> name
    doesDir <- Directory.doesDirectoryExist path

    if doesDir
      then getRecursiveFiles path
      else return [path]

  return (concat paths)

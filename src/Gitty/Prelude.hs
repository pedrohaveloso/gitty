module Gitty.Prelude
  ( WorkDir,
    bsToHex,
    repoDir,
    makeAbsoluteFrom,
    isInsideGittyDir,
  )
where

import qualified Data.ByteString as ByteString
import Data.List (isPrefixOf)
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import Text.Printf (printf)

type WorkDir = FilePath

bsToHex :: ByteString.ByteString -> String
bsToHex = concatMap (printf "%02x") . ByteString.unpack

repoDir :: WorkDir -> FilePath
repoDir workDir = workDir ++ "/.gitty"

makeAbsoluteFrom :: FilePath -> FilePath -> FilePath
makeAbsoluteFrom baseDir path
  | FilePath.isAbsolute path = path
  | otherwise = FilePath.normalise (baseDir </> path)

isInsideGittyDir :: WorkDir -> FilePath -> Bool
isInsideGittyDir workDir path =
  let absPath = makeAbsoluteFrom workDir path
      gittyPath = repoDir workDir
   in absPath == gittyPath || (gittyPath ++ "/") `isPrefixOf` absPath
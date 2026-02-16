module Gitty.Prelude
  ( WorkDir,
    bsToHex,
    repoDir,
    makeAbsoluteFrom,
  )
where

import qualified Data.ByteString as ByteString
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import Text.Printf (printf)

type WorkDir = FilePath

bsToHex :: ByteString.ByteString -> String
bsToHex = concatMap (printf "%02x") . ByteString.unpack

repoDir :: FilePath -> FilePath
repoDir workDir = workDir ++ "/.gitty"

makeAbsoluteFrom :: FilePath -> FilePath -> FilePath
makeAbsoluteFrom baseDir path
  | FilePath.isAbsolute path = path
  | otherwise = FilePath.normalise (baseDir </> path)
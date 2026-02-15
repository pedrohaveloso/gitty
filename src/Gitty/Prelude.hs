module Gitty.Prelude
  ( WorkDir,
    bsToHex,
    repoDir,
  )
where

import qualified Data.ByteString as ByteString
import Text.Printf (printf)

type WorkDir = FilePath

bsToHex :: ByteString.ByteString -> String
bsToHex = concatMap (printf "%02x") . ByteString.unpack

repoDir :: FilePath -> FilePath
repoDir workDir = workDir ++ "/.gitty"
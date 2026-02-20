module Gitty.Common
  ( WorkDir (..),
    RepoDir (..),
    makeRepoDir,
    isInsideRepoDir,
    makeAbsoluteFrom,
  )
where

import Data.List (isPrefixOf)
import System.FilePath (isAbsolute, normalise, (</>))

newtype WorkDir = WorkDir FilePath

newtype RepoDir = RepoDir FilePath

makeRepoDir :: WorkDir -> RepoDir
makeRepoDir (WorkDir workDir) = RepoDir $ workDir </> ".gitty"

makeAbsoluteFrom :: FilePath -> FilePath -> FilePath
makeAbsoluteFrom baseDir path
  | isAbsolute path = path
  | otherwise = normalise (baseDir </> path)

isInsideRepoDir :: WorkDir -> FilePath -> Bool
isInsideRepoDir (WorkDir workDir) path =
  absolute == path
    || (repoDir </> "/") `isPrefixOf` absolute
  where
    (RepoDir repoDir) = makeRepoDir (WorkDir workDir)
    absolute = makeAbsoluteFrom workDir path
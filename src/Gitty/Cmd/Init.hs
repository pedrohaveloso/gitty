module Gitty.Cmd.Init (cmdInit, definition) where

import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common (WorkDir, makeRepoDir)
import qualified System.Directory as Directory
import System.FilePath ((</>))

cmdInit :: WorkDir -> () -> IO ()
cmdInit workDir _ = do
  exists <- Directory.doesFileExist repoDir

  if exists
    then return ()
    else do
      Directory.createDirectory repoDir
      Directory.createDirectoryIfMissing True $ repoDir </> "refs/heads"
      writeFile (repoDir </> "HEAD") "refs/heads/main"
  where
    repoDir = makeRepoDir workDir

definition :: CmdDefinition ()
definition =
  CmdDefinition
    { name = "init",
      description = "Create an empty repository",
      parser = pure ()
    }

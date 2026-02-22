module Gitty.Cmd.Init (cmdInit, definition) where

import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common (WorkDir, makeRepoDir)
import qualified System.Directory as Directory

cmdInit :: WorkDir -> () -> IO ()
cmdInit workDir _ = do
  exists <- Directory.doesFileExist repoDir

  if exists
    then return ()
    else Directory.createDirectory repoDir
  where
    repoDir = makeRepoDir workDir

definition :: CmdDefinition ()
definition =
  CmdDefinition
    { name = "init",
      description = "Create an empty Git repository",
      parser = pure ()
    }

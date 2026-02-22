module Gitty.Cmd.WriteTree (cmdWriteTree, definition) where

import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common (WorkDir, needRepo)
import qualified Gitty.Manager as Manager

cmdWriteTree :: WorkDir -> () -> IO ()
cmdWriteTree workDir _ = needRepo workDir writeTree
  where
    writeTree :: IO ()
    writeTree = do
      idx <- Manager.readIdx workDir
      let tree = Manager.mountTreeStructFromIdx idx
      rootOid <- Manager.writeTreeObj workDir tree
      print rootOid

definition :: CmdDefinition ()
definition =
  CmdDefinition
    { name = "write-tree",
      description = "Create a tree object from the current index",
      parser = pure ()
    }

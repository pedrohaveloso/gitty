{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Cmd.HashObject (cmdHashObject, definition) where

import Control.Monad (when)
import qualified Data.ByteString as ByteString
import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common (WorkDir, isInsideRepoDir, makeAbsoluteFrom)
import qualified Gitty.Manager as Manager
import qualified Gitty.Output as Output
import qualified Gitty.Validation as Validation
import qualified Options.Applicative as Cli

data Options = Options
  { write :: Bool,
    kind :: String,
    file :: FilePath
  }
  deriving (Show)

cmdHashObject :: WorkDir -> Options -> IO ()
cmdHashObject workDir opts
  | isInsideRepoDir workDir opts.file = return ()
  | otherwise = do
      fileError <- Validation.fileAccess workDir opts.file

      case fileError of
        Just err -> Output.echo err
        Nothing -> do
          fileContent <- ByteString.readFile file

          let (oid, object) = Manager.makeObject kind fileContent

          when opts.write $ Manager.writeObject workDir (oid, object)

          Output.echo (show oid)

          return ()
  where
    kind :: Manager.ObjectKind
    kind = Manager.objectKindFromString opts.kind

    file :: FilePath
    file = makeAbsoluteFrom workDir opts.file

definition :: CmdDefinition Options
definition =
  CmdDefinition
    { name = "hash-object",
      description = "Compute object ID and optionally creates a blob from a file",
      parser =
        Options
          <$> Cli.switch
            ( Cli.long "write"
                <> Cli.short 'w'
                <> Cli.help "Actually write the object into the database"
            )
          <*> Cli.strOption
            ( Cli.long "kind"
                <> Cli.short 'k'
                <> Cli.value "blob"
                <> Cli.showDefault
                <> Cli.metavar "KIND"
                <> Cli.help "Specify the kind of object to be created (default: 'blob'). Possible values are commit, tree, blob, and tag."
            )
          <*> Cli.argument
            Cli.str
            ( Cli.metavar "FILE"
                <> Cli.help "File to hash"
            )
    }

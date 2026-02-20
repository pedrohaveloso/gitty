{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Cmd.HashObject (cmdHashObject, Options (..), definition) where

import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common (WorkDir (..), isInsideRepoDir)
import Gitty.Output (echo)
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
      result <- Object.hashFile workDir opts.write kind opts.file

      case result of
        Left err -> echo err
        Right hash -> echo hash
  where
    kind = Object.kindFromString opts.kind

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

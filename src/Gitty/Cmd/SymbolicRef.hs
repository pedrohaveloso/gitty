{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Cmd.SymbolicRef (cmdSymbolicRef, definition) where

import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common (WorkDir, needRepo)
import qualified Gitty.Manager as Manager
import qualified Options.Applicative as Cli

data Options = Options
  { refName :: String,
    target :: Maybe String
  }

cmdSymbolicRef :: WorkDir -> Options -> IO ()
cmdSymbolicRef workDir opts = needRepo workDir $ case opts.target of
  Just ref -> Manager.writeSymbolicRef workDir opts.refName ref
  Nothing -> do
    result <- Manager.readSymbolicRef workDir opts.refName
    case result of
      Just ref -> putStrLn ref
      Nothing -> putStrLn $ "fatal: ref " <> opts.refName <> " is not a symbolic ref"

definition :: CmdDefinition Options
definition =
  CmdDefinition
    { name = "symbolic-ref",
      description = "Read or modify symbolic refs",
      parser =
        Options
          <$> Cli.argument Cli.str (Cli.metavar "NAME" <> Cli.help "Symbolic ref name (e.g. HEAD)")
          <*> Cli.optional (Cli.argument Cli.str (Cli.metavar "REF" <> Cli.help "Ref to point to (e.g. refs/heads/main)"))
    }

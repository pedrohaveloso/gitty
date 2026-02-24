{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Cmd.UpdateRef (cmdUpdateRef, definition) where

import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common (WorkDir, needRepo)
import qualified Gitty.Manager as Manager
import qualified Options.Applicative as Cli

data Options = Options
  { delete :: Bool,
    ref :: String,
    newValue :: Maybe String
  }

cmdUpdateRef :: WorkDir -> Options -> IO ()
cmdUpdateRef workDir opts = needRepo workDir $ case (opts.delete, opts.newValue) of
  (True, _) -> do
    deleted <- Manager.deleteRefFile workDir opts.ref
    if deleted
      then return ()
      else putStrLn $ "fatal: could not delete ref " <> opts.ref
  (False, Just newValue) -> case Manager.validateObjId (Manager.ObjId newValue) of
    Left err -> putStrLn err
    Right oid -> do
      exists <- Manager.readObj workDir oid
      case exists of
        Nothing -> putStrLn $ "fatal: not a valid object name " <> newValue
        Just _ -> Manager.writeRefFile workDir opts.ref oid
  (False, Nothing) ->
    putStrLn "fatal: missing new value for update-ref"

definition :: CmdDefinition Options
definition =
  CmdDefinition
    { name = "update-ref",
      description = "Update the object name stored in a ref",
      parser =
        Options
          <$> Cli.switch (Cli.short 'd' <> Cli.help "Delete the ref")
          <*> Cli.argument Cli.str (Cli.metavar "REF" <> Cli.help "Ref to update (e.g. refs/heads/main)")
          <*> Cli.optional (Cli.argument Cli.str (Cli.metavar "NEWVALUE" <> Cli.help "New object ID"))
    }

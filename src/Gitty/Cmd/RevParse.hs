module Gitty.Cmd.RevParse (cmdRevParse, definition) where

import Control.Applicative ((<|>))
import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common (WorkDir, makeRelativeTo, makeRepoDir, needRepo)
import qualified Gitty.Manager as Manager
import qualified Options.Applicative as Cli
import qualified System.Directory as Directory
import System.Exit (exitFailure)

data Mode
  = Resolve String
  | Verify Bool String
  | ShowGittyDir
  | ShowTopLevel
  | IsInsideWorkTree
  | ShowPrefix

cmdRevParse :: WorkDir -> Mode -> IO ()
cmdRevParse workDir mode = case mode of
  IsInsideWorkTree -> do
    exists <- Directory.doesDirectoryExist (makeRepoDir workDir)
    putStrLn $ if exists then "true" else "false"
  ShowGittyDir -> needRepo workDir $ putStrLn (makeRepoDir workDir)
  ShowTopLevel -> needRepo workDir $ putStrLn workDir
  ShowPrefix -> needRepo workDir $ do
    cwd <- Directory.getCurrentDirectory
    putStrLn $ makeRelativeTo workDir cwd
  Resolve ref -> needRepo workDir $ resolve ref
  Verify quiet ref -> needRepo workDir $ verify quiet ref
  where
    resolve :: String -> IO ()
    resolve ref = do
      result <- Manager.resolveRef workDir ref
      case result of
        Just oid -> print oid
        Nothing -> putStrLn $ ref <> ": unknown revision or path"

    verify :: Bool -> String -> IO ()
    verify quiet ref = do
      result <- Manager.resolveRef workDir ref
      case result of
        Nothing -> verifyFail quiet
        Just oid -> do
          obj <- Manager.readObj workDir oid
          case obj of
            Nothing -> verifyFail quiet
            Just _ -> print oid

    verifyFail :: Bool -> IO ()
    verifyFail quiet =
      if quiet
        then exitFailure
        else do
          putStrLn "fatal: Needed a single revision"
          exitFailure

definition :: CmdDefinition Mode
definition =
  CmdDefinition
    { name = "rev-parse",
      description = "Pick out and massage parameters",
      parser =
        Cli.flag' ShowGittyDir (Cli.long "gitty-dir" <> Cli.help "Show the .gitty directory path")
          <|> Cli.flag' ShowTopLevel (Cli.long "show-toplevel" <> Cli.help "Show the working tree root directory")
          <|> Cli.flag' IsInsideWorkTree (Cli.long "is-inside-work-tree" <> Cli.help "Check if the current directory is inside a work tree")
          <|> Cli.flag' ShowPrefix (Cli.long "show-prefix" <> Cli.help "Show the path prefix relative to the top-level directory")
          <|> Verify
            <$> Cli.switch (Cli.long "quiet" <> Cli.short 'q' <> Cli.help "Do not output error messages (only meaningful with --verify)")
            <*> Cli.strOption (Cli.long "verify" <> Cli.metavar "REF" <> Cli.help "Verify that a ref exists and resolve it")
          <|> Resolve <$> Cli.argument Cli.str (Cli.metavar "REF" <> Cli.help "Resolve a ref to its object ID")
    }

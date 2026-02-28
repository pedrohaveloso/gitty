{-# LANGUAGE OverloadedRecordDot #-}

module Gitty.Cmd.Log (cmdLog, definition) where

import Data.Foldable (forM_)
import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common (WorkDir, die, needRepo)
import qualified Gitty.Manager as Manager
import qualified Options.Applicative as Cli

cmdLog :: WorkDir -> () -> IO ()
cmdLog workDir _ = needRepo workDir logCmd
  where
    logCmd :: IO ()
    logCmd = do
      headOid <- Manager.resolveRef workDir "HEAD"
      case headOid of
        Nothing -> die "fatal: your current branch does not have any commits yet"
        Just oid -> walkCommits oid

    walkCommits :: Manager.ObjId -> IO ()
    walkCommits oid = do
      maybeObj <- Manager.readObj workDir oid
      case maybeObj of
        Nothing -> return ()
        Just obj -> case Manager.parseCommitContent obj.content of
          Nothing -> return ()
          Just info -> do
            putStrLn $ "commit " <> show oid
            putStrLn $ "Author: " <> info.author
            putStrLn ""
            putStrLn $ "    " <> info.message
            putStrLn ""

            forM_ info.parent walkCommits

definition :: CmdDefinition ()
definition =
  CmdDefinition
    { name = "log",
      description = "Show commit logs",
      parser = Cli.pure ()
    }

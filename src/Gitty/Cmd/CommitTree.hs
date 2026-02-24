{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Cmd.CommitTree (cmdCommitTree, definition) where

import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common (WorkDir, die, needRepo)
import qualified Gitty.Manager as Manager
import qualified Options.Applicative as Cli

data Options = Options
  { tree :: String,
    parent :: Maybe String,
    message :: Maybe String
  }

cmdCommitTree :: WorkDir -> Options -> IO ()
cmdCommitTree workDir opts = needRepo workDir commitTree
  where
    commitTree :: IO ()
    commitTree = do
      treeOid <- validateTree
      case treeOid of
        Left err -> die err
        Right treeOid' -> do
          parentOid <- validateParent
          case parentOid of
            Left err -> die err
            Right parentOid' -> do
              msg <- maybe getContents return opts.message
              author <- Manager.getAuthorInfo
              timestamp <- Manager.getTimestamp

              let (oid, obj) = Manager.makeCommitObj treeOid' parentOid' author timestamp msg
              Manager.writeObj workDir (oid, obj)
              print oid

    validateTree :: IO (Either String Manager.ObjId)
    validateTree = case Manager.validateObjId (Manager.ObjId opts.tree) of
      Left err -> return $ Left err
      Right oid -> do
        maybeObj <- Manager.readObj workDir oid
        return $ case maybeObj of
          Nothing -> Left $ "fatal: not a valid object name " <> opts.tree
          Just obj
            | obj.kind /= Manager.ObjTree -> Left "fatal: not a tree object"
            | otherwise -> Right oid

    validateParent :: IO (Either String (Maybe Manager.ObjId))
    validateParent = case opts.parent of
      Nothing -> return $ Right Nothing
      Just p -> case Manager.validateObjId (Manager.ObjId p) of
        Left err -> return $ Left err
        Right oid -> do
          exists <- Manager.readObj workDir oid
          return $ case exists of
            Nothing -> Left $ "fatal: not a valid object name " <> p
            Just _ -> Right $ Just oid

definition :: CmdDefinition Options
definition =
  CmdDefinition
    { name = "commit-tree",
      description = "Create a new commit object",
      parser =
        Options
          <$> Cli.argument Cli.str (Cli.metavar "TREE" <> Cli.help "Tree object ID")
          <*> Cli.optional
            ( Cli.strOption
                ( Cli.short 'p'
                    <> Cli.long "parent"
                    <> Cli.metavar "PARENT"
                    <> Cli.help "Parent commit object ID"
                )
            )
          <*> Cli.optional
            ( Cli.strOption
                ( Cli.short 'm'
                    <> Cli.long "message"
                    <> Cli.metavar "MESSAGE"
                    <> Cli.help "Commit message (reads from stdin if not provided)"
                )
            )
    }

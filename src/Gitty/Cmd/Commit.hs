{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Cmd.Commit (cmdCommit, definition) where

import qualified Data.ByteString.Char8 as Char8
import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common (WorkDir, needRepo)
import qualified Gitty.Manager as Manager
import qualified Options.Applicative as Cli

newtype Options = Options
  { message :: Maybe String
  }

cmdCommit :: WorkDir -> Options -> IO ()
cmdCommit workDir opts = needRepo workDir commit
  where
    commit :: IO ()
    commit = do
      idx <- Manager.readIdx workDir

      if null idx.entries
        then putStrLn "nothing to commit"
        else do
          let tree = Manager.mountTreeStructFromIdx idx
          treeOid <- Manager.writeTreeObj workDir tree
          parentOid <- Manager.resolveRef workDir "HEAD"
          headTreeOid <- getHeadTreeOid parentOid

          if headTreeOid == Just treeOid
            then putStrLn "Nothing to commit, working tree clean"
            else do
              msg <- maybe getContents return opts.message
              author <- Manager.getAuthorInfo
              timestamp <- Manager.getTimestamp

              let (commitOid, commitObj) = Manager.makeCommitObj treeOid parentOid author timestamp msg
              Manager.writeObj workDir (commitOid, commitObj)

              headRef <- Manager.readSymbolicRef workDir "HEAD"
              case headRef of
                Just ref -> Manager.writeRefFile workDir ref commitOid
                Nothing -> return ()

              putStrLn $ "[" <> branchName headRef <> " " <> shortOid commitOid <> "] " <> firstLine msg

    getHeadTreeOid :: Maybe Manager.ObjId -> IO (Maybe Manager.ObjId)
    getHeadTreeOid Nothing = return Nothing
    getHeadTreeOid (Just pid) = do
      maybeObj <- Manager.readObj workDir pid
      return $ case maybeObj of
        Nothing -> Nothing
        Just obj ->
          let header = takeWhile (/= '\n') (Char8.unpack obj.content)
           in case words header of
                ["tree", oid] -> Just (Manager.ObjId oid)
                _ -> Nothing

    branchName :: Maybe FilePath -> String
    branchName (Just ref) = drop (length ("refs/heads/" :: String)) ref
    branchName Nothing = "detached"

    shortOid :: Manager.ObjId -> String
    shortOid (Manager.ObjId oid) = take 7 oid

    firstLine :: String -> String
    firstLine = takeWhile (/= '\n')

definition :: CmdDefinition Options
definition =
  CmdDefinition
    { name = "commit",
      description = "Record changes to the repository",
      parser =
        Options
          <$> Cli.optional
            ( Cli.strOption
                ( Cli.short 'm'
                    <> Cli.long "message"
                    <> Cli.metavar "MESSAGE"
                    <> Cli.help "Commit message (reads from stdin if not provided)"
                )
            )
    }

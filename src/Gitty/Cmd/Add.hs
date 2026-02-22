{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Cmd.Add (cmdAdd, definition) where

import qualified Data.ByteString as ByteString
import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common
  ( WorkDir,
    getFileMode,
    getRecursiveFiles,
    isInsideRepoDir,
    makeAbsoluteFrom,
    makeRelativeTo,
    needRepo,
  )
import qualified Gitty.Manager as Manager
import qualified Options.Applicative as Cli
import qualified System.Directory as Directory

newtype Options
  = Options {paths :: [FilePath]}
  deriving (Show)

cmdAdd :: WorkDir -> Options -> IO ()
cmdAdd workDir opts = needRepo workDir (mapM_ add opts.paths)
  where
    add :: FilePath -> IO ()
    add path
      | isInsideRepoDir workDir path = return ()
      | otherwise =
          Directory.doesDirectoryExist path >>= \case
            True -> do
              files <- getRecursiveFiles path
              mapM_ addSingle files
            _ -> addSingle path

    addSingle :: FilePath -> IO ()
    addSingle file = do
      let absFile = makeAbsoluteFrom workDir file

      mode <- getFileMode absFile
      content <- ByteString.readFile absFile

      let (oid, obj) = Manager.makeObj Manager.ObjBlob content
      Manager.writeObj workDir (oid, obj)

      idx <- Manager.readIdx workDir

      Manager.writeIdx workDir $
        Manager.insertIdxEntry
          idx
          Manager.IdxEntry
            { mode = mode,
              oid = oid,
              path = makeRelativeTo workDir absFile
            }

definition :: CmdDefinition Options
definition =
  CmdDefinition
    { name = "add",
      description = "Add file contents to the index",
      parser =
        Options
          <$> Cli.some
            ( Cli.argument
                Cli.str
                ( Cli.metavar "FILES/DIRS"
                    <> Cli.help "Files and/or directories to add"
                )
            )
    }

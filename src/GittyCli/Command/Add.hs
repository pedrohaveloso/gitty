{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module GittyCli.Command.Add (Options, parser, run) where

import qualified Gitty
import qualified Gitty.FileSystem as FileSystem
import Gitty.Index (UpdateIndexOptions (..))
import qualified Gitty.Index as Index
import qualified Gitty.Object as Object
import Gitty.Prelude (WorkDir, isInsideGittyDir)
import Options.Applicative
  ( Parser,
    argument,
    help,
    many,
    metavar,
    str,
  )
import qualified System.Directory as Directory

newtype Options
  = Options {paths :: [FilePath]}
  deriving (Show)

parser :: Parser Options
parser =
  Options
    <$> many
      ( argument
          str
          ( metavar "FILES/DIRS"
              <> help "Files and/or directories to add"
          )
      )

runSingle :: WorkDir -> FilePath -> IO ()
runSingle workDir path = do
  hashEither <- Object.hashFile workDir True Object.Blob path
  mode <- FileSystem.getFileMode path

  case hashEither of
    Left err -> Gitty.fatal err
    Right hash -> do
      result <-
        Index.updateIndex
          workDir
          Index.UpdateIndexOptions
            { object = hash,
              add = True,
              mode = mode,
              file = path
            }

      case result of
        Left err -> Gitty.fatal err
        Right _ -> return ()

run :: WorkDir -> Options -> IO ()
run workDir options = do
  mapM_ run' options.paths
  where
    run' :: FilePath -> IO ()
    run' path
      | isInsideGittyDir workDir path = return ()
      | otherwise =
          Directory.doesDirectoryExist path >>= \case
            True -> do
              files <- FileSystem.getRecursiveFiles path
              mapM_ (runSingle workDir) files
            _ -> runSingle workDir path

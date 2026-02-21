{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Cmd.UpdateIndex (cmdUpdateIndex, Options (..), definition) where

import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common (WorkDir, isInsideRepoDir)
import qualified Gitty.Manager as Manager
import qualified Gitty.Output as Output
import qualified Options.Applicative as Cli

data CacheInfo = CacheInfo
  { mode :: String,
    object :: String,
    file :: FilePath
  }
  deriving (Show)

data Options = Options
  { add :: Bool,
    cacheInfos :: [CacheInfo],
    files :: [FilePath]
  }
  deriving (Show)

cmdUpdateIndex :: WorkDir -> Options -> IO ()
cmdUpdateIndex workDir opts =
  if null opts.cacheInfos
    then
      mapM_
        ( \file ->
            runSingle
              Index.UpdateIndexOptions
                { add = opts.add,
                  file = file,
                  mode = "",
                  object = ""
                }
        )
        opts.files
    else
      mapM_
        ( \cacheInfo ->
            runSingle
              Index.UpdateIndexOptions
                { add = opts.add,
                  file = cacheInfo.file,
                  mode = cacheInfo.mode,
                  object = cacheInfo.object
                }
        )
        opts.cacheInfos
  where
    runSingle :: Index.UpdateIndexOptions -> IO ()
    runSingle opts
      | isInsideGittyDir workDir opts.file = return ()
      | otherwise = do
          mode <- makeMode opts
          hashEither <- makeHash opts

          case hashEither of
            Left err -> Gitty.fatal err
            Right hash ->
              Index.updateIndex workDir (opts {Index.object = hash, Index.mode = mode})
                >>= \case
                  Left err -> Gitty.msg err
                  Right _ -> return ()

    makeMode :: Index.UpdateIndexOptions -> IO String
    makeMode opts =
      if null opts.mode
        then FileSystem.getFileMode opts.file
        else return opts.mode

    makeHash :: Index.UpdateIndexOptions -> IO (Either String Object.Hash)
    makeHash opts =
      if null opts.object
        then Object.hashFile workDir True Object.Blob opts.file
        else return $ Object.validateHash opts.object

definition :: CmdDefinition Options
definition =
  CmdDefinition
    { name = "update-index",
      description = "Register file contents in the working tree to the index",
      parser =
        resolve
          <$> Cli.switch
            ( Cli.long "add"
                <> Cli.help "If a specified file isn't in the index already then it's added"
            )
          <*> Cli.many
            ( Cli.strOption
                ( Cli.long "cacheinfo"
                    <> Cli.metavar "MODE,OBJECT,PATH"
                    <> Cli.help "Directly insert the specified info into the index"
                )
            )
          <*> Cli.many
            ( Cli.argument
                Cli.str
                (Cli.metavar "FILES...")
            )
    }

resolve :: Bool -> [String] -> [FilePath] -> Options
resolve add rawCacheInfos positionalArgs =
  let (cacheInfos, files) = resolveCacheInfos rawCacheInfos positionalArgs
   in Options {add = add, cacheInfos = cacheInfos, files = files}

resolveCacheInfos :: [String] -> [FilePath] -> ([CacheInfo], [FilePath])
resolveCacheInfos [] files = ([], files)
resolveCacheInfos (raw : rest) files =
  case parseCacheInfoComma raw of
    Just ci ->
      let (more, remaining) = resolveCacheInfos rest files
       in (ci : more, remaining)
    Nothing -> case files of
      (obj : path : filesRest) ->
        let ci = CacheInfo {mode = raw, object = obj, file = path}
            (more, remaining) = resolveCacheInfos rest filesRest
         in (ci : more, remaining)
      _ -> resolveCacheInfos rest files

parseCacheInfoComma :: String -> Maybe CacheInfo
parseCacheInfoComma s =
  case break (== ',') s of
    (mode, ',' : rest) -> case break (== ',') rest of
      (obj, ',' : path)
        | not (null mode) && not (null obj) && not (null path) ->
            Just (CacheInfo {mode = mode, object = obj, file = path})
      _ -> Nothing
    _ -> Nothing
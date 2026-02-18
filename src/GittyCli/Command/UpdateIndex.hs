{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module GittyCli.Command.UpdateIndex (Options (..), parser, run) where

import qualified Gitty
import qualified Gitty.FileSystem as FileSystem
import qualified Gitty.Index as Index
import qualified Gitty.Object as Object
import Gitty.Prelude (WorkDir, isInsideGittyDir)
import Options.Applicative
  ( Parser,
    argument,
    help,
    long,
    many,
    metavar,
    str,
    strOption,
    switch,
  )

data CacheInfo = CacheInfo
  { mode :: String,
    object :: Object.Hash,
    file :: FilePath
  }
  deriving (Show)

data Options = Options
  { add :: Bool,
    cacheInfos :: [CacheInfo],
    files :: [FilePath]
  }
  deriving (Show)

parser :: Parser Options
parser =
  resolve
    <$> switch
      ( long "add"
          <> help "If a specified file isn't in the index already then it's added"
      )
    <*> many
      ( strOption
          ( long "cacheinfo"
              <> metavar "MODE,OBJECT,PATH"
              <> help "Directly insert the specified info into the index"
          )
      )
    <*> many
      ( argument
          str
          (metavar "FILES...")
      )

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

run :: WorkDir -> Options -> IO ()
run workDir options = do
  if null options.cacheInfos
    then
      mapM_
        ( \file ->
            runSingle
              Index.UpdateIndexOptions
                { add = options.add,
                  file = file,
                  mode = "",
                  object = ""
                }
        )
        options.files
    else
      mapM_
        ( \cacheInfo ->
            runSingle
              Index.UpdateIndexOptions
                { add = options.add,
                  file = cacheInfo.file,
                  mode = cacheInfo.mode,
                  object = cacheInfo.object
                }
        )
        options.cacheInfos
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

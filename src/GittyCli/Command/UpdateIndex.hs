{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module GittyCli.Command.UpdateIndex (Options (..), parser, run) where

import Gitty.Index (UpdateIndexOptions (..))
import qualified Gitty.Index as Index
import qualified Gitty.Object as Object
import Gitty.Prelude (WorkDir)
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
            do
              y <-
                Index.updateIndex
                  workDir
                  Index.defaultUpdateIndexOptions {add = options.add, file = file}
              case y of
                Left err -> print err
                Right _ -> return ()
        )
        options.files
    else
      mapM_
        ( \cacheInfo -> do
            y <-
              Index.updateIndex
                workDir
                Index.defaultUpdateIndexOptions
                  { add = options.add,
                    file = cacheInfo.file,
                    mode = Just cacheInfo.mode,
                    object = Just cacheInfo.object
                  }

            case y of
              Left err -> print err
              Right _ -> return ()
        )
        options.cacheInfos

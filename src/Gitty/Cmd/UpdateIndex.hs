{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Cmd.UpdateIndex (cmdUpdateIndex, Options (..), definition) where

import qualified Data.ByteString as ByteString
import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common (WorkDir, getFileMode, isInsideRepoDir, makeAbsoluteFrom)
import qualified Gitty.Manager as Manager
import qualified Gitty.Output as Output
import qualified Gitty.Validation as Validation
import qualified Options.Applicative as Cli

data CacheInfo = CacheInfo
  { mode :: String,
    oid :: String,
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
    then mapM_ (\file -> runSingle file Nothing Nothing) opts.files
    else mapM_ (\ci -> runSingle ci.file (Just ci.mode) (Just ci.oid)) opts.cacheInfos
  where
    runSingle :: FilePath -> Maybe String -> Maybe String -> IO ()
    runSingle file mode oid
      | isInsideRepoDir workDir file = return ()
      | otherwise = do
          fileError <- Validation.fileAccess workDir file

          case fileError of
            Just err -> Output.echo err
            Nothing -> do
              let file' = makeAbsoluteFrom workDir file
              idx <- Manager.readIdx workDir
              let exists = Manager.idxEntryExists idx file'

              if not opts.add && not exists
                then Output.echo $ "Cannot add " <> file <> " to the index"
                else do
                  mode <- makeMode mode file'
                  hashEither <- makeOid oid file'

                  case hashEither of
                    Left err -> Output.echo err
                    Right oid -> do
                      let entry =
                            Manager.IdxEntry
                              { mode = mode,
                                oid = oid,
                                path = file'
                              }

                      let idx = Manager.insertIdxEntry idx entry
                      Manager.writeIdx workDir idx

    makeMode :: Maybe String -> FilePath -> IO String
    makeMode mode file =
      case mode of
        Nothing -> getFileMode file
        Just mode' -> return mode'

    makeOid :: Maybe String -> FilePath -> IO (Either String Manager.ObjId)
    makeOid oid file = case oid of
      Nothing -> do
        content <- ByteString.readFile file
        let (oid', obj) = Manager.makeObj Manager.ObjBlob content
        Manager.writeObj workDir (oid', obj)
        return $ Right oid'
      Just oid' -> return $ Manager.validateObjId $ Manager.ObjId oid'

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
        let ci = CacheInfo {mode = raw, oid = obj, file = path}
            (more, remaining) = resolveCacheInfos rest filesRest
         in (ci : more, remaining)
      _ -> resolveCacheInfos rest files

parseCacheInfoComma :: String -> Maybe CacheInfo
parseCacheInfoComma s =
  case break (== ',') s of
    (mode, ',' : rest) -> case break (== ',') rest of
      (obj, ',' : path)
        | not (null mode) && not (null obj) && not (null path) ->
            Just (CacheInfo {mode = mode, oid = obj, file = path})
      _ -> Nothing
    _ -> Nothing
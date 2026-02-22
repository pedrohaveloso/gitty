{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Cmd.UpdateIndex (cmdUpdateIndex, Options (..), definition) where

import qualified Data.ByteString as ByteString
import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common (WorkDir, getFileMode, isInsideRepoDir, makeAbsoluteFrom, needRepo)
import qualified Gitty.Manager as Manager
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
cmdUpdateIndex workDir opts = needRepo workDir cmdUpdateIndex'
  where
    cmdUpdateIndex' =
      let args =
            if null opts.cacheInfos
              then map (,Nothing,Nothing) opts.files
              else map (\ci -> (ci.file, Just ci.mode, Just ci.oid)) opts.cacheInfos
       in mapM_
            (\(file, mode, oid) -> processFile workDir (opts.add, file, mode, oid))
            args

processFile :: WorkDir -> (Bool, FilePath, Maybe String, Maybe String) -> IO ()
processFile workDir (add, file, mode, oid)
  | isInsideRepoDir workDir file = return ()
  | otherwise = do
      fileError <- Validation.fileAccess workDir file

      case fileError of
        Just err -> putStrLn err
        Nothing -> do
          idx <- Manager.readIdx workDir
          let exists = Manager.idxEntryExists idx makeFile

          if not add && not exists
            then putStrLn $ "Cannot add " <> file <> " to the index"
            else do
              mode' <- makeMode
              oidEither <- makeOid

              case oidEither of
                Left err -> putStrLn err
                Right oid' -> do
                  let entry =
                        Manager.IdxEntry
                          { mode = mode',
                            oid = oid',
                            path = makeFile
                          }

                  Manager.writeIdx workDir (Manager.insertIdxEntry idx entry)
  where
    makeFile :: FilePath
    makeFile = makeAbsoluteFrom workDir file

    makeMode :: IO String
    makeMode =
      case mode of
        Nothing -> getFileMode makeFile
        Just mode' -> return mode'

    makeOid :: IO (Either String Manager.ObjId)
    makeOid = case oid of
      Nothing -> do
        content <- ByteString.readFile makeFile
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
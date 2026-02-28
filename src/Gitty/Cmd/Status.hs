{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Cmd.Status (cmdStatus, definition) where

import Control.Monad (filterM, when)
import qualified Data.ByteString as ByteString
import Data.List (sort)
import qualified Data.Map as Map
import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common
  ( WorkDir,
    getRecursiveFiles,
    makeAbsoluteFrom,
    makeRelativeTo,
    needRepo,
  )
import qualified Gitty.Manager as Manager
import qualified Options.Applicative as Cli
import qualified System.Directory as Directory
import System.FilePath ((</>))

cmdStatus :: WorkDir -> () -> IO ()
cmdStatus workDir _ = needRepo workDir status
  where
    status :: IO ()
    status = do
      headRef <- Manager.readSymbolicRef workDir "HEAD"
      let branch = case headRef of
            Just ref -> drop (length ("refs/heads/" :: String)) ref
            Nothing -> "detached"

      putStrLn $ "On branch " <> branch

      idx <- Manager.readIdx workDir
      let indexPaths = sort $ map (.path) idx.entries

      headTreeMap <- getHeadTreeMap workDir

      let stagedNewOrModified =
            sort
              [ e.path
              | e <- idx.entries,
                case Map.lookup e.path headTreeMap of
                  Nothing -> True
                  Just headOid -> headOid /= e.oid
              ]
          stagedDeleted =
            sort [p | p <- Map.keys headTreeMap, p `notElem` indexPaths]
          staged = stagedNewOrModified ++ stagedDeleted

      allFiles <- getRecursiveFiles workDir
      let workFiles = sort $ map (makeRelativeTo workDir) allFiles
          untracked = filter (`notElem` indexPaths) workFiles

      modified <- filterM (isModified idx) indexPaths
      deleted <- filterM isDeleted indexPaths

      let hasStaged = not (null staged)
          hasUnstaged = not (null modified) || not (null deleted) || not (null untracked)

      if not hasStaged && not hasUnstaged
        then putStrLn "nothing to commit, working tree clean"
        else do
          when hasStaged $
            printSection "Changes to be committed:" $
              ["\tnew file:   " <> p | p <- stagedNewOrModified, p `notElem` Map.keys headTreeMap]
                ++ ["\tmodified:   " <> p | p <- stagedNewOrModified, p `elem` Map.keys headTreeMap]
                ++ map ("\tdeleted:    " <>) stagedDeleted

          when hasUnstaged $ do
            printSection "Changes not staged for commit:" $
              map ("\tmodified:   " <>) modified
                ++ map ("\tdeleted:    " <>) deleted

            printSection "Untracked files:" $
              map ("\t" <>) untracked

    getHeadTreeMap :: WorkDir -> IO (Map.Map FilePath Manager.ObjId)
    getHeadTreeMap wd = do
      headMaybe <- Manager.resolveRef wd "HEAD"
      case headMaybe of
        Nothing -> return Map.empty
        Just commitOid -> do
          objMaybe <- Manager.readObj wd commitOid
          case objMaybe >>= Manager.parseCommitContent . (.content) of
            Nothing -> return Map.empty
            Just info -> do
              treeMaybe <- Manager.readTreeObj wd info.tree
              return $ case treeMaybe of
                Nothing -> Map.empty
                Just t -> Map.fromList (flattenTreeStruct t)

    flattenTreeStruct :: Manager.TreeStruct -> [(FilePath, Manager.ObjId)]
    flattenTreeStruct (Manager.TreeEntry e) = [(e.path, e.oid)]
    flattenTreeStruct (Manager.TreeStruct _ children) =
      concatMap (flattenChild "") children
      where
        flattenChild :: FilePath -> Manager.TreeStruct -> [(FilePath, Manager.ObjId)]
        flattenChild prefix (Manager.TreeEntry e) =
          [(if null prefix then e.path else prefix </> e.path, e.oid)]
        flattenChild prefix (Manager.TreeStruct dirName ch) =
          concatMap
            (flattenChild (if null prefix then dirName else prefix </> dirName))
            ch

    isModified :: Manager.Idx -> FilePath -> IO Bool
    isModified idx path = do
      let absPath = makeAbsoluteFrom workDir path
      exists <- Directory.doesFileExist absPath

      if exists
        then do
          content <- ByteString.readFile absPath
          let (currentOid, _) = Manager.makeObj Manager.ObjBlob content
              indexOid = lookup path [(e.path, e.oid) | e <- idx.entries]
          return $ indexOid /= Just currentOid
        else return False

    isDeleted :: FilePath -> IO Bool
    isDeleted path = do
      let absPath = makeAbsoluteFrom workDir path
      exists <- Directory.doesFileExist absPath
      return $ not exists

    printSection :: String -> [String] -> IO ()
    printSection _ [] = return ()
    printSection header items = do
      putStrLn ""
      putStrLn header
      mapM_ putStrLn items

definition :: CmdDefinition ()
definition =
  CmdDefinition
    { name = "status",
      description = "Show the working tree status",
      parser = Cli.pure ()
    }

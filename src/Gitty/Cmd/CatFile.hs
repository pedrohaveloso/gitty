{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Cmd.CatFile (cmdCatFile, definition) where

import Control.Applicative ((<|>))
import qualified Data.ByteString as ByteString
import Gitty.Cmd.Common (CmdDefinition (..))
import Gitty.Common (WorkDir, needRepo)
import qualified Gitty.Manager as Manager
import qualified Options.Applicative as Cli

data Mode = ShowType | ShowSize | PrettyPrint

data Options = Options
  { mode :: Mode,
    oid :: String
  }

cmdCatFile :: WorkDir -> Options -> IO ()
cmdCatFile workDir opts = needRepo workDir catFile
  where
    catFile :: IO ()
    catFile = case Manager.validateObjId (Manager.ObjId opts.oid) of
      Left err -> putStrLn err
      Right oid -> do
        maybeObj <- Manager.readObj workDir oid
        case maybeObj of
          Nothing -> putStrLn $ "fatal: Not a valid object name " <> opts.oid
          Just obj -> case opts.mode of
            ShowType -> putStrLn $ showKind obj.kind
            ShowSize -> print obj.len
            PrettyPrint -> prettyPrint obj

    prettyPrint :: Manager.Obj -> IO ()
    prettyPrint obj = case obj.kind of
      Manager.ObjTree -> case Manager.deserializeTreeEntries obj.content of
        Nothing -> putStrLn "fatal: Could not parse tree object"
        Just entries -> mapM_ printTreeEntry entries
      _ -> ByteString.putStr obj.content

    printTreeEntry :: (String, FilePath, Manager.ObjId) -> IO ()
    printTreeEntry (entryMode, entryName, oid) =
      putStrLn $ entryMode <> " " <> entryKind <> " " <> show oid <> "\t" <> entryName
      where
        entryKind = if entryMode == "40000" then "tree" else "blob"

showKind :: Manager.ObjKind -> String
showKind Manager.ObjBlob = "blob"
showKind Manager.ObjCommit = "commit"
showKind Manager.ObjTree = "tree"
showKind Manager.ObjTag = "tag"

definition :: CmdDefinition Options
definition =
  CmdDefinition
    { name = "cat-file",
      description = "Provide content or kind and size information for repository objects",
      parser =
        Options
          <$> ( Cli.flag' ShowType (Cli.short 'k' <> Cli.help "Show object kind")
                  <|> Cli.flag' ShowSize (Cli.short 's' <> Cli.help "Show object size")
                  <|> Cli.flag' PrettyPrint (Cli.short 'p' <> Cli.help "Pretty-print object content")
              )
          <*> Cli.argument Cli.str (Cli.metavar "OBJECT" <> Cli.help "The object ID to display")
    }

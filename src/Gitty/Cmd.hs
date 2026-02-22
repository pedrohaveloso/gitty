{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Cmd (run) where

import Control.Monad (join)
import qualified Gitty.Cmd.Add as Cmd.Add
import qualified Gitty.Cmd.CatFile as Cmd.CatFile
import Gitty.Cmd.Common (CmdDefinition (..))
import qualified Gitty.Cmd.HashObject as Cmd.HashObject
import qualified Gitty.Cmd.Init as Cmd.Init
import qualified Gitty.Cmd.UpdateIndex as Cmd.UpdateIndex
import qualified Gitty.Cmd.WriteTree as Cmd.WriteTree
import Gitty.Common (WorkDir)
import qualified Options.Applicative as Cli
import qualified System.Directory as Directory

data Command = forall opts. Command (CmdDefinition opts) (WorkDir -> opts -> IO ())

commands :: [Command]
commands =
  [ Command Cmd.Add.definition Cmd.Add.cmdAdd,
    Command Cmd.CatFile.definition Cmd.CatFile.cmdCatFile,
    Command Cmd.HashObject.definition Cmd.HashObject.cmdHashObject,
    Command Cmd.Init.definition Cmd.Init.cmdInit,
    Command Cmd.UpdateIndex.definition Cmd.UpdateIndex.cmdUpdateIndex,
    Command Cmd.WriteTree.definition Cmd.WriteTree.cmdWriteTree
  ]

buildParser :: WorkDir -> [Command] -> Cli.Parser (IO ())
buildParser workDir = Cli.subparser . foldMap build
  where
    build (Command def exec) =
      Cli.command
        def.name
        ( Cli.info
            (exec workDir <$> def.parser)
            (Cli.progDesc def.description)
        )

run :: IO ()
run = do
  workDir <- Directory.getCurrentDirectory

  join $
    Cli.execParser $
      Cli.info
        (buildParser workDir commands Cli.<**> Cli.helper)
        ( Cli.fullDesc
            <> Cli.progDesc "Pseudo-Git implementation in Haskell"
            <> Cli.header "Gitty"
        )

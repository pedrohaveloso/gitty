{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Gitty.Cmd (run) where

import Control.Monad (join)
import Gitty.Cmd.Common (CmdDefinition (..))
import qualified Gitty.Cmd.HashObject as Cmd.HashObject
import qualified Gitty.Cmd.Init as Cmd.Init
import qualified Gitty.Cmd.UpdateIndex as Cmd.UpdateIndex
import Gitty.Common (WorkDir)
import qualified Options.Applicative as Cli
import qualified System.Directory as Directory

data Command = forall opts. Command (CmdDefinition opts) (WorkDir -> opts -> IO ())

commands :: [Command]
commands =
  [ Command Cmd.Init.definition Cmd.Init.cmdInit,
    Command Cmd.HashObject.definition Cmd.HashObject.cmdHashObject,
    Command Cmd.UpdateIndex.definition Cmd.UpdateIndex.cmdUpdateIndex
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

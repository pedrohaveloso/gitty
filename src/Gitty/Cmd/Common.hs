module Gitty.Cmd.Common (CmdDefinition (..)) where

import qualified Options.Applicative as Cli

data CmdDefinition options = CmdDefinition
  { name :: String,
    description :: String,
    parser :: Cli.Parser options
  }
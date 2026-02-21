module Gitty (cli) where

import qualified Gitty.Cmd

cli :: IO ()
cli = Gitty.Cmd.run
module GittyCli.Command.HashObject (Options (..), run) where

import qualified Gitty
import qualified System.Directory as Directory

data Options = Options
  { write :: Bool,
    kind :: String,
    file :: String
  }
  deriving (Show)

run :: Options -> IO ()
run (Options {write = write', kind = kind', file = file'}) = do
  fileExists <- Directory.doesFileExist file'
  
  if fileExists then run'

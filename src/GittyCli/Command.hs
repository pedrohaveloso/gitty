module GittyCli.Command (Available (..), parser, run) where

import qualified Gitty
import Gitty.Prelude (WorkDir)
import qualified Gitty.Validation as Gitty.Validation
import qualified GittyCli.Command.HashObject as HashObject
import Options.Applicative
  ( Parser,
    command,
    info,
    progDesc,
    subparser,
  )

data Available
  = HashObject HashObject.Options
  deriving (Show)

parser :: Parser Available
parser =
  subparser
    ( command
        "hash-object"
        ( info
            (HashObject <$> HashObject.parser)
            (progDesc "Compute object ID and optionally creates a blob from a file")
        )
    )

needRepo :: ((WorkDir -> options -> IO ()), options) -> WorkDir -> IO ()
needRepo (fn, options) workDir = do
  repoExists <- Gitty.Validation.repoExists workDir

  case repoExists of
    Nothing -> fn workDir options
    Just err -> Gitty.fatal err

run :: WorkDir -> Available -> IO ()
run workDir cmd = case cmd of
  HashObject opts -> needRepo (HashObject.run, opts) workDir

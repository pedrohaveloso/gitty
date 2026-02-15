module GittyCli.Command (Available (..), parser, run) where

import qualified Gitty
import Gitty.Prelude (WorkDir)
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

run :: WorkDir -> Available -> IO ()
run workDir cmd = case cmd of
  HashObject opts -> run' True opts HashObject.run
  where
    run' :: Bool -> options -> (WorkDir -> options -> IO ()) -> IO ()
    run' needRepo options fn =
      if needRepo
        then do
          repoExists <- Gitty.repoExists

          if repoExists
            then fn workDir options
            else Gitty.fatal "There is no repository in that folder."
        else fn workDir options

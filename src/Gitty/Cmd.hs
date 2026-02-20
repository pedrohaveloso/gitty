module GittyCli.Command (Available (..), parser, run) where

import qualified Gitty
import Gitty.Prelude (WorkDir)
import qualified Gitty.Validation
import qualified GittyCli.Command.Add as Add
import qualified GittyCli.Command.HashObject as HashObject
import qualified GittyCli.Command.UpdateIndex as UpdateIndex
import Options.Applicative
  ( Parser,
    command,
    info,
    progDesc,
    subparser,
  )

data Available
  = HashObject HashObject.Options
  | UpdateIndex UpdateIndex.Options
  | Add Add.Options
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
        <> command
          "update-index"
          ( info
              (UpdateIndex <$> UpdateIndex.parser)
              (progDesc "Register file contents in the working tree to the index")
          )
        <> command
          "add"
          ( info
              (Add <$> Add.parser)
              (progDesc "Add file contents to the index")
          )
    )

needRepo :: (WorkDir -> options -> IO (), options) -> WorkDir -> IO ()
needRepo (fn, options) workDir = do
  repoExists <- Gitty.Validation.repoExists workDir

  case repoExists of
    Nothing -> fn workDir options
    Just err -> Gitty.fatal err

run :: WorkDir -> Available -> IO ()
run workDir cmd = case cmd of
  HashObject opts -> needRepo (HashObject.run, opts) workDir
  UpdateIndex opts -> needRepo (UpdateIndex.run, opts) workDir
  Add opts -> needRepo (Add.run, opts) workDir

module GittyCli (run) where

import qualified GittyCli.Command as Command
import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    header,
    helper,
    info,
    progDesc,
    (<**>),
  )
import qualified System.Directory as Directory

newtype Options
  = Options {command :: Command.Available}
  deriving (Show)

optionsParser :: Parser Options
optionsParser = Options <$> Command.parser

run :: IO ()
run = do
  opts <-
    execParser $
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Pseudo-Git implementation in Haskell"
            <> header "Gitty"
        )

  workDir <- Directory.getCurrentDirectory

  Command.run workDir (command opts)

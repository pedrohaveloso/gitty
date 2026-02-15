module GittyCli (run) where

import qualified Gitty
import qualified Gitty.Object
import qualified GittyCli.Command as Command
import Options.Applicative
  ( Parser,
    argument,
    command,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    short,
    showDefault,
    str,
    strOption,
    subparser,
    switch,
    value,
    (<**>),
  )

data Options = Options
  { commandKind :: Command.Available
  }
  deriving (Show)

hashObjectParser :: Parser Command
hashObjectParser =
  HashObject
    <$> ( HashObjectOptions
            <$> switch
              ( long "write"
                  <> short 'w'
                  <> help "Actually write the object into the database"
              )
            <*> strOption
              ( long "kind"
                  <> short 'k'
                  <> value "blob"
                  <> showDefault
                  <> metavar "KIND"
                  <> help "Specify the kind (blob, commit, tree, tag)"
              )
            <*> argument
              str
              ( metavar "FILE"
                  <> help "File to hash"
              )
        )

commandParser :: Parser Command
commandParser =
  subparser
    ( command
        "hash-object"
        ( info
            hashObjectParser
            (progDesc "Compute object ID and optionally creates a blob from a file")
        )
    )

run :: IO ()
run = do
  Options {commandKind = cmd} <-
    execParser $
      info
        (Options <$> commandParser <**> helper)
        ( fullDesc
            <> progDesc "Pseudo-Git implementation in Haskell"
            <> header "Gitty"
        )

  case cmd of
    HashObject o -> runHashObjectCommand o

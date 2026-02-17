module GittyCli.Command.HashObject (Options, parser, run) where

import qualified Gitty
import qualified Gitty.Object as Object
import Gitty.Prelude (WorkDir)
import Options.Applicative
  ( Parser,
    argument,
    help,
    long,
    metavar,
    short,
    showDefault,
    str,
    strOption,
    switch,
    value,
  )

data Options = Options
  { write :: Bool,
    kind :: String,
    file :: FilePath
  }
  deriving (Show)

parser :: Parser Options
parser =
  Options
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

run :: WorkDir -> Options -> IO ()
run workDir (Options {write = w, kind = k, file = f}) = do
  result <- Object.hashFile workDir w kind' f

  case result of
    Left err -> Gitty.fatal err
    Right hash -> Gitty.msg hash
  where
    kind' = Object.kindFromString k

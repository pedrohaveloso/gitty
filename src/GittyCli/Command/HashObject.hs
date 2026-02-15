module GittyCli.Command.HashObject (Options, parser, run) where

import qualified Data.ByteString as ByteString
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
import qualified System.Directory as Directory

data Options = Options
  { write :: Bool,
    kind :: String,
    file :: String
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
  fileExists <- Directory.doesFileExist f
  fileReadable <- Directory.readable <$> Directory.getPermissions f

  runValidated fileExists fileReadable
  where
    runValidated exists readable
      | not exists = Gitty.fatal $ "The file " <> f <> " was not found."
      | not readable = Gitty.fatal $ "The file " <> f <> " is not readable."
      | otherwise = hashObject >>= putStrLn

    hashObject :: IO String
    hashObject = do
      rawContent <- ByteString.readFile f

      let content = Object.makeContent rawContent (Object.kindFromString k)
          hash = Object.hashContent content

      if w
        then Object.writeContent workDir hash content >> return hash
        else return hash
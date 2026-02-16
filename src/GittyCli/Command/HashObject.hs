module GittyCli.Command.HashObject
  ( Options,
    parser,
    run,
    hashObject,
  )
where

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
run workDir options = do
  result <- hashObject workDir options

  case result of
    Left err -> Gitty.fatal err
    Right hash -> Gitty.msg hash

hashObject :: WorkDir -> Options -> IO (Either String Object.Hash)
hashObject workDir (Options {write = w, kind = k, file = f}) = do
  fileExists <- Directory.doesFileExist f
  fileReadable <- Directory.readable <$> Directory.getPermissions f

  validate fileExists fileReadable
  where
    validate :: Bool -> Bool -> IO (Either String Object.Hash)
    validate exists readable
      | not exists = pure $ Left $ "The file " <> f <> " was not found."
      | not readable = pure $ Left $ "The file " <> f <> " is not readable."
      | otherwise = hashObject'

    hashObject' :: IO (Either String Object.Hash)
    hashObject' = do
      rawContent <- ByteString.readFile f

      let content = Object.makeContent rawContent (Object.kindFromString k)
          hash = Object.hashContent content

      if w
        then do
          Object.writeContent workDir hash content
          return $ Right hash
        else return $ Right hash
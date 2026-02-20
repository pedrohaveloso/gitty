module Gitty (cli) where

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

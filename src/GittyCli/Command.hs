module GittyCli.Command (Available) where

import qualified GittyCli.Command.HashObject as HashObjectCommand

data Available
  = HashObject HashObjectCommand.Options
  deriving (Show)

run :: Available -> IO ()
run command = case command of
  HashObject -> HashObjectCommand.run

module Main (main) where

import qualified Gitty
import qualified System.Directory as Directory
import qualified System.Environment as Environment

main :: IO ()
main = do
  args <- Environment.getArgs
  let arg = head args

  workDir <- Directory.getCurrentDirectory

  case arg of
    "init" -> do
      x <- Gitty.init workDir
      print x
    "add" -> do
      x <- Gitty.add workDir (tail args)
      case x of
        Left err -> putStrLn err
        Right msg -> putStrLn msg
    _ -> print "error"

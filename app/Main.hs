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
      putStrLn x
    "add" -> do
      x <- Gitty.add (tail args) workDir
      case x of
        Left err -> putStrLn err
        Right _ -> putStrLn "success"
    _ -> print "error"

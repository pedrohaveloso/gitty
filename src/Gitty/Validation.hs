module Gitty.Validation (fileAccess) where

import Data.List (isPrefixOf)
import Gitty.Common (WorkDir, makeAbsoluteFrom)
import qualified System.Directory as Directory

type ValidationError = String

validateFileAccess :: FilePath -> Bool -> Bool -> Bool -> Maybe ValidationError
validateFileAccess file exists readable inRepo
  | not exists = Just $ "The file " <> file <> " was not found."
  | not readable = Just $ "The file " <> file <> " is not readable."
  | not inRepo = Just $ "The file " <> file <> " is outside repository."
  | otherwise = Nothing

fileAccess :: WorkDir -> FilePath -> IO (Maybe ValidationError)
fileAccess workDir file = do
  if null file
    then return $ Just "The file value is empty"
    else do
      exists <- Directory.doesFileExist absoluteFile
      readable <- Directory.readable <$> Directory.getPermissions absoluteFile

      return $ validateFileAccess file exists readable inRepo
  where
    absoluteFile :: String
    absoluteFile = makeAbsoluteFrom workDir file

    inRepo :: Bool
    inRepo = workDir `isPrefixOf` absoluteFile

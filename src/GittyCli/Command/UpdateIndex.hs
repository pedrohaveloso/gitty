module GittyCli.Command.UpdateIndex (Options, parser, run) where

import Control.Monad (when)
import qualified Gitty
import qualified Gitty.Index as Index
import Gitty.Prelude (WorkDir)
import Options.Applicative
  ( Parser,
    argument,
    help,
    long,
    metavar,
    optional,
    short,
    str,
    strOption,
    switch,
  )
import System.Exit (die)

data Options = Options
  { add :: Bool,
    cacheinfo :: Maybe CacheInfo,
    file :: FilePath
  }
  deriving (Show)

data CacheInfo = CacheInfo
  { mode :: String,
    hash :: String,
    path :: FilePath
  }
  deriving (Show)

parser :: Parser Options
parser =
  Options
    <$> switch
      ( long "add"
          <> help "Add the file to the index if it doesn't exist"
      )
    <*> optional cacheinfoParser
    <*> argument
      str
      ( metavar "FILE"
          <> help "File to add to index"
      )

cacheinfoParser :: Parser CacheInfo
cacheinfoParser =
  CacheInfo
    <$> strOption
      ( long "cacheinfo-mode"
          <> metavar "MODE"
          <> help "File mode (e.g., 100644, 100755)"
      )
    <*> strOption
      ( long "cacheinfo-hash"
          <> metavar "HASH"
          <> help "Object hash"
      )
    <*> strOption
      ( long "cacheinfo-path"
          <> metavar "PATH"
          <> help "File path in repository"
      )

run :: WorkDir -> Options -> IO ()
run workDir (Options {add = shouldAdd, cacheinfo = maybeCacheInfo, file = filepath}) = do
  case maybeCacheInfo of
    -- Modo: --cacheinfo fornecido (informações manuais)
    Just (CacheInfo {mode = m, hash = h, path = p}) -> do
      runWithCacheInfo workDir shouldAdd m h p

    -- Modo: sem --cacheinfo (ler do disco)
    Nothing -> do
      runWithoutCacheInfo workDir shouldAdd filepath

-- Quando --cacheinfo é usado
runWithCacheInfo :: WorkDir -> Bool -> String -> String -> FilePath -> IO ()
runWithCacheInfo workDir shouldAdd mode hash path = do
  result <- Index.updateIndex workDir shouldAdd (Just mode) (Just hash) path
  case result of
    Left err -> die $ "Error: " ++ err
    Right () -> putStrLn $ "Updated index: " ++ path ++ " (mode: " ++ mode ++ ", hash: " ++ hash ++ ")"

-- Quando --cacheinfo NÃO é usado (automático)
runWithoutCacheInfo :: WorkDir -> Bool -> FilePath -> IO ()
runWithoutCacheInfo workDir shouldAdd filepath = do
  -- Git vai automaticamente:
  -- 1. Ler o arquivo do disco
  -- 2. Calcular o hash (hash-object -w)
  -- 3. Obter o mode das permissões
  -- 4. Criar o blob
  -- 5. Atualizar o índice
  result <- Index.updateIndex workDir shouldAdd Nothing Nothing filepath
  case result of
    Left err -> die $ "Error: " ++ err
    Right () -> putStrLn $ "Added to index: " ++ filepath
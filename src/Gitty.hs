module Gitty (fatal, msg) where

import qualified Data.ByteString as BS
import qualified Gitty.Object as Object
import Gitty.Prelude (WorkDir)
import qualified System.Directory as Directory

needRepo :: WorkDir -> a -> IO a
needRepo workDir a = return a

-- todo

fatal :: String -> IO ()
fatal m = putStrLn $ "Fatal error:\n'" <> m <> "'"

msg :: String -> IO ()
msg = putStrLn

-- {-# LANGUAGE TupleSections #-}

-- module Gitty (init, add) where

-- import Codec.Compression.Zlib (compress, decompress)
-- import qualified Crypto.Hash.SHA1 as SHA1
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as C
-- import Data.Function ((&))
-- import Data.List (intercalate)
-- import qualified Data.Map as Map
-- import qualified System.Directory as Directory
-- import System.FilePath ((</>))
-- import qualified System.FilePath as FilePath
-- import Text.Printf (printf)
-- import Prelude hiding (init)

-- type WorkDir = FilePath

-- -- todo:
-- -- move to objects
-- blobType :: String
-- blobType = "blob"

-- -- index
-- -- objects/*
-- -- refs/heads/
-- -- refs/tags/

-- gittyDir :: FilePath -> FilePath
-- gittyDir workDir = workDir ++ "/.gitty"

-- -- todo: move to object module
-- objectsDir :: FilePath -> FilePath
-- objectsDir = (++ "/objects") . gittyDir

-- reinitialize :: IO String
-- reinitialize = return "Reinitialized existing repository"

-- initialize :: WorkDir -> IO String
-- initialize workDir = do
--   Directory.createDirectoryIfMissing True $ gittyDir workDir
--   return "Initialized empty repository"

-- init :: WorkDir -> IO String
-- init workDir = do
--   isInit <- checkIsInit workDir
--   if isInit then reinitialize else initialize workDir

-- checkIsInit :: FilePath -> IO Bool
-- checkIsInit = Directory.doesDirectoryExist . gittyDir

-- needIsInit :: (WorkDir -> IO (Either String ())) -> WorkDir -> IO (Either String ())
-- needIsInit fn workDir = do
--   isInit <- checkIsInit workDir

--   if isInit
--     then fn workDir
--     else return $ Left "Error: not a repository"

-- -- todo
-- type FileHash = String

-- data Index = Index
--   { indexFiles :: Map.Map FilePath FileHash
--   }

-- writeIndex :: WorkDir -> Index -> IO ()
-- writeIndex workDir (Index files) = do
--   let indexPath = gittyDir workDir ++ "/index"
--       lines = map (\(p, h) -> C.pack $ p ++ "," ++ h) (Map.toList files)
--       content = C.intercalate (str2bs "\n") lines
--       compressed = BS.toStrict $ compress $ BS.fromStrict content
--   BS.writeFile indexPath compressed

-- -- todo
-- readIndex :: WorkDir -> IO Index
-- readIndex workDir = do
--   let indexPath = gittyDir workDir ++ "/index"
--   exists <- Directory.doesFileExist indexPath
--   if not exists
--     then return $ Index Map.empty
--     else do
--       compressed <- BS.readFile indexPath
--       let content = BS.toStrict $ decompress $ BS.fromStrict compressed
--           lines = filter (not . BS.null) $ C.split '\n' content
--           entries =
--             map
--               ( \line ->
--                   let [p, h] = C.split ',' line
--                    in (C.unpack p, C.unpack h)
--               )
--               lines
--       return $ Index $ Map.fromList entries

-- bs2hex :: BS.ByteString -> String
-- bs2hex = concatMap (printf "%02x") . BS.unpack

-- str2bs :: String -> BS.ByteString
-- str2bs = C.pack

-- bs2str :: BS.ByteString -> String
-- bs2str = C.unpack

-- -- hashFile =

-- makeAbsoluteFrom :: FilePath -> FilePath -> FilePath
-- makeAbsoluteFrom baseDir path
--   | FilePath.isAbsolute path = path
--   | otherwise = FilePath.normalise (baseDir </> path)

-- getNonExistentPaths :: [FilePath] -> IO [FilePath]
-- getNonExistentPaths paths = result
--   where
--     relation = mapM (\path -> (path,) <$> Directory.doesPathExist path) paths
--     result = fmap (map (\(path, _) -> path) . filter (\(_, exists) -> not exists)) relation

-- add :: [FilePath] -> WorkDir -> IO (Either String ())
-- add paths =
--   needIsInit
--     ( \workDir -> do
--         let absolutePaths = map (makeAbsoluteFrom workDir) paths
--         let relativePaths = map (FilePath.makeRelative workDir) absolutePaths

--         nonExistentPaths <- getNonExistentPaths relativePaths

--         if null nonExistentPaths
--           then add' workDir relativePaths >>= return . Right
--           else return $ Left $ nonExistentPathsError nonExistentPaths
--     )
--   where
--     nonExistentPathsError :: [FilePath] -> String
--     nonExistentPathsError nonExistentPaths =
--       "The following files do not exist in the root directory of the current repository:\n"
--         ++ (intercalate "\n" $ map ("• " ++) nonExistentPaths)

--     add' :: WorkDir -> [FilePath] -> IO ()
--     add' _ [] = return ()
--     add' workDir (path : rest) = do
--       _ <- add' workDir rest
--       isFile <- Directory.doesFileExist path
--       if isFile then addFile workDir path else addDir workDir path

--     addFile :: WorkDir -> FilePath -> IO ()
--     addFile workDir filePath = do
--       originalContent <- BS.readFile filePath

--       let contentLength = BS.length originalContent
--           contentHeader = str2bs $ blobType <> " " <> show contentLength

--           content = contentHeader <> str2bs "\0" <> originalContent

--           hashedContent = content & SHA1.hash & bs2hex
--           compressedContent = content & BS.fromStrict & compress & BS.toStrict

--           fileDir = objectsDir workDir <> "/" <> take 2 hashedContent
--           fileName = fileDir <> "/" <> drop 2 hashedContent

--       Directory.createDirectoryIfMissing True fileDir
--       BS.writeFile fileName compressedContent

--       index <- readIndex workDir
--       let idxFiles = indexFiles index & Map.insert filePath hashedContent
--       writeIndex workDir (Index {indexFiles = idxFiles})

--       return ()

--     addDir :: WorkDir -> FilePath -> IO ()
--     addDir workDir dirPath = undefined -- todo

-- commit :: WorkDir
-- commit = undefined

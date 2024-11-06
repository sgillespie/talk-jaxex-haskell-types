module Development.Examples.Parse where

import Control.Exception (throwIO)
import Data.List.Extra (splitOn)
import Development.Examples.Basics (headMay)
import System.Environment (getEnv)

getConfigurationDirectories :: IO [FilePath]
getConfigurationDirectories = do
  -- Read environment variable CONFIG_DIRS as a List of `FilePaths`
  configDirsString <- getEnv "CONFIG_DIRS"
  let configDirsList = splitOn "," configDirsString

  -- Make sure it's not empty
  if null configDirsList
    then throwIO (userError "CONFIG_DIRS cannot be empty")
    else pure configDirsList

setUpConfigurationDirectories :: IO ()
setUpConfigurationDirectories = do
  configDirs <- getConfigurationDirectories

  case headMay configDirs of
    Just cacheDir -> initializeCache cacheDir
    Nothing -> error "should never happen; already checked configDirs is non-empty"

initializeCache :: FilePath -> IO ()
initializeCache _ = pure ()

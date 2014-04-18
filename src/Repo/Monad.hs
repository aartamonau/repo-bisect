{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Repo.Monad
       ( RepoInfo(repoRootDir, repoStateDir)
       , runRepo
       , Repo
       ) where

import Control.Monad (when, unless)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Control.Monad.Trans (MonadIO, liftIO)

import System.Directory (getCurrentDirectory, canonicalizePath,
                         doesDirectoryExist, doesFileExist, createDirectory)
import System.FilePath (combine, takeDirectory)

data RepoInfo = RepoInfo { repoRootDir :: FilePath
                         , repoStateDir :: FilePath
                         }

newtype Repo a = Repo { unRepo :: ReaderT RepoInfo IO a }
               deriving (Monad, MonadReader RepoInfo, MonadIO)

runRepo :: MonadIO m => Repo a -> m a
runRepo r = do
  root <- liftIO findRootDir
  state <- liftIO $ mustStateDir root

  let info = RepoInfo { repoRootDir = root
                      , repoStateDir = state
                      }

  liftIO $ runReaderT (unRepo r) info

findRootDir :: IO FilePath
findRootDir = go =<< canonicalizePath =<< getCurrentDirectory
  where go :: FilePath -> IO FilePath
        go dir = do
          exists <- doesDirectoryExist $ combine dir ".repo"
          if exists
            then return dir
            else do
              let parentDir = takeDirectory dir
              when (parentDir == dir) $
                error "could not find .repo directory"
              go parentDir

mustDir :: FilePath -> IO ()
mustDir dir = do
  exists <- doesDirectoryExist dir
  unless exists $ do
    fileExists <- doesFileExist dir
    when fileExists $
      error $ "failed to create state directory (" ++ dir ++ "): file exists"
    createDirectory dir

mustStateDir :: FilePath -> IO FilePath
mustStateDir rootDir = do
  mustDir stateDir
  mustDir (combine stateDir "snapshots")
  return stateDir

  where stateDir = combine rootDir ".repo-utils"

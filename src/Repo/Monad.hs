{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Repo.Monad
       ( RepoInfo(repoRootDir)
       , runRepo
       , Repo
       ) where

import Control.Monad (when)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Control.Monad.Trans (MonadIO, liftIO)

import System.Directory (getCurrentDirectory, canonicalizePath,
                         doesDirectoryExist)
import System.FilePath (combine, takeDirectory)

data RepoInfo = RepoInfo { repoRootDir :: FilePath
                         }

newtype Repo a = Repo { unRepo :: ReaderT RepoInfo IO a }
               deriving (Monad, MonadReader RepoInfo, MonadIO)

runRepo :: MonadIO m => Repo a -> m a
runRepo r = do
  root <- liftIO findRootDir
  let info = RepoInfo { repoRootDir = root }

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

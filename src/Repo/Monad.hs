{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Repo.Monad
       ( RepoInfo(repoRootDir, repoStateDir, repoSnapshotsDir)
       , runRepo
       , Repo
       ) where

import Control.Monad (when)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Control.Monad.Trans (MonadIO)

import System.Directory (getCurrentDirectory, canonicalizePath,
                         doesDirectoryExist)
import System.FilePath (combine, takeDirectory)

import Repo.Utils (mustDir, io)

data RepoInfo = RepoInfo { repoRootDir :: FilePath
                         , repoStateDir :: FilePath
                         , repoSnapshotsDir :: FilePath
                         }

newtype Repo a = Repo { unRepo :: ReaderT RepoInfo IO a }
               deriving (Monad, MonadReader RepoInfo, MonadIO)

runRepo :: MonadIO m => Repo a -> m a
runRepo r = do
  root <- io findRootDir
  state <- io $ mustStateDir root
  snapshots <- io $ mustSnapshotsDir state

  let info = RepoInfo { repoRootDir = root
                      , repoStateDir = state
                      , repoSnapshotsDir = snapshots
                      }

  io $ runReaderT (unRepo r) info

  where findRootDir :: IO FilePath
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

        mustStateDir :: FilePath -> IO FilePath
        mustStateDir rootDir = do
          mustDir stateDir
          return stateDir

          where stateDir = combine rootDir ".repo-utils"

        mustSnapshotsDir :: FilePath -> IO FilePath
        mustSnapshotsDir stateDir = do
          mustDir snapshotsDir
          return snapshotsDir

          where snapshotsDir = (combine stateDir "snapshots")

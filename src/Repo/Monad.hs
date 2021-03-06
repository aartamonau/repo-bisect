{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module Repo.Monad
       ( RepoInfo(repoRootDir, repoStateDir, repoSnapshotsDir)
       , runRepo
       , Repo
       ) where

import Control.Applicative ((<$>))
import Control.Exception (evaluate, finally)
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Control.Monad.Trans (MonadIO)

import Data.Maybe (fromMaybe)

import Git.Libgit2 (lgFactory)

import System.Directory (getCurrentDirectory, canonicalizePath,
                         doesDirectoryExist)
import System.FileLock (SharedExclusive(Exclusive), tryLockFile, unlockFile)
import System.FilePath (combine, takeDirectory)

import Repo.Git (WithFactory, withFactory)
import Repo.Utils (mustDir, io)

data RepoInfo = RepoInfo { repoRootDir :: FilePath
                         , repoStateDir :: FilePath
                         , repoSnapshotsDir :: FilePath
                         }

newtype Repo a = Repo { unRepo :: ReaderT RepoInfo IO a }
               deriving (Monad, MonadReader RepoInfo, MonadIO, Functor)

runRepo :: MonadIO m => (forall n r . WithFactory n r => Repo a) -> m a
runRepo r = withFactory lgFactory $ do
  root <- io findRootDir
  state <- mustStateDir root
  snapshots <- mustSnapshotsDir state

  let info = RepoInfo { repoRootDir = root
                      , repoStateDir = state
                      , repoSnapshotsDir = snapshots
                      }

  io $ withLock state $ runReaderT (unRepo r) info

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

        mustStateDir rootDir = do
          mustDir stateDir
          return stateDir

          where stateDir = combine rootDir ".repo-utils"

        mustSnapshotsDir stateDir = do
          mustDir snapshotsDir
          return snapshotsDir

          where snapshotsDir = (combine stateDir "snapshots")

        withLock stateDir f = do
          lock <- evaluate =<< extractLock <$> tryLockFile lockPath Exclusive
          f `finally` unlockFile lock
          where lockPath = combine stateDir "lock"
                extractLock =
                  fromMaybe (error $ "could not acquire file lock at " ++ lockPath)

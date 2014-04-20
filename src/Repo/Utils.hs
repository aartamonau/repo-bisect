module Repo.Utils
       ( mustDir
       , mustFile
       , io
       ) where

import Control.Monad (when, unless)
import Control.Monad.Trans (MonadIO, liftIO)

import System.Directory (doesDirectoryExist, doesFileExist, createDirectory)

mustDir :: MonadIO m => FilePath -> m ()
mustDir dir = io $ do
  exists <- doesDirectoryExist dir
  if exists
    then do
      fileExists <- doesFileExist dir
      when fileExists $
        error $ "failed to create state directory (" ++ dir ++ "): file exists"
    else createDirectory dir

mustFile :: MonadIO m => FilePath -> m ()
mustFile path = io $ do
  exists <- doesFileExist path
  unless exists $
    error $ "could not find " ++ path

io :: MonadIO m => IO a -> m a
io = liftIO

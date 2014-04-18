module Repo.Utils
       ( mustDir
       , io
       ) where

import Control.Monad (when, unless)
import Control.Monad.Trans (MonadIO, liftIO)

import System.Directory (doesDirectoryExist, doesFileExist, createDirectory)

mustDir :: FilePath -> IO ()
mustDir dir = do
  exists <- doesDirectoryExist dir
  unless exists $ do
    fileExists <- doesFileExist dir
    when fileExists $
      error $ "failed to create state directory (" ++ dir ++ "): file exists"
  createDirectory dir

io :: MonadIO m => IO a -> m a
io = liftIO

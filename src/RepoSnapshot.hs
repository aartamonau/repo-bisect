{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Exception (finally, evaluate)
import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class (liftIO)

import Data.Default (def)
import Data.Maybe (fromMaybe)

import System.Directory (getCurrentDirectory, canonicalizePath,
                         doesDirectoryExist, doesFileExist, createDirectory)
import System.FilePath (combine, takeDirectory)

import System.FileLock (SharedExclusive(Exclusive), tryLockFile, unlockFile)

import Git (withRepository, resolveReference)
import Git.Libgit2 (OidPtr, lgFactory)

import UI.Command (Application(appName, appVersion, appAuthors, appProject,
                               appCmds, appShortDesc, appLongDesc,
                               appCategories, appBugEmail),
                   Command(cmdName, cmdHandler, cmdShortDesc, cmdCategory),

                   appMain, defCmd)

repoDirName :: FilePath
repoDirName = ".repo"

findRootDir :: IO FilePath
findRootDir = go =<< canonicalizePath =<< getCurrentDirectory
  where go :: FilePath -> IO FilePath
        go dir = do
          exists <- doesDirectoryExist $ combine dir repoDirName
          if exists
            then return dir
            else do
              let parentDir = takeDirectory dir
              when (parentDir == dir) $
                error "could not find .repo directory"
              go parentDir

stateDirName :: FilePath
stateDirName = ".repo-utils"

createStateDir :: FilePath -> IO FilePath
createStateDir root = do
  exists <- doesDirectoryExist stateDir
  unless exists $ do
    fileExists <- doesFileExist stateDir
    when fileExists $
      error $ "failed to create state directory (" ++ stateDir ++ "): file exists"
    createDirectory stateDir
  return stateDir
  where stateDir = combine root stateDirName

withLock :: FilePath -> IO a -> IO a
withLock stateDir f = do
  lock <- evaluate =<< extractLock <$> tryLockFile lockPath Exclusive
  f `finally` unlockFile lock
  where lockPath = combine stateDir "lock"
        extractLock = fromMaybe (error $ "could not acquire file lock at " ++ lockPath)

data Project = Project { name :: String
                       , path :: FilePath
                       }
             deriving Show

readProjects :: FilePath -> IO [Project]
readProjects rootDir = map toProject . lines <$> readFile projectsPath
  where projectsPath = combine rootDir ".repo/project.list"
        toProject name = Project { name = name
                                 , path = (combine rootDir name)
                                 }

readProjectHead :: Project -> IO OidPtr
readProjectHead Project {path} =
  withRepository lgFactory path $ do
    ref <- resolveReference "HEAD"
    return $ fromMaybe (error $ "could not read HEAD of " ++ path) ref

app :: Application () ()
app = def { appName = "repo-snapshot"
          , appVersion = "0.1"
          , appAuthors = ["Aliaksey Artamonau <aliaksiej.artamonau@gmail.com>"]
          , appShortDesc = "short description"
          , appLongDesc = "long description"
          , appProject = "repo-utils"
          , appCategories = ["foo"]
          , appCmds = [foo]
          , appBugEmail = "aliaksiej.artamonau@gmail.com"
          }

foo :: Command ()
foo = defCmd { cmdName = "foo"
             , cmdHandler = liftIO $ fooHandler
             , cmdShortDesc = "foo short desc"
             , cmdCategory = "foo"
             }

fooHandler :: IO ()
fooHandler = do
  root <- findRootDir
  state <- createStateDir root

  withLock state $ do
    putStrLn $ "root directory: " ++ root
    putStrLn $ "state directory: " ++ state
    projects <- readProjects root

    putStrLn "projects: "
    forM_ projects $ \p@(Project {name, path}) -> do
      head <- readProjectHead p
      putStrLn $ name ++ ": " ++ path ++ " => " ++ show head

    threadDelay 10000000


main :: IO ()
main = appMain app

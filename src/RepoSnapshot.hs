{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Exception (finally, evaluate)
import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Default (def)
import Data.Maybe (fromMaybe)

import Data.Text (Text)
import qualified Data.Text as Text

import System.Directory (getCurrentDirectory, canonicalizePath,
                         doesDirectoryExist, doesFileExist, createDirectory)
import System.FilePath (combine, takeDirectory)

import System.FileLock (SharedExclusive(Exclusive), tryLockFile, unlockFile)

import Git (MonadGit, RefTarget(RefObj, RefSymbolic),
            withRepository, lookupReference)
import Git.Libgit2 (LgRepo, lgFactory)

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
createStateDir rootDir = do
  exists <- doesDirectoryExist stateDir
  unless exists $ do
    fileExists <- doesFileExist stateDir
    when fileExists $
      error $ "failed to create state directory (" ++ stateDir ++ "): file exists"
    createDirectory stateDir
  return stateDir
  where stateDir = combine rootDir stateDirName

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

readProjectHead :: Project -> IO (RefTarget LgRepo)
readProjectHead Project {path} =
  withRepository lgFactory path $ do
    ref <- lookupReference "HEAD"
    return $ fromMaybe (error $ "could not resolve HEAD of " ++ path) ref

type ShellRef = Text

shellRef :: RefTarget LgRepo -> ShellRef
shellRef (RefObj obj) = Text.pack $ show obj
shellRef (RefSymbolic sym) = decodeBranch sym
  where decodeBranch s | Just branch <- Text.stripPrefix "refs/heads/" s = branch
                       | otherwise = s

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
  rootDir <- findRootDir
  state <- createStateDir rootDir

  withLock state $ do
    putStrLn $ "root directory: " ++ rootDir
    putStrLn $ "state directory: " ++ state
    projects <- readProjects rootDir

    putStrLn "projects: "
    forM_ projects $ \p@(Project {name, path}) -> do
      head <- readProjectHead p
      putStrLn $ name ++ ": " ++ path ++ " => " ++ Text.unpack (shellRef head)

    threadDelay 10000000


main :: IO ()
main = appMain app

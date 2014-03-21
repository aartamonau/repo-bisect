{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Exception (finally, evaluate)
import Control.Monad (forM, forM_, when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Default (def)
import Data.List (find)
import Data.Maybe (fromMaybe, fromJust)
import Data.String (fromString)
import Data.Tagged (Tagged(Tagged))

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Data.Time (UTCTime, zonedTimeToUTC)
import Data.Time.Git (io_approxidate, posixToUTC)

import System.Directory (getCurrentDirectory, canonicalizePath,
                         doesDirectoryExist, doesFileExist, createDirectory)
import System.FilePath (combine, takeDirectory)

import System.FileLock (SharedExclusive(Exclusive), tryLockFile, unlockFile)

import Git (MonadGit(lookupCommit),
            Commit(commitParents, commitOid, commitCommitter),
            Signature(signatureWhen),
            RefTarget(RefObj, RefSymbolic), Oid, CommitOid,
            RepositoryFactory, IsOid,
            withRepository, lookupReference, renderOid, referenceToOid)
import Git.Libgit2 (lgFactory)

import Shelly (cd, run_, shelly, silently)

import UI.Command (Application(appName, appVersion, appAuthors, appProject,
                               appCmds, appShortDesc, appLongDesc,
                               appCategories, appBugEmail),
                   Command(cmdName, cmdHandler, cmdShortDesc, cmdCategory),
                   appMain, defCmd)

import Control.Monad.Trans.Control (MonadBaseControl)

type FactoryConstraints n r =
  (MonadGit r n, MonadBaseControl IO n, IsOid (Oid r))

type RepoFactory n r = RepositoryFactory n IO r

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

data Project = Project { name :: Text
                       , path :: FilePath
                       }
             deriving Show

withProject :: (FactoryConstraints n r, ?factory :: RepoFactory n r)
            => Project -> n a -> IO a
withProject proj = withRepository ?factory (path proj)

readProjects :: FilePath -> IO [Project]
readProjects rootDir = map toProject . lines <$> readFile projectsPath
  where projectsPath = combine rootDir ".repo/project.list"
        toProject name = Project { name = Text.pack name
                                 , path = combine rootDir name
                                 }

readProjectHead :: (FactoryConstraints n r, ?factory :: RepoFactory n r)
                => Project -> IO (RefTarget r)
readProjectHead proj =
  withProject proj $ do
    ref <- lookupReference "HEAD"
    return $ fromMaybe (error $ "could not resolve HEAD of " ++ path proj) ref

renderRef :: (IsOid (Oid r)) => RefTarget r -> Text
renderRef (RefObj obj) = renderOid obj
renderRef (RefSymbolic sym) = sym

headsPath :: FilePath -> FilePath
headsPath stateDir = combine stateDir "HEADS"

saveSnapshot :: IsOid (Oid r) => FilePath -> [(Project, RefTarget r)] -> IO ()
saveSnapshot path projects =
  Text.writeFile path $ Text.concat (map oneLine projects)

  where oneLine (Project {name}, ref) =
          Text.concat [renderRef ref, " ", name, "\n"]

readSnapshot :: FilePath -> [Project] -> IO [(Project, Text)]
readSnapshot path projects = do
  content <- Text.readFile path

  forM (map decodeLine $ Text.lines content) $ \(ref, name) ->
    return (findProject name, ref)

  where decodeLine s
          | (ref, rest) <- Text.breakOn " " s = (ref, Text.tail rest)
          | otherwise = error $ "got invalid line in snapshot file "
                                  ++ path ++ " : " ++ Text.unpack s

        findProject needle
          | Just proj <- find p projects = proj
          | otherwise = error $ "got invalid project name in snapshot file "
                                  ++ path ++ " : " ++ Text.unpack needle
          where p proj = name proj == needle

saveHeads :: (FactoryConstraints n r, ?factory :: RepoFactory n r)
          => FilePath -> [Project] -> IO ()
saveHeads stateDir projects = do
  heads <- liftIO $ mapM readProjectHead projects
  saveSnapshot (headsPath stateDir) (zip projects heads)

readHeads :: FilePath -> [Project] -> IO [(Project, Text)]
readHeads stateDir projects = readSnapshot (headsPath stateDir) projects

checkout :: Project -> Text -> IO ()
checkout (Project {path}) ref =
  shelly $ silently $ do
    cd $ fromString path
    run_ "git" ["checkout", branchify ref]

  where branchify s | Just branch <- Text.stripPrefix "refs/heads/" s = branch
                    | otherwise = s

oidToCommitOid :: Oid r -> CommitOid r
oidToCommitOid = Tagged

findCommit :: (FactoryConstraints n r, ?factory :: RepoFactory n r) =>
              Project -> RefTarget r -> (Commit r -> Bool) -> IO (Maybe (Commit r))
findCommit proj head p =
  withProject proj $ do
    headCommitOid <- oidToCommitOid <$> resolve head
    go headCommitOid

  where resolve ref = do
          maybeOid <- referenceToOid ref
          return $
            fromMaybe (error $ "could not resolve "
                       ++ Text.unpack (renderRef ref) ++ " at " ++ path proj) maybeOid

        go cid = do
          commit <- lookupCommit cid
          if p commit
            then return $ Just commit
            else
              case commitParents commit of
                [] -> return Nothing
                (first : _) -> go first

findCommitByDate :: (FactoryConstraints n r, ?factory :: RepoFactory n r)
                 => Project -> RefTarget r -> UTCTime
                 -> IO (Maybe (Commit r))
findCommitByDate proj head date = findCommit proj head p
  where p commit = zonedTimeToUTC (signatureWhen committer) <= date
          where committer = commitCommitter commit

app :: (FactoryConstraints n r, ?factory :: RepoFactory n r)
    => Application () ()
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

foo :: (FactoryConstraints n r, ?factory :: RepoFactory n r)
    => Command ()
foo = defCmd { cmdName = "foo"
             , cmdHandler = liftIO $ fooHandler
             , cmdShortDesc = "foo short desc"
             , cmdCategory = "foo"
             }

fooHandler :: forall n r . (FactoryConstraints n r, ?factory :: RepoFactory n r)
           => IO ()
fooHandler = do
  rootDir <- findRootDir
  stateDir <- createStateDir rootDir

  yearAgo <- posixToUTC . fromJust <$> io_approxidate "one year ago"

  withLock stateDir $ do
    putStrLn $ "root directory: " ++ rootDir
    putStrLn $ "state directory: " ++ stateDir
    projects <- readProjects rootDir

    saveHeads stateDir projects
    heads <- readHeads stateDir projects

    putStrLn "projects: "
    forM_ heads $ \(p@(Project {name, path}), head) -> do
      putStrLn $ Text.unpack name ++ ": " ++
        path ++ " => " ++ Text.unpack head

      headRef <- readProjectHead p

      maybeCommitOid <- (fmap commitOid) <$> findCommitByDate p headRef yearAgo

      putStrLn $ "year ago commit => " ++ show maybeCommitOid
      putStrLn ""

      checkout p head

    threadDelay 10000000

main :: IO ()
main = appMain app
  where ?factory = lgFactory

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- to give a Show instance to Snapshot r
{-# LANGUAGE UndecidableInstances #-}

import Control.Arrow (second)
import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Exception (finally, evaluate, catch)
import Control.Monad (forM, forM_, when, unless, filterM)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Default (def)
import Data.List (find)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.String (fromString)
import Data.Tagged (Tagged(Tagged))

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Data.Time (UTCTime, zonedTimeToUTC)
import Data.Time.Git (io_approxidate, posixToUTC)

import System.Directory (getCurrentDirectory, canonicalizePath,
                         doesDirectoryExist, doesFileExist, createDirectory,
                         getDirectoryContents)
import System.FilePath (combine, takeDirectory)

import System.FileLock (SharedExclusive(Exclusive), tryLockFile, unlockFile)

import Git (MonadGit(lookupCommit, lookupReference),
            Commit(commitParents, commitOid, commitCommitter),
            Signature(signatureWhen),
            RefTarget(RefObj, RefSymbolic), Oid, CommitOid,
            RepositoryFactory, IsOid,
            GitException,
            withRepository, parseOid, renderOid,
            referenceToOid, commitRefTarget)
import Git.Libgit2 (lgFactory)

import Shelly (cd, run_, shelly, silently)

import UI.Command (Application(appName, appVersion, appAuthors, appProject,
                               appCmds, appShortDesc, appLongDesc,
                               appCategories, appBugEmail),
                   App,
                   Command(cmdName, cmdHandler, cmdShortDesc, cmdCategory),
                   appMain, defCmd, appArgs)

import Control.Monad.Trans.Control (MonadBaseControl, control)

type FactoryConstraints n r = (MonadGit r n, MonadBaseControl IO n)

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

snapshotsDir :: FilePath -> FilePath
snapshotsDir stateDir = combine stateDir "snapshots"

mustDir :: FilePath -> IO ()
mustDir dir = do
  exists <- doesDirectoryExist dir
  unless exists $ do
    fileExists <- doesFileExist dir
    when fileExists $
      error $ "failed to create state directory (" ++ dir ++ "): file exists"
    createDirectory dir

mustFile :: FilePath -> IO ()
mustFile path = do
  exists <- doesFileExist path
  unless exists $
    error $ "could not find " ++ path

mustStateDir :: FilePath -> IO FilePath
mustStateDir rootDir = do
  mustDir stateDir
  mustDir (snapshotsDir stateDir)
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

onGitException :: MonadBaseControl IO m => m a -> m a -> m a
onGitException body what =
  control $ \run -> run body `catch` \ (_ ::GitException) -> run what

parseRef :: (FactoryConstraints n r, ?factory :: RepoFactory n r)
         => Project -> Text -> IO (RefTarget r)
parseRef proj ref =
  withProject proj $ do
    shaRef <- trySHA
    finalRef <- maybe (trySymName) (return . Just) shaRef

    return $ fromMaybe
      (error $ "could not resolve " ++ Text.unpack ref
               ++ " at " ++ path proj) finalRef

  where trySHA =
          (do oid <- parseOid ref
              _ <- lookupCommit (oidToCommitOid oid)
              return $ Just (RefObj oid))
          `onGitException` return Nothing

        trySymName =
          lookupReference ref `onGitException` return Nothing

headsPath :: FilePath -> FilePath
headsPath stateDir = combine stateDir "HEADS"

newtype Snapshot r = Snapshot { unSnapshot :: [(Project, RefTarget r)] }
type PartialSnapshot r = [(Project, Maybe (RefTarget r))]

instance IsOid (Oid r) => Show (Snapshot r) where
  show = concatMap showPair . unSnapshot
    where showPair (proj, ref) =
            Text.unpack (name proj)
              ++ " => " ++ Text.unpack (renderRef ref) ++ "\n"

toFullSnapshot :: PartialSnapshot r -> Snapshot r
toFullSnapshot = Snapshot . map (second fromJust) . filter (isJust . snd)

saveSnapshot :: IsOid (Oid r) => FilePath -> Snapshot r -> IO ()
saveSnapshot path projects =
  Text.writeFile path $ Text.concat (map oneLine $ unSnapshot projects)

  where oneLine (Project {name}, ref) =
          Text.concat [renderRef ref, " ", name, "\n"]

readSnapshot :: (FactoryConstraints n r, ?factory :: RepoFactory n r)
             => FilePath -> [Project] -> IO (Snapshot r)
readSnapshot path projects = do
  content <- Text.readFile path

  fmap Snapshot $ forM (map decodeLine $ Text.lines content) $ \(ref, name) -> do
    let proj = findProject name
    ref' <- parseRef proj ref
    return (proj, ref')

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
  saveSnapshot (headsPath stateDir) (Snapshot $ zip projects heads)

readHeads :: (FactoryConstraints n r, ?factory :: RepoFactory n r)
          => FilePath -> [Project] -> IO (Snapshot r)
readHeads stateDir projects = readSnapshot (headsPath stateDir) projects

checkoutRef :: IsOid (Oid r) => Project -> RefTarget r -> IO ()
checkoutRef (Project {path}) ref =
  shelly $ silently $ do
    cd $ fromString path
    run_ "git" ["checkout", branchify (renderRef ref)]

  where branchify s | Just branch <- Text.stripPrefix "refs/heads/" s = branch
                    | otherwise = s

oidToCommitOid :: Oid r -> CommitOid r
oidToCommitOid = Tagged

findCommit :: (FactoryConstraints n r, ?factory :: RepoFactory n r)
           => Project -> RefTarget r -> (Commit r -> Bool) -> IO (Maybe (Commit r))
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

snapshotByDate :: (FactoryConstraints n r, ?factory :: RepoFactory n r)
               => Snapshot r -> UTCTime -> IO (PartialSnapshot r)
snapshotByDate heads date =
  forM (unSnapshot heads) $ \(p, head) -> do
    commit <- findCommitByDate p head date
    return (p, commitRefTarget <$> commit)

getSnapshots :: FilePath -> IO [String]
getSnapshots stateDir =
  filterM (doesFileExist . combine dir) =<< getDirectoryContents dir
  where dir = snapshotsDir stateDir

mainCategory :: String
mainCategory = "Working with snapshots"

app :: (FactoryConstraints n r, ?factory :: RepoFactory n r)
    => Application () ()
app = def { appName = "repo-snapshot"
          , appVersion = "0.1"
          , appAuthors = ["Aliaksey Artamonau <aliaksiej.artamonau@gmail.com>"]
          , appShortDesc = "short description"
          , appLongDesc = "long description"
          , appProject = "repo-utils"
          , appCategories = ["foo", mainCategory]
          , appCmds = [list, checkout, foo]
          , appBugEmail = "aliaksiej.artamonau@gmail.com"
          }

list :: Command ()
list = defCmd { cmdName = "list"
              , cmdHandler = liftIO $ listHandler
              , cmdCategory = mainCategory
              , cmdShortDesc = "List known snapshot"
              }

listHandler :: IO ()
listHandler = do
  rootDir <- findRootDir
  stateDir <- mustStateDir rootDir
  mapM_ putStrLn =<< getSnapshots stateDir

checkout :: (FactoryConstraints n r, ?factory :: RepoFactory n r) => Command ()
checkout = defCmd { cmdName = "checkout"
                  , cmdHandler = checkoutHandler
                  , cmdShortDesc = "checkout snapshot by name or date"
                  , cmdCategory = mainCategory
                  }

checkoutHandler :: (FactoryConstraints n r, ?factory :: RepoFactory n r)
                => App () ()
checkoutHandler = do
  args <- appArgs
  case args of
    [refOrDate] ->
      go refOrDate
    _ ->
      error "bad arguments"

  where go refOrDate = do
          snapshots <- liftIO $ do
            stateDir <- mustStateDir =<< findRootDir
            getSnapshots stateDir

          if refOrDate `elem` snapshots
            then checkoutSnapshot refOrDate
            else checkoutDate refOrDate

        checkoutSnapshot snapshot =
          liftIO $ putStrLn $ "checking out snapshot " ++ snapshot

        checkoutDate date =
          liftIO $ putStrLn $ "checking out date " ++ date

foo :: (FactoryConstraints n r, ?factory :: RepoFactory n r) => Command ()
foo = defCmd { cmdName = "foo"
             , cmdHandler = liftIO $ fooHandler
             , cmdShortDesc = "foo short desc"
             , cmdCategory = "foo"
             }

fooHandler :: forall n r . (FactoryConstraints n r, ?factory :: RepoFactory n r)
           => IO ()
fooHandler = do
  rootDir <- findRootDir
  stateDir <- mustStateDir rootDir

  yearAgo <- posixToUTC . fromJust <$> io_approxidate "one year ago"
  monthAgo <- posixToUTC . fromJust <$> io_approxidate "one month ago"

  withLock stateDir $ do
    putStrLn $ "root directory: " ++ rootDir
    putStrLn $ "state directory: " ++ stateDir
    projects <- readProjects rootDir

    saveHeads stateDir projects
    heads <- readHeads stateDir projects

    putStrLn "projects: "
    forM_ (unSnapshot heads) $ \(p@(Project {name, path}), head) -> do
      putStrLn $ Text.unpack name ++ ": " ++
        path ++ " => " ++ Text.unpack (renderRef head)

      maybeCommitOid <- (fmap commitOid) <$> findCommitByDate p head yearAgo

      putStrLn $ "year ago commit => " ++ show maybeCommitOid
      putStrLn ""

      checkoutRef p head

    putStrLn "=================================================="

    yearAgoSnapshot <- toFullSnapshot <$> snapshotByDate heads yearAgo
    putStrLn $ "one year old snapshot:\n" ++ show yearAgoSnapshot

    putStrLn "=================================================="

    monthAgoSnapshot <- toFullSnapshot <$> snapshotByDate heads monthAgo
    putStrLn $ "one month old snapshot:\n" ++ show monthAgoSnapshot

    threadDelay 10000000

main :: IO ()
main = appMain app
  where ?factory = lgFactory

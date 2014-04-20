{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}

import Control.Arrow (second)
import Control.Applicative ((<$>))
import Control.Exception (catch, onException)
import Control.Monad (forM, forM_, zipWithM_, when, unless, filterM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks)

import Data.Default (Default(def))
import Data.List (find)
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import Data.String (fromString)
import Data.Tagged (Tagged(Tagged))

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Data.Time (UTCTime, zonedTimeToUTC)
import Data.Time.Git (io_approxidate, posixToUTC)

import System.Directory (doesFileExist, getDirectoryContents, removeFile)
import System.FilePath (combine)

import System.IO (stderr, hPutStrLn)

import Text.RawString.QQ (r)
import Text.Regex.Posix ((=~))

import Text.XML.Light (showTopElement)

import Git (MonadGit,
            Commit(commitCommitter),
            Signature(signatureWhen),
            RefTarget(RefObj), Oid, CommitOid,
            IsOid,
            GitException,
            commitRefTarget)
import Git.Libgit2 (lgFactory)

import Shelly (cd, run_, shelly, silently)

import System.Console.GetOpt (OptDescr(Option), ArgDescr(NoArg))
import UI.Command (Application(appName, appVersion, appAuthors, appProject,
                               appCmds, appShortDesc, appLongDesc,
                               appCategories, appBugEmail,
                               appOptions, appProcessConfig),
                   App,
                   Command(cmdName, cmdHandler, cmdShortDesc, cmdCategory),
                   appMainWithOptions, defCmd, appArgs, appConfig)

import Control.Monad.Trans.Control (MonadBaseControl, control)

import Repo (Project(Project, projectName, projectPath),
             Snapshot(Snapshot, unSnapshot),
             Repo, runRepo, repoStateDir, repoSnapshotsDir,
             Project, getProjectHead, findCommit, withProject,
             WithFactory, Gitty, GitFactory,
             parseRef, resolveRef, renderRef,
             readManifest, readManifest_, snapshotManifest)
import Repo.Utils (io, mustFile)

-- TODO: something better than this
warn :: String -> IO ()
warn = hPutStrLn stderr . ("Warning: " ++)

getHeadsSnapshot :: WithFactory n r => [Project] -> Repo (Snapshot r)
getHeadsSnapshot projects =
  Snapshot . zip projects <$> mapM getProjectHead projects

onGitException :: MonadBaseControl IO m => m a -> m a -> m a
onGitException body what =
  control $ \run -> run body `catch` \ (_ :: GitException) -> run what

resolveSnapshot :: WithFactory n r => Snapshot r -> Repo (Snapshot r)
resolveSnapshot = fmap Snapshot . mapM f . unSnapshot
  where f (p, ref) = fmap (p,) (withProject p $ RefObj <$> resolveRef ref)

headsPath :: FilePath -> FilePath
headsPath stateDir = combine stateDir "HEADS"

type PartialSnapshot r = [(Project, Maybe (RefTarget r))]

renderSnapshot :: IsOid (Oid r) => Snapshot r -> Text
renderSnapshot (Snapshot ps) = Text.intercalate "\n" (map renderPair ps)
  where width = maximum $ map (Text.length . projectName . fst) ps
        renderPair (proj, ref) = Text.concat [projectName proj,
                                              Text.replicate (width - n) " ",
                                              " ",
                                              renderRef ref]
          where n = Text.length $ projectName proj

toFullSnapshot :: PartialSnapshot r -> Snapshot r
toFullSnapshot = Snapshot . map (second fromJust) . filter (isJust . snd)

snapshotProjects :: Snapshot r -> [Project]
snapshotProjects = fst . unzip . unSnapshot

saveSnapshotFile :: IsOid (Oid r) => FilePath -> Snapshot r -> IO ()
saveSnapshotFile path projects =
  Text.writeFile path $ Text.concat (map oneLine $ unSnapshot projects)

  where oneLine (Project {projectName}, ref) =
          Text.concat [renderRef ref, " ", projectName, "\n"]

readSnapshotFile :: WithFactory n r => FilePath -> [Project] -> Repo (Snapshot r)
readSnapshotFile path projects = do
  io $ mustFile path
  content <- io $ Text.readFile path

  fmap Snapshot $ forM (map decodeLine $ Text.lines content) $ \(ref, name) -> do
    let proj = findProject name
    ref' <- withProject proj $ parseRef ref
    return (proj, ref')

  where decodeLine s
          | (ref, rest) <- Text.breakOn " " s = (ref, Text.tail rest)
          | otherwise = error $ "got invalid line in snapshot file "
                                  ++ path ++ " : " ++ Text.unpack s

        findProject needle
          | Just proj <- find p projects = proj
          | otherwise = error $ "got invalid project name in snapshot file "
                                  ++ path ++ " : " ++ Text.unpack needle
          where p proj = projectName proj == needle

readSnapshotByName :: WithFactory n r
                   => FilePath -> String -> [Project] -> Repo (Snapshot r)
readSnapshotByName snapshotsDir = readSnapshotFile . combine snapshotsDir

saveSnapshotByName :: IsOid (Oid r) => FilePath -> String -> Snapshot r -> IO ()
saveSnapshotByName snapshotsDir = saveSnapshotFile . combine snapshotsDir

removeSnapshotByName :: FilePath -> String -> IO ()
removeSnapshotByName snapshotsDir = removeFile . combine snapshotsDir

checkoutRef :: IsOid (Oid r) => Project -> RefTarget r -> IO ()
checkoutRef (Project {projectPath}) ref =
  shelly $ silently $ do
    cd $ fromString projectPath
    run_ "git" ["checkout", branchify (renderRef ref)]

  where branchify s | Just branch <- Text.stripPrefix "refs/heads/" s = branch
                    | otherwise = s

checkoutSnapshot :: IsOid (Oid r) => Snapshot r -> IO ()
checkoutSnapshot = uncurry (zipWithM_ checkoutRef) . unzip . unSnapshot

tryCheckoutSnapshot :: WithFactory n r => Snapshot r -> Repo ()
tryCheckoutSnapshot snapshot = do
  heads <- getHeadsSnapshot (snapshotProjects snapshot)
  io $ checkoutSnapshot snapshot `onException` tryRecover heads

  where tryRecover heads = do
          warn "checkout failed; trying to roll back to previous state"
          checkoutSnapshot heads

oidToCommitOid :: Oid r -> CommitOid r
oidToCommitOid = Tagged

findCommitByDate :: WithFactory n r
                 => Project -> RefTarget r -> UTCTime -> Repo (Maybe (Commit r))
findCommitByDate proj head date = findCommit proj head p
  where p commit = zonedTimeToUTC (signatureWhen committer) <= date
          where committer = commitCommitter commit

snapshotByDate :: WithFactory n r
               => Snapshot r -> UTCTime -> Repo (PartialSnapshot r)
snapshotByDate heads date =
  forM (unSnapshot heads) $ \(p, head) -> do
    commit <- findCommitByDate p head date
    return (p, commitRefTarget <$> commit)

getSnapshots :: FilePath -> IO [String]
getSnapshots snapshotsDir =
  filterM (doesFileExist . combine snapshotsDir)
    =<< getDirectoryContents snapshotsDir

mustParseDate :: String -> IO UTCTime
mustParseDate date = do
  parsed <- fmap posixToUTC <$> io_approxidate date
  return $ fromMaybe (error $ "can't recognize date \"" ++ date ++ "\"") parsed

isValidSnapshotName :: String -> Bool
isValidSnapshotName name = not $ name =~ regexp
  where regexp :: String
        regexp = [r|^\.|\.\.|[\/:~^[:cntrl:][:space:]]|]

withFactory :: Gitty n r
            => GitFactory n r
            -> (WithFactory n r => a) -> a
withFactory factory x = let ?factory = factory in x

mainCategory :: String
mainCategory = "Working with snapshots"

data Options = Options { forceDate :: Bool
                       , overwriteSnapshot :: Bool
                       , resolveRefNames :: Bool
                       }

instance Default Options where
  def = Options { forceDate = False
                , overwriteSnapshot = False
                , resolveRefNames = False
                }

app :: WithFactory n r => Application (Options -> Options) Options
app = def' { appName = "repo-snapshot"
           , appVersion = "0.1"
           , appAuthors = ["Aliaksey Artamonau <aliaksiej.artamonau@gmail.com>"]
           , appShortDesc = "short description"
           , appLongDesc = "long description"
           , appProject = "repo-utils"
           , appCategories = ["foo", mainCategory]
           , appCmds = [listCmd, checkoutCmd, saveCmd,
                        deleteCmd, showCmd, exportCmd]
           , appBugEmail = "aliaksiej.artamonau@gmail.com"
           , appOptions = options
           , appProcessConfig = processConfig
           }
  where processConfig :: Options -> [Options -> Options] -> IO Options
        processConfig z = return . foldl (flip ($)) z

        def' :: Application (Options -> Options) Options
        def' = def

options :: [OptDescr (Options -> Options)]
options = [ forceDateOpt
          , overwriteSnapshot
          , resolveRefNames
          ]
  where forceDateOpt = Option "d" ["--force-date"]
                              (NoArg $ \opts -> opts { forceDate = True })
                              "interpret command argument as a date"
        overwriteSnapshot = Option "F" ["--overwrite-snapshot"]
                              (NoArg $ \opts -> opts { overwriteSnapshot = True })
                              "overwrite snapshot if it already exists"
        resolveRefNames = Option "r" ["--resolve-refnames"]
                              (NoArg $ \opts -> opts { resolveRefNames = True })
                              "resolve reference names before saving snapshot"

argsExistingSnapshot :: App Options String
argsExistingSnapshot = do
  args <- appArgs

  case args of
    [name] ->
      runRepo $ do
        snapshotsDir <- asks repoSnapshotsDir

        liftIO $ do
          snapshots <- getSnapshots snapshotsDir

          unless (name `elem` snapshots) $
            error $ "unknown snapshot '" ++ name ++ "'"

          return name
    _ ->
      -- TODO: would be helpful to be able to show help here
      error "bad arguments"

listCmd :: Command Options
listCmd = defCmd { cmdName = "list"
                 , cmdHandler = liftIO listHandler
                 , cmdCategory = mainCategory
                 , cmdShortDesc = "List known snapshot"
                 }

listHandler :: IO ()
listHandler = runRepo $ do
  snapshotsDir <- asks repoSnapshotsDir

  liftIO $
    mapM_ putStrLn =<< getSnapshots snapshotsDir

checkoutCmd :: WithFactory n r => Command Options
checkoutCmd = defCmd { cmdName = "checkout"
                     , cmdHandler = checkoutHandler
                     , cmdShortDesc = "Checkout snapshot by name or date"
                     , cmdCategory = mainCategory
                     }

checkoutHandler :: WithFactory n r => App Options ()
checkoutHandler = do
  args <- appArgs
  options <- appConfig
  runRepo $ do
    stateDir <- asks repoStateDir
    snapshotsDir <- asks repoSnapshotsDir
    manifest <- readManifest

    snapshots <- io $ getSnapshots snapshotsDir

    case parseArgs args options snapshots of
      Left date ->
        handleDate manifest date
      Right snapshot ->
        handleSnapshot stateDir manifest snapshot

  where parseArgs args options snapshots
          | forceDate options = Left snapshotOrDate
          | snapshotOrDate `elem` snapshots = Right snapshotOrDate
          | otherwise = Left snapshotOrDate
          where snapshotOrDate = unwords args

        handleSnapshot snapshotsDir manifest snapshot =
              tryCheckoutSnapshot =<< readSnapshotByName snapshotsDir snapshot projects
          where projects = snapshotProjects manifest

        handleDate manifest date = do
          partialSnapshot <- snapshotByDate manifest =<< io (mustParseDate date)
          forM_ partialSnapshot $ \(Project{projectName}, ref) ->
            when (isNothing ref) $
              io $ warn $ "couldn't find a commit matching the date in "
                            ++ Text.unpack projectName

          tryCheckoutSnapshot (toFullSnapshot partialSnapshot)

saveCmd :: WithFactory n r => Command Options
saveCmd = defCmd { cmdName = "save"
                 , cmdHandler = saveHandler
                 , cmdShortDesc = "save current state of all projects"
                 , cmdCategory = mainCategory
                 }

saveHandler :: WithFactory n r => App Options ()
saveHandler = do
  args <- appArgs
  options <- appConfig

  case args of
    [name] ->
      runRepo $ do
        snapshotsDir <- asks repoSnapshotsDir
        projects <- snapshotProjects <$> readManifest

        snapshots <- io $ getSnapshots snapshotsDir

        unless (isValidSnapshotName name) $
          error $ "invalid snapshot name '" ++ name ++ "'"

        when (name `elem` snapshots && not (overwriteSnapshot options)) $
          error $ "snapshot '" ++ name ++ "' already exists"

        heads <- getHeadsSnapshot projects >>= \hs ->
          if resolveRefNames options
            then resolveSnapshot hs
            else return hs

        io $ saveSnapshotByName snapshotsDir name heads
    _ ->
      -- TODO: would be helpful to be able to show help here
      error "bad arguments"

deleteCmd :: WithFactory n r => Command Options
deleteCmd = defCmd { cmdName = "delete"
                   , cmdHandler = deleteHandler
                   , cmdShortDesc = "delete named snapshot"
                   , cmdCategory = mainCategory
                   }

deleteHandler :: WithFactory n r => App Options ()
deleteHandler = do
  name <- argsExistingSnapshot

  runRepo $ do
    snapshotsDir <- asks repoStateDir

    liftIO $
      removeSnapshotByName snapshotsDir name

showCmd :: WithFactory n r => Command Options
showCmd = defCmd { cmdName = "show"
                 , cmdHandler = showHandler
                 , cmdShortDesc = "show snapshot"
                 , cmdCategory = mainCategory
                 }

showHandler :: WithFactory n r => App Options ()
showHandler = do
  name <- argsExistingSnapshot

  runRepo $ do
    snapshotsDir <- asks repoSnapshotsDir
    projects <- snapshotProjects <$> readManifest
    snapshot <- readSnapshotByName snapshotsDir name projects

    io $ Text.putStrLn $ renderSnapshot snapshot

exportCmd :: WithFactory n r => Command Options
exportCmd = defCmd { cmdName = "export"
                   , cmdHandler = exportHandler
                   , cmdShortDesc = "export snapshot as manifest"
                   , cmdCategory = mainCategory
                   }

exportHandler :: WithFactory n r => App Options ()
exportHandler = do
  name <- argsExistingSnapshot

  runRepo $ do
    snapshotsDir <- asks repoSnapshotsDir
    (xml, manifest) <- readManifest_
    snapshot <- readSnapshotByName snapshotsDir name (snapshotProjects manifest)

    io $ putStrLn $ showTopElement (snapshotManifest snapshot xml)

main :: IO ()
main = withFactory lgFactory (appMainWithOptions app)

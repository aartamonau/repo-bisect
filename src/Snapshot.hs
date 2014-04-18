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

import Control.Arrow ((***), second)
import Control.Applicative ((<$>), (<|>))
import Control.Exception (finally, evaluate, catch, onException)
import Control.Monad (forM, forM_, zipWithM_, when, unless, filterM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks)
import Control.Monad.Trans (lift)

import Data.Default (Default(def))
import Data.Generics (everywhere, mkT)
import Data.List (find, sort)
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

import System.FileLock (SharedExclusive(Exclusive), tryLockFile, unlockFile)

import System.IO (stderr, hPutStrLn)

import Text.RawString.QQ (r)
import Text.Regex.Posix ((=~))

import Text.XML.Light (QName(QName),
                       Element(elName, elAttribs, elContent),
                       Content(Elem),
                       Attr(Attr, attrKey, attrVal),
                       parseXMLDoc, findElement, blank_element,
                       findChild, findChildren, findAttr,
                       showTopElement)

import Git (MonadGit(lookupCommit, lookupReference, lookupTree),
            Commit(commitParents, commitCommitter, commitTree),
            Signature(signatureWhen),
            RefTarget(RefObj, RefSymbolic), Oid, CommitOid,
            Tree,
            RepositoryFactory, IsOid,
            GitException,
            withRepository, parseOid, renderOid,
            referenceToOid, commitRefTarget)
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

import Repo (runRepo, repoRootDir, repoStateDir)

type Gitty n r = (MonadGit r n, MonadBaseControl IO n)
type RepoFactory n r = RepositoryFactory n IO r
type WithFactory n r = (Gitty n r, ?factory :: RepoFactory n r)

-- TODO: something better than this
warn :: String -> IO ()
warn = hPutStrLn stderr . ("Warning: " ++)

stateDirName :: FilePath
stateDirName = ".repo-utils"

snapshotsDir :: FilePath -> FilePath
snapshotsDir stateDir = combine stateDir "snapshots"

mustFile :: FilePath -> IO ()
mustFile path = do
  exists <- doesFileExist path
  unless exists $
    error $ "could not find " ++ path

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

type RM m a = ReaderT Project m a

withProject :: WithFactory n r => Project -> RM n a -> IO a
withProject proj k = withRepository ?factory (path proj) $ runReaderT k proj

getProjectHead :: WithFactory n r => Project -> IO (RefTarget r)
getProjectHead proj =
  withProject proj $ do
    ref <- lift $ lookupReference "HEAD"
    return $ fromMaybe (error $ "could not resolve HEAD of " ++ path proj) ref

getHeadsSnapshot :: WithFactory n r => [Project] -> IO (Snapshot r)
getHeadsSnapshot projects =
  Snapshot . zip projects <$> mapM getProjectHead projects

renderRef :: (IsOid (Oid r)) => RefTarget r -> Text
renderRef (RefObj obj) = renderOid obj
renderRef (RefSymbolic sym) = sym

onGitException :: MonadBaseControl IO m => m a -> m a -> m a
onGitException body what =
  control $ \run -> run body `catch` \ (_ :: GitException) -> run what

parseSHA :: Gitty n r => Text -> RM n (Maybe (RefTarget r))
parseSHA ref =
  (do oid <- lift $ parseOid ref
      _ <- lift $ lookupCommit (oidToCommitOid oid)
      return $ Just (RefObj oid))
  `onGitException` return Nothing

isSHA :: Gitty n r => Text -> RM n Bool
isSHA ref = isJust <$> parseSHA ref

parseRef :: Gitty n r => Text -> RM n (RefTarget r)
parseRef ref = do
  shaRef <- parseSHA ref
  finalRef <- maybe trySymName (return . Just) shaRef

  proj <- ask
  return $ fromMaybe
    (error $ "could not resolve " ++ Text.unpack ref
             ++ " at " ++ path proj) finalRef

  where trySymName =
          lift $
            (do _ <- evaluate <$> lookupReference ref
                return $ Just (RefSymbolic ref)) `onGitException` return Nothing

resolveRef :: Gitty n r => RefTarget r -> RM n (Oid r)
resolveRef ref = do
  maybeOid <- lift $ referenceToOid ref
  maybe err return maybeOid

  where err = do
          proj <- ask
          error $ "could not resolve "
            ++ Text.unpack (renderRef ref)
            ++ " in project '" ++ Text.unpack (name proj) ++ "'"

resolveSnapshot :: WithFactory n r => Snapshot r -> IO (Snapshot r)
resolveSnapshot = fmap Snapshot . mapM f . unSnapshot
  where f (p, ref) = fmap (p,) (withProject p $ RefObj <$> resolveRef ref)

refTree :: Gitty n r => RefTarget r -> RM n (Tree r)
refTree ref = do
  cid <- oidToCommitOid <$> resolveRef ref
  tid <- commitTree <$> lift (lookupCommit cid)
  lift (lookupTree tid)

headsPath :: FilePath -> FilePath
headsPath stateDir = combine stateDir "HEADS"

newtype Snapshot r = Snapshot { unSnapshot :: [(Project, RefTarget r)] }
type PartialSnapshot r = [(Project, Maybe (RefTarget r))]

renderSnapshot :: IsOid (Oid r) => Snapshot r -> Text
renderSnapshot (Snapshot ps) = Text.intercalate "\n" (map renderPair ps)
  where width = maximum $ map (Text.length . name . fst) ps
        renderPair (proj, ref) = Text.concat [name proj,
                                              Text.replicate (width - n) " ",
                                              " ",
                                              renderRef ref]
          where n = Text.length $ name proj

toFullSnapshot :: PartialSnapshot r -> Snapshot r
toFullSnapshot = Snapshot . map (second fromJust) . filter (isJust . snd)

snapshotProjects :: Snapshot r -> [Project]
snapshotProjects = fst . unzip . unSnapshot

saveSnapshotFile :: IsOid (Oid r) => FilePath -> Snapshot r -> IO ()
saveSnapshotFile path projects =
  Text.writeFile path $ Text.concat (map oneLine $ unSnapshot projects)

  where oneLine (Project {name}, ref) =
          Text.concat [renderRef ref, " ", name, "\n"]

readSnapshotFile :: WithFactory n r => FilePath -> [Project] -> IO (Snapshot r)
readSnapshotFile path projects = do
  mustFile path
  content <- Text.readFile path

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
          where p proj = name proj == needle

snapshotPath :: FilePath -> String -> FilePath
snapshotPath stateDir = combine (snapshotsDir stateDir)

readSnapshotByName :: WithFactory n r
                   => FilePath -> String -> [Project] -> IO (Snapshot r)
readSnapshotByName stateDir = readSnapshotFile . snapshotPath stateDir

saveSnapshotByName :: IsOid (Oid r) => FilePath -> String -> Snapshot r -> IO ()
saveSnapshotByName stateDir = saveSnapshotFile . snapshotPath stateDir

removeSnapshotByName :: FilePath -> String -> IO ()
removeSnapshotByName stateDir = removeFile . snapshotPath stateDir

checkoutRef :: IsOid (Oid r) => Project -> RefTarget r -> IO ()
checkoutRef (Project {path}) ref =
  shelly $ silently $ do
    cd $ fromString path
    run_ "git" ["checkout", branchify (renderRef ref)]

  where branchify s | Just branch <- Text.stripPrefix "refs/heads/" s = branch
                    | otherwise = s

checkoutSnapshot :: IsOid (Oid r) => Snapshot r -> IO ()
checkoutSnapshot = uncurry (zipWithM_ checkoutRef) . unzip . unSnapshot

tryCheckoutSnapshot :: WithFactory n r => Snapshot r -> IO ()
tryCheckoutSnapshot snapshot = do
  heads <- getHeadsSnapshot (snapshotProjects snapshot)
  checkoutSnapshot snapshot `onException` tryRecover heads

  where tryRecover heads = do
          warn "checkout failed; trying to roll back to previous state"
          checkoutSnapshot heads

oidToCommitOid :: Oid r -> CommitOid r
oidToCommitOid = Tagged

findCommit :: WithFactory n r
           => Project -> RefTarget r -> (Commit r -> Bool) -> IO (Maybe (Commit r))
findCommit proj head p =
  withProject proj $ do
    headCommitOid <- oidToCommitOid <$> resolveRef head
    go headCommitOid

  where go cid = do
          commit <- lift $ lookupCommit cid
          if p commit
            then return $ Just commit
            else
              case commitParents commit of
                [] -> return Nothing
                (first : _) -> go first

findCommitByDate :: WithFactory n r
                 => Project -> RefTarget r -> UTCTime -> IO (Maybe (Commit r))
findCommitByDate proj head date = findCommit proj head p
  where p commit = zonedTimeToUTC (signatureWhen committer) <= date
          where committer = commitCommitter commit

snapshotByDate :: WithFactory n r
               => Snapshot r -> UTCTime -> IO (PartialSnapshot r)
snapshotByDate heads date =
  forM (unSnapshot heads) $ \(p, head) -> do
    commit <- findCommitByDate p head date
    return (p, commitRefTarget <$> commit)

getSnapshots :: FilePath -> IO [String]
getSnapshots stateDir =
  filterM (doesFileExist . combine dir) =<< getDirectoryContents dir
  where dir = snapshotsDir stateDir

readManifest_ :: WithFactory n r => FilePath -> IO (Element, Snapshot r)
readManifest_ rootDir = do
  mustFile manifestPath
  doc <- parseXMLDoc <$> readFile manifestPath
  let maybeManifest = findElement (byName "manifest") =<< doc

  let manifest = fromJust maybeManifest
  let remotes = map parseRemote $ findChildren (byName "remote") manifest
  let projects = findChildren (byName "project") manifest
  let def = parseDefault remotes $ findChild (byName "default") manifest

  when (isNothing maybeManifest || null remotes || null projects)
    errorBadManifest

  snapshot <- Snapshot <$> mapM (parseProject def) projects
  return (fromJust doc, snapshot)

  where manifestPath = combine rootDir ".repo/manifest.xml"

        byName name = QName name Nothing Nothing

        errorBadManifest = error $ "invalid manifest file " ++ manifestPath

        must = fromMaybe errorBadManifest

        mustAttr attr elem = must $ findAttr attr elem

        parseRemote elem = (name, fromMaybe name alias)
          where name = mustAttr (byName "name") elem
                alias = findAttr (byName "alias") elem

        findRemote remotes name =
          fromMaybe errorBadManifest $ lookup name remotes

        parseDefault remotes = maybe (Nothing, Nothing) go
          where go elem = (remote, revision)
                  where remote = findRemote remotes <$>
                                   findAttr (byName "remote") elem
                        revision = findAttr (byName "revision") elem

        parseProject (defRemote, defRev) elem =
          withProject project $ do
            revIsSHA <- isSHA rev
            let ref | revIsSHA = rev
                    | "refs/tags/" `Text.isPrefixOf` rev = rev
                    | Just branch <- Text.stripPrefix "refs/heads" rev =
                      Text.concat ["refs/remotes/", remote, "/", branch]
                    | otherwise = Text.concat ["refs/remotes/", remote, "/", rev]
            parsedRef <- parseRef ref
            return (project, parsedRef)
          where name = mustAttr (byName "name") elem
                path = must $ findAttr (byName "path") elem <|> Just name
                absPath = combine rootDir path
                project = Project { name = Text.pack name,
                                    path = absPath }

                remote = Text.pack $ must $ findAttr (byName "remote") elem <|> defRemote
                rev = Text.pack $ must $ findAttr (byName "revision") elem <|> defRev

readManifest :: WithFactory n r => FilePath -> IO (Snapshot r)
readManifest = fmap snd . readManifest_

snapshotManifest :: IsOid (Oid r) => Snapshot r -> Element -> Element
snapshotManifest s = everywhere (mkT go)
  where projects = map (Text.unpack . name *** renderRef) (unSnapshot s)

        filterBlank x = x { elContent = content }
          where content = filter (not . isBlankElem) (elContent x)

                isBlankElem (Elem el) | QName "" _ _ <- elName el = True
                                      | otherwise = False
                isBlankElem _ = False

        go :: Element -> Element
        go x | QName "project" _ _ <- elName x = goProject $ filterBlank x
             | otherwise = filterBlank x

        goProject x | Just ref <- maybeRef = x {elAttribs = sort $ rev ref : attrs'}
                    | otherwise = blank_element
          where mkName name = QName name Nothing Nothing
                maybeName = findAttr (mkName "name") x
                maybeRef = maybeName >>= \n -> lookup n projects

                attrs' = filter p (elAttribs x)
                  where p x = mkName "revision" /= attrKey x
                rev ref = Attr { attrKey = mkName "revision"
                               , attrVal = Text.unpack ref
                               }

mustParseDate :: String -> IO UTCTime
mustParseDate date = do
  parsed <- fmap posixToUTC <$> io_approxidate date
  return $ fromMaybe (error $ "can't recognize date \"" ++ date ++ "\"") parsed

isValidSnapshotName :: String -> Bool
isValidSnapshotName name = not $ name =~ regexp
  where regexp :: String
        regexp = [r|^\.|\.\.|[\/:~^[:cntrl:][:space:]]|]

withFactory :: Gitty n r
            => RepoFactory n r
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
        stateDir <- asks repoStateDir

        liftIO $ do
          snapshots <- getSnapshots stateDir

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
  stateDir <- asks repoStateDir

  liftIO $
    mapM_ putStrLn =<< getSnapshots stateDir

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
    rootDir <- asks repoRootDir
    stateDir <- asks repoStateDir

    liftIO $ do
      manifest <- readManifest rootDir
      snapshots <- getSnapshots stateDir

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

        handleSnapshot stateDir manifest snapshot = liftIO $
              tryCheckoutSnapshot =<< readSnapshotByName stateDir snapshot projects
          where projects = snapshotProjects manifest

        handleDate manifest date = liftIO $ do
          partialSnapshot <- snapshotByDate manifest =<< mustParseDate date
          forM_ partialSnapshot $ \(Project{name}, ref) ->
            when (isNothing ref) $
              warn $ "couldn't find a commit matching the date in "
                       ++ Text.unpack name

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
        rootDir <- asks repoRootDir
        stateDir <- asks repoStateDir

        liftIO $ do
          projects <- snapshotProjects <$> readManifest rootDir
          snapshots <- getSnapshots stateDir

          unless (isValidSnapshotName name) $
            error $ "invalid snapshot name '" ++ name ++ "'"

          when (name `elem` snapshots && not (overwriteSnapshot options)) $
            error $ "snapshot '" ++ name ++ "' already exists"

          heads <- getHeadsSnapshot projects >>= \hs ->
            if resolveRefNames options
              then resolveSnapshot hs
              else return hs

          saveSnapshotByName stateDir name heads
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
    stateDir <- asks repoStateDir

    liftIO $
      removeSnapshotByName stateDir name

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
    rootDir <- asks repoRootDir
    stateDir <- asks repoStateDir

    liftIO $ do
      projects <- snapshotProjects <$> readManifest rootDir
      snapshot <- readSnapshotByName stateDir name projects
      Text.putStrLn $ renderSnapshot snapshot

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
    rootDir <- asks repoRootDir
    stateDir <- asks repoStateDir

    liftIO $ do
      (xml, manifest) <- readManifest_ rootDir

      snapshot <- readSnapshotByName stateDir name (snapshotProjects manifest)
      putStrLn $ showTopElement (snapshotManifest snapshot xml)

main :: IO ()
main = withFactory lgFactory (appMainWithOptions app)
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

import Control.Applicative ((<$>))
import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks)

import Data.Default (Default(def))
import Data.Maybe (fromMaybe, isNothing)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Data.Time (UTCTime)
import Data.Time.Git (io_approxidate, posixToUTC)

import System.IO (stderr, hPutStrLn)

import Text.RawString.QQ (r)
import Text.Regex.Posix ((=~))

import Text.XML.Light (showTopElement)

import Git.Libgit2 (lgFactory)

import System.Console.GetOpt (OptDescr(Option), ArgDescr(NoArg))
import UI.Command (Application(appName, appVersion, appAuthors, appProject,
                               appCmds, appShortDesc, appLongDesc,
                               appCategories, appBugEmail,
                               appOptions, appProcessConfig),
                   App,
                   Command(cmdName, cmdHandler, cmdShortDesc, cmdCategory),
                   appMainWithOptions, defCmd, appArgs, appConfig)

import Repo (Project(Project, projectName),
             runRepo, repoStateDir, repoSnapshotsDir,
             Project,
             WithFactory, Gitty, GitFactory,
             readManifest, readManifest_, snapshotManifest,
             getSnapshots, snapshotProjects, renderSnapshot,
             tryCheckoutSnapshot, snapshotByDate, toFullSnapshot,
             readSnapshotByName, getHeadsSnapshot, resolveSnapshot,
             saveSnapshotByName, removeSnapshotByName)
import Repo.Utils (io)

-- TODO: something better than this
warn :: String -> IO ()
warn = hPutStrLn stderr . ("Warning: " ++)

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

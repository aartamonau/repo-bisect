{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}

module Repo.Snapshot
       ( Snapshot(Snapshot, unSnapshot)
       , getHeadsSnapshot
       , resolveSnapshot
       , renderSnapshot
       , toFullSnapshot
       , snapshotProjects
       , saveSnapshotFile
       , readSnapshotFile
       , readSnapshotByName
       , saveSnapshotByName
       , removeSnapshotByName
       , tryCheckoutSnapshot
       , snapshotByDate
       , getSnapshots
       ) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Exception (onException)
import Control.Monad (forM, zipWithM_, filterM)

import Data.List (find)
import Data.Maybe (isJust, fromJust)
import Data.String (fromString)
import Data.Time (UTCTime, zonedTimeToUTC)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Git (MonadGit, RefTarget,
            Commit(commitCommitter),
            Signature(signatureWhen),
            RefTarget(RefObj), Oid, IsOid,
            commitRefTarget)

import Shelly (cd, run_, shelly, silently)
import System.Directory (doesFileExist, getDirectoryContents, removeFile)
import System.FilePath (combine)
import System.IO (stderr, hPutStrLn)

import Repo.Git (WithFactory, renderRef, parseRef, resolveRef)
import Repo.Monad (Repo)
import Repo.Project (Project(Project, projectName, projectPath),
                     findCommit, withProject, getProjectHead)
import Repo.Utils (io, mustFile)

newtype Snapshot r = Snapshot { unSnapshot :: [(Project, RefTarget r)] }
type PartialSnapshot r = [(Project, Maybe (RefTarget r))]

getHeadsSnapshot :: WithFactory n r => [Project] -> Repo (Snapshot r)
getHeadsSnapshot projects =
  Snapshot . zip projects <$> mapM getProjectHead projects

resolveSnapshot :: WithFactory n r => Snapshot r -> Repo (Snapshot r)
resolveSnapshot = fmap Snapshot . mapM f . unSnapshot
  where f (p, ref) = fmap (p,) (withProject p $ RefObj <$> resolveRef ref)

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

        -- TODO: something better than this
        warn :: String -> IO ()
        warn = hPutStrLn stderr . ("Warning: " ++)

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

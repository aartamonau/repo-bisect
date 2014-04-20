{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Repo.Manifest
       ( readManifest
       , readManifest_
       , snapshotManifest
       ) where

import Control.Applicative ((<$>), (<|>))
import Control.Arrow ((***))
import Control.Monad (when)
import Control.Monad.Reader (asks)

import Data.Generics (everywhere, mkT)
import Data.List (sort)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import qualified Data.Text as Text

import Git (IsOid, Oid)

import System.FilePath (combine)

import Text.XML.Light (QName(QName),
                       Element(elName, elAttribs, elContent),
                       Content(Elem),
                       Attr(Attr, attrKey, attrVal),
                       parseXMLDoc, findElement, blank_element,
                       findChild, findChildren, findAttr)

import Repo.Git (WithFactory, isSHA, parseRef, renderRef)
import Repo.Monad (Repo, repoRootDir)
import Repo.Project (Project(Project, projectName, projectPath), withProject)
import Repo.Snapshot (Snapshot(Snapshot, unSnapshot))
import Repo.Utils (io, mustFile)

readManifest_ :: WithFactory n r => Repo (Element, Snapshot r)
readManifest_ = doReadManifest =<< asks repoRootDir

readManifest :: WithFactory n r => Repo (Snapshot r)
readManifest = snd <$> readManifest_

doReadManifest :: WithFactory n r => FilePath -> Repo (Element, Snapshot r)
doReadManifest rootDir = do
  mustFile manifestPath
  doc <- io $ parseXMLDoc <$> readFile manifestPath
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
                project = Project { projectName = Text.pack name,
                                    projectPath = absPath }

                remote = Text.pack $ must $ findAttr (byName "remote") elem <|> defRemote
                rev = Text.pack $ must $ findAttr (byName "revision") elem <|> defRev

snapshotManifest :: IsOid (Oid r) => Snapshot r -> Element -> Element
snapshotManifest s = everywhere (mkT go)
  where projects = map (Text.unpack . projectName *** renderRef) (unSnapshot s)

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


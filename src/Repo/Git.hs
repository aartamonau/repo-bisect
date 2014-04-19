{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repo.Git
       ( Git
       , Gitty
       , GitFactory
       , WithFactory
       , onGitException
       , renderRef
       , parseSHA
       , isSHA
       , parseRef
       , resolveRef
       , refTree
       , withGitRepo
       ) where

import Control.Applicative ((<$>))
import Control.Exception (catch, evaluate)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (MonadIO, lift)
import Control.Monad.Trans.Control (MonadBaseControl, control)

import Data.Maybe (isJust, fromMaybe)
import Data.Tagged (Tagged(Tagged))

import Data.Text (Text)
import qualified Data.Text as Text

import Git (MonadGit(lookupCommit),
            RepositoryFactory, GitException, Oid, Tree, IsOid,
            RefTarget(RefObj, RefSymbolic),
            parseOid, referenceToOid, renderOid,
            commitTree, lookupTree,
            withRepository, lookupReference)

import Repo.Utils (io)

type Git m a = ReaderT FilePath m a
type Gitty n r = (MonadGit r n, MonadBaseControl IO n)
type GitFactory n r = RepositoryFactory n IO r
type WithFactory n r = (Gitty n r, ?factory :: GitFactory n r)

onGitException :: MonadBaseControl IO m => m a -> m a -> m a
onGitException body what =
  control $ \run -> run body `catch` \ (_ :: GitException) -> run what

renderRef :: (IsOid (Oid r)) => RefTarget r -> Text
renderRef (RefObj obj) = renderOid obj
renderRef (RefSymbolic sym) = sym

parseSHA :: Gitty n r => Text -> Git n (Maybe (RefTarget r))
parseSHA ref =
  (lift $ do oid <- parseOid ref
             _ <- lookupCommit (Tagged oid)
             return $ Just (RefObj oid))
  `onGitException` return Nothing

isSHA :: Gitty n r => Text -> Git n Bool
isSHA ref = isJust <$> parseSHA ref

parseRef :: Gitty n r => Text -> Git n (RefTarget r)
parseRef ref = do
  shaRef <- parseSHA ref
  finalRef <- maybe trySymName (return . Just) shaRef

  path <- ask
  return $ fromMaybe
    (error $ "could not resolve " ++ Text.unpack ref
             ++ " at " ++ path) finalRef

  where trySymName =
          lift $
            (do _ <- evaluate <$> lookupReference ref
                return $ Just (RefSymbolic ref)) `onGitException` return Nothing

resolveRef :: Gitty n r => RefTarget r -> Git n (Oid r)
resolveRef ref = do
  maybeOid <- lift $ referenceToOid ref
  maybe err return maybeOid

  where err = do
          path <- ask
          error $ "could not resolve "
            ++ Text.unpack (renderRef ref) ++ " at " ++ path

refTree :: Gitty n r => RefTarget r -> Git n (Tree r)
refTree ref = do
  cid <- Tagged <$> resolveRef ref
  lift $ do tid <- commitTree <$> lookupCommit cid
            lookupTree tid

withGitRepo :: (WithFactory n r, MonadIO m) => FilePath -> Git n a -> m a
withGitRepo path a = io $ withRepository ?factory path $ runReaderT a path
